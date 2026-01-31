//******************************************************************************
// See LICENSE.Berkeley for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.lsu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.common._
import boom.exu.BrResolutionInfo
import boom.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask}



abstract class DataPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val mshr_avail = Input(Bool())
    val req_val    = Input(Bool())
    val req_addr   = Input(UInt(coreMaxAddrBits.W))
    val req_coh    = Input(new ClientMetadata)

    val prefetch   = Decoupled(new BoomDCacheReq)
  })
}

/**
  * Does not prefetch
  */
class NullPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{
  io.prefetch.valid := false.B
  io.prefetch.bits  := DontCare
}

/**
  * Next line prefetcher. Grabs the next line on a cache miss
  * Prefetch depth = 2 (issue next prefetch as soon as previous is accepted)
  */
class NLPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{
  val prefetch_depth = 2 // Number of lines to prefetch ahead
  val prefetch_queue = Reg(Vec(prefetch_depth, Valid(new Bundle {
    val addr = UInt(coreMaxAddrBits.W)
    val cmd  = UInt(M_SZ.W)
  })))
  
  // Tagged prefetching: track last miss address to filter random accesses
  val last_miss_addr = Reg(UInt(coreMaxAddrBits.W))
  val last_miss_valid = RegInit(false.B)
  
  for (i <- 0 until prefetch_depth) {
    when (reset.asBool) {
      prefetch_queue(i).valid := false.B
    }
  }

  // On demand miss: only prefetch if sequential pattern detected
  val mshr_req_addr = io.req_addr + cacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)
  
  // Tagged prefetching: check if current miss is sequential to last miss
  val is_sequential = last_miss_valid && (io.req_addr === last_miss_addr + cacheBlockBytes.U)
  
  when (io.req_val) {
    last_miss_addr := io.req_addr
    last_miss_valid := true.B
  }
  
  when (io.req_val && cacheable && is_sequential) {
    // Enqueue prefetches for next N lines (only on sequential pattern)
    for (i <- 0 until prefetch_depth) {
      val pf_addr = io.req_addr + ((i + 1) * cacheBlockBytes).U
      val pf_cacheable = edge.manager.supportsAcquireBSafe(pf_addr, lgCacheBlockBytes.U)
      when (pf_cacheable && !prefetch_queue(i).valid) {
        printf("[NL PF] Queue[%d] addr=0x%x (tagged)\n", i.U, pf_addr)
        prefetch_queue(i).valid := true.B
        prefetch_queue(i).bits.addr := pf_addr
        prefetch_queue(i).bits.cmd := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
      }
    }
  }
  
  // Issue from queue head
  io.prefetch.valid := prefetch_queue(0).valid
  io.prefetch.bits := DontCare
  io.prefetch.bits.addr := prefetch_queue(0).bits.addr
  io.prefetch.bits.uop := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := prefetch_queue(0).bits.cmd
  io.prefetch.bits.data := DontCare
  
  // Shift queue on issue
  when (io.prefetch.fire) {
    printf("[NL PF] Issued addr=0x%x\n", prefetch_queue(0).bits.addr)
    for (i <- 0 until prefetch_depth - 1) {
      prefetch_queue(i) := prefetch_queue(i + 1)
    }
    prefetch_queue(prefetch_depth - 1).valid := false.B
  }
}

/**
  * Strided Prefetcher
  * 
  * Detects stride patterns from consecutive miss addresses.
  * Prefetches multiple lines ahead when a stable stride is detected.
  */
class StridePrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
  with HasBoomCoreParameters
{
  val queueSize = 4
  val pf_queue = Module(new Queue(UInt(coreMaxAddrBits.W), queueSize))
  val pf_cmd   = Reg(UInt(M_SZ.W))

  // Configuration
  val nEntries = 16
  val match_tol = 4
  val dist = boomParams.prefetchDistance
  val degree = boomParams.prefetchDegree // Use configured degree

  // Allow strides up to 64KB (covers 1024 bytes easily)
  val maxStrideBytes = 65536 // 64KB

  class StrideEntry extends Bundle {
    val valid     = Bool()
    val last_addr = UInt(coreMaxAddrBits.W)
    val stride    = SInt(coreMaxAddrBits.W)
    val conf      = UInt(2.W)
    val hwm       = UInt(coreMaxAddrBits.W) // High Water Mark
  }

  val table = RegInit(VecInit(Seq.fill(nEntries)(0.U.asTypeOf(new StrideEntry))))
  val victim_ptr = RegInit(0.U(log2Ceil(nEntries).W))

  // Issue Queue Control
  val issue_active = RegInit(false.B)
  val issue_count  = Reg(UInt(log2Ceil(degree + 1).W))
  val issue_base   = Reg(UInt(coreMaxAddrBits.W))
  val issue_stride = Reg(SInt(coreMaxAddrBits.W))
  val issue_dist   = Reg(UInt(8.W)) // Store dist to allow runtime config if needed
  val issue_hwm    = Reg(UInt(coreMaxAddrBits.W)) // Store HWM of active stream
  val issue_idx    = Reg(UInt(log2Ceil(nEntries).W)) // Store index of active stream to update HWM

  // Information about the current request
  val req_addr = io.req_addr
  val req_cmd  = Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)

  // Default output
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits  := DontCare
  
  // ---------------------------------------------
  // 1. Snoop / Train Logic
  // ---------------------------------------------
  when (io.req_val) {
    pf_cmd := req_cmd
    val s_addr = req_addr.asSInt
    
    // Search for matching entry
    val hits = VecInit(table.map(e => e.valid && (e.last_addr.asSInt + e.stride).asUInt === req_addr))
    val hit = hits.reduce(_ || _)
    val hit_idx = PriorityEncoder(hits)

    // Search for allocating/training entry
    val trains = VecInit(table.map{ e => 
      val diff = s_addr - e.last_addr.asSInt
      e.valid && diff =/= 0.S && diff.abs < maxStrideBytes.S
    })
    val train = trains.reduce(_ || _)
    val train_idx = PriorityEncoder(trains)

    // Replacement logic
    val has_free = table.map(!_.valid).reduce(_ || _)
    val free_idx = PriorityEncoder(table.map(!_.valid)) 
    val replace_idx = Mux(has_free, free_idx, victim_ptr)

    when (hit) {
       // Hit logic
       val entry = table(hit_idx)
       table(hit_idx).last_addr := req_addr
       when (entry.conf < 3.U) {
         table(hit_idx).conf := entry.conf + 1.U
       }
       
       // Trigger Issuance if confident and NOT already issuing
       // (Dropping new triggers is preferable to interrupting an active burst)
       when (entry.conf >= 2.U && !issue_active) {
         issue_active := true.B
         issue_count  := 0.U
         issue_base   := req_addr
         issue_stride := entry.stride
         issue_dist   := dist.U // Use current config dist
         issue_hwm    := entry.hwm
         issue_idx    := hit_idx
       }

    } .elsewhen (train) {
       // Train logic
       val diff = s_addr - table(train_idx).last_addr.asSInt
       table(train_idx).last_addr := req_addr
       table(train_idx).stride := diff
       
       val same_stride = table(train_idx).stride === diff
       
       table(train_idx).conf := Mux(same_stride, 
                                    Mux(table(train_idx).conf < 3.U, table(train_idx).conf + 1.U, 3.U), 
                                    1.U)
       // Reset HWM on retrain
       when (!same_stride) {
          table(train_idx).hwm := 0.U
       }
       // If confident transition, initialize HWM to current to avoid prefetching behind
       when (same_stride && table(train_idx).conf === 1.U) {
          table(train_idx).hwm := req_addr
       }
       
       printf("[STRIDE PF] Train: addr=0x%x diff=%d conf=%d\n", req_addr, diff, table(train_idx).conf)
       
    } .otherwise {
       // Allocate
       table(replace_idx).valid := true.B
       table(replace_idx).last_addr := req_addr
       table(replace_idx).stride := 0.S
       table(replace_idx).conf := 0.U
       table(replace_idx).hwm  := req_addr // Initialize HWM
       
       when (!has_free) {
         victim_ptr := Mux(victim_ptr === (nEntries-1).U, 0.U, victim_ptr + 1.U)
       }
       
       printf("[STRIDE PF] Alloc: addr=0x%x idx=%d\n", req_addr, replace_idx)
    }
  }

  // ---------------------------------------------
  // 2. Issue Logic (Burst Generator)
  // ---------------------------------------------
  when (issue_active) {
    // Calculate target address: base + stride * (dist + count)
    // We do one multiplication per cycle to be safe, or just add stride accumulator
    // Using accumulator is simpler: base_next = base + stride
    
    // For the FIRST item (count=0), target = base + stride*dist
    // For subsequent (count=k), target = prev_target + stride
    
    // To minimize critical path, let's just calculate fresh each time or use accumulator
    // We already have issue_base, let's treat it as the "current prefetch base"
    
    // On the cycle we trigger (above), issue_base is set to req_addr
    // So for the first issue cycle:
    val pf_offset = issue_stride * (issue_dist + issue_count).asSInt
    val next_addr = (issue_base.asSInt + pf_offset).asUInt
    
    // Safety check: Don't cross 4KB page boundary blindly? (Optional optimization)
    // HWM Check: Only issue if target is beyond HWM (in the direction of stride)
    val hwm_ok = Mux(issue_stride > 0.S, next_addr > issue_hwm, next_addr < issue_hwm)
    // Also handle HWM uninitialized (0) or wrap cases carefully. 
    // Assuming simple for now. If HWM=0 and stride<0, might issue bad. 
    // But allocation sets HWM=addr, so usually safe.
    
    // We always increment count/finish burst logic, but only ENQUEUE if hwm_ok
    issue_count := issue_count + 1.U
    when (issue_count + 1.U === degree.U) {
      issue_active := false.B
    }

    when (pf_queue.io.enq.ready && hwm_ok) {
      pf_queue.io.enq.valid := true.B
      pf_queue.io.enq.bits  := next_addr
      
      // Update HWM in table (write-through to register file)
      // This ensures next trigger sees new HWM
      table(issue_idx).hwm := next_addr
      // Update local HWM too incase we use it for next item in burst? 
      // Actually strictly we compare against original HWM or rolling?
      // Comparing against original 'issue_hwm' is fine if we issue strictly increasing.
      // But standard says compare against 'max verified'.
      issue_hwm := next_addr
      
      printf("[STRIDE PF] BURST: addr=0x%x stride=%d pf=0x%x (HWM update)\n", 
             issue_base, issue_stride, next_addr)
             
    } .otherwise {
       // If filtered, we just skip enqueue but still consume issue slot
       printf("[STRIDE PF] FILTERED: pf=0x%x <= hwm=0x%x\n", next_addr, issue_hwm)
    }
  }

  // Dequeue
  pf_queue.io.deq.ready := io.prefetch.ready
  io.prefetch.valid := pf_queue.io.deq.valid
  io.prefetch.bits := DontCare
  io.prefetch.bits.is_hella := false.B
  io.prefetch.bits.addr := pf_queue.io.deq.bits
  io.prefetch.bits.uop := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := pf_cmd
  io.prefetch.bits.data := DontCare
}

/**
  * AMPM (Access Map Pattern Matching) Prefetcher
  * 
  * Tracks access patterns within memory zones using bitmaps.
  * Detects and prefetches based on sequential and stride patterns.
  * 
  * Based on: "Access Map Pattern Matching for Data Cache Prefetch"
  * Ishii et al., ICS 2009
  */
class AMPMPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
  with HasBoomCoreParameters
{
  // Configuration
  val zoneSize = 4096  // 4KB zones (1 page)
  val blocksPerZone = zoneSize / cacheBlockBytes  // Typically 64 blocks per zone
  val numZones = 16    // Number of tracked zones
  
  // Access Map Entry
  class AccessMapEntry extends Bundle {
    val valid     = Bool()
    val zoneTag   = UInt((coreMaxAddrBits - log2Ceil(zoneSize)).W)
    val accessMap = UInt(blocksPerZone.W)  // Bitmap of accessed blocks
  }
  
  // Access Map Table (fully associative, LRU replacement)
  val accessMapTable = RegInit(VecInit(Seq.fill(numZones)(0.U.asTypeOf(new AccessMapEntry))))
  val lruCounter = RegInit(VecInit(Seq.fill(numZones)(0.U(log2Ceil(numZones).W))))
  
  // Prefetch queue
  val pf_queue = Module(new Queue(UInt(coreMaxAddrBits.W), 8))
  val pf_cmd   = Reg(UInt(M_SZ.W))
  
  // Address decomposition
  val current_addr = io.req_addr
  val zoneTag = current_addr >> log2Ceil(zoneSize)
  // Constrain blockOffset to log2(blocksPerZone) bits to avoid FIRRTL shift width errors
  val blockOffsetBits = log2Ceil(blocksPerZone)
  val blockOffset = ((current_addr >> log2Ceil(cacheBlockBytes)) & ((1 << blockOffsetBits) - 1).U)(blockOffsetBits-1, 0)
  
  // Find matching zone or allocate new one
  val zoneHits = VecInit(accessMapTable.map(e => e.valid && e.zoneTag === zoneTag))
  val zoneHitIdx = PriorityEncoder(zoneHits)
  val zoneHit = zoneHits.reduce(_ || _)
  
  // Find LRU entry for replacement
  val lruIdx = PriorityEncoder(VecInit(lruCounter.map(_ === 0.U)))
  
  // Pattern detection offsets to check
  val checkOffsets = Seq(-2, -1, 1, 2)  // Look for patterns at offsets ±1, ±2
  
  // Default: no enqueue (must be BEFORE conditional so they can override)
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  when (io.req_val) {
    val cacheable = edge.manager.supportsAcquireBSafe(current_addr, lgCacheBlockBytes.U)
    pf_cmd := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
    
    when (zoneHit && cacheable) {
      val entry = accessMapTable(zoneHitIdx)
      val oldMap = entry.accessMap
      
      // Update access map
      accessMapTable(zoneHitIdx).accessMap := oldMap | (1.U << blockOffset)
      
      // Update LRU
      for (i <- 0 until numZones) {
        when (i.U === zoneHitIdx) {
          lruCounter(i) := (numZones - 1).U
        } .elsewhen (lruCounter(i) > 0.U) {
          lruCounter(i) := lruCounter(i) - 1.U
        }
      }
      
      // Pattern Matching: Check if we see a pattern in the access map
      // Look for consecutive accesses that suggest a direction
      val prefetchDegree = boomParams.prefetchDegree
      
      // Simple pattern: If block N-1 was accessed and we just accessed N, prefetch N+1
      val prevBlockAccessed = (oldMap >> (blockOffset - 1.U)) & 1.U
      val forwardPattern = blockOffset > 0.U && prevBlockAccessed === 1.U
      
      // Also check N+1 -> N (backward pattern) -> prefetch N-1
      val nextBlockAccessed = (oldMap >> (blockOffset + 1.U)) & 1.U
      val backwardPattern = blockOffset < (blocksPerZone - 1).U && nextBlockAccessed === 1.U
      
      // Dynamic degree: count consecutive accessed blocks to determine pattern strength
      val prev2Accessed = blockOffset > 1.U && ((oldMap >> (blockOffset - 2.U)) & 1.U) === 1.U
      val strongPattern = forwardPattern && prev2Accessed  // 3+ consecutive = strong
      
      when (forwardPattern && pf_queue.io.enq.ready) {
        // Prefetch forward: N+2 normally, N+3 for strong patterns
        val pfOffset = Mux(strongPattern, blockOffset + 3.U, blockOffset + 2.U)
        when (pfOffset < blocksPerZone.U && ((oldMap >> pfOffset) & 1.U) === 0.U) {
          val pfAddr = Cat(zoneTag, pfOffset) << log2Ceil(cacheBlockBytes)
          pf_queue.io.enq.valid := true.B
          pf_queue.io.enq.bits := pfAddr
          printf("[AMPM PF] Forward prefetch (N+%d): zone=0x%x offset=%d addr=0x%x strong=%d\n", 
                 Mux(strongPattern, 3.U, 2.U), zoneTag, pfOffset, pfAddr, strongPattern)
        }
      } .elsewhen (backwardPattern && pf_queue.io.enq.ready) {
        // Prefetch backward with lookahead
        val pfOffset = blockOffset - 2.U
        when (pfOffset > 0.U && ((oldMap >> pfOffset) & 1.U) === 0.U) {
          val pfAddr = Cat(zoneTag, pfOffset) << log2Ceil(cacheBlockBytes)
          pf_queue.io.enq.valid := true.B
          pf_queue.io.enq.bits := pfAddr
          printf("[AMPM PF] Backward prefetch (N-2): zone=0x%x offset=%d addr=0x%x\n", 
                 zoneTag, pfOffset, pfAddr)
        }
      }
      
    } .elsewhen (cacheable) {
      // Zone miss - allocate new entry
      printf("[AMPM PF] New zone: tag=0x%x offset=%d\n", zoneTag, blockOffset)
      accessMapTable(lruIdx).valid := true.B
      accessMapTable(lruIdx).zoneTag := zoneTag
      accessMapTable(lruIdx).accessMap := 1.U << blockOffset
      lruCounter(lruIdx) := (numZones - 1).U
    }
  }
  
  // Dequeue prefetch requests to output
  pf_queue.io.deq.ready := io.prefetch.ready
  
  io.prefetch.valid := pf_queue.io.deq.valid
  io.prefetch.bits := DontCare
  io.prefetch.bits.addr := pf_queue.io.deq.bits
  io.prefetch.bits.uop := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := pf_cmd
  io.prefetch.bits.data := DontCare
  
  when (io.prefetch.fire) {
    printf("[AMPM PF] Prefetch issued: addr=0x%x\n", io.prefetch.bits.addr)
  }
}

/**
  * BOP (Best Offset Prefetcher)
  * 
  * Self-tuning prefetcher that finds the best offset to prefetch.
  * Periodically tests different offsets and selects the one with highest accuracy.
  * 
  * Based on: "Best-Offset Hardware Prefetching" - Michaud, HPCA 2016
  */
class BOPPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
  with HasBoomCoreParameters
{
  // Candidate offsets to test (in cache blocks)
  val candidateOffsets = Seq(1, 2, 3, 4, 6, 8, 10, 12, 16, -1, -2, -3, -4)
  val numOffsets = candidateOffsets.length
  
  // Recent Requests Table (RR Table) - tracks recent miss addresses
  val rrTableSize = 64
  val rrTable = Reg(Vec(rrTableSize, UInt((coreMaxAddrBits - log2Ceil(cacheBlockBytes)).W)))
  val rrHead = RegInit(0.U(log2Ceil(rrTableSize).W))
  
  // Score table - tracks accuracy of each offset
  val scoreTable = RegInit(VecInit(Seq.fill(numOffsets)(0.U(8.W))))
  
  // Best offset and current test state
  val bestOffset = RegInit(1.S(8.W))  // Default: +1 offset
  val bestScore  = RegInit(0.U(8.W))  // Confidence in bestOffset
  val testIdx    = RegInit(0.U(log2Ceil(numOffsets).W))
  val testRound  = RegInit(0.U(8.W))
  val roundLength = 256.U  // Rounds before selecting new best offset
  
  // Bad score pruning: track tests per offset in current round
  val testsPerOffset = RegInit(VecInit(Seq.fill(numOffsets)(0.U(6.W))))
  val pruneThreshold = 32.U  // Skip offset if 0 score after this many tests
  
  // Prefetch queue
  val pf_queue = Module(new Queue(UInt(coreMaxAddrBits.W), 4))
  val pf_cmd   = Reg(UInt(M_SZ.W))
  
  // Current address in block units
  val current_addr = io.req_addr
  val currentBlock = current_addr >> log2Ceil(cacheBlockBytes)
  
  // Default: no enqueue (must be BEFORE conditional so they can override)
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  when (io.req_val) {
    val cacheable = edge.manager.supportsAcquireBSafe(current_addr, lgCacheBlockBytes.U)
    pf_cmd := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
    
    when (cacheable) {
      // Add current block to RR table
      rrTable(rrHead) := currentBlock
      rrHead := Mux(rrHead === (rrTableSize - 1).U, 0.U, rrHead + 1.U)
      
      // Test current candidate offset: check if (current - offset) is in RR table
      // Create Vec for hardware indexing
      val candidateOffsetsVec = VecInit(candidateOffsets.map(_.S(8.W)))
      val testOffset = candidateOffsetsVec(testIdx)
      val testBlock = (currentBlock.asSInt - testOffset).asUInt
      
      // Search RR table for testBlock
      val rrHits = VecInit(rrTable.map(_ === testBlock))
      when (rrHits.reduce(_ || _)) {
        // Hit! Increment score for this offset
        when (scoreTable(testIdx) < 255.U) {
          scoreTable(testIdx) := scoreTable(testIdx) + 1.U
        }
      }
      
      // Track tests for this offset
      testsPerOffset(testIdx) := testsPerOffset(testIdx) + 1.U
      
      // Move to next offset candidate (with bad score pruning)
      // Skip offsets that have 0 score after pruneThreshold tests
      val nextIdxRaw = Mux(testIdx === (numOffsets - 1).U, 0.U, testIdx + 1.U)
      val nextOffsetBad = testsPerOffset(nextIdxRaw) >= pruneThreshold && scoreTable(nextIdxRaw) === 0.U
      val nextIdx = Mux(nextOffsetBad, 
                        Mux(nextIdxRaw === (numOffsets - 1).U, 0.U, nextIdxRaw + 1.U),
                        nextIdxRaw)
      testIdx := nextIdx
      
      // Check if round is complete
      testRound := testRound + 1.U
      when (testRound === roundLength) {
        testRound := 0.U
        
         // Find best offset (highest score)
         val maxScore = scoreTable.reduce((a, b) => Mux(a > b, a, b))
         val bestIdx = scoreTable.indexWhere(_ === maxScore)
        
         // Create Vec for hardware indexing
         val candidateOffsetsVec = VecInit(candidateOffsets.map(_.S(8.W)))
         bestOffset := candidateOffsetsVec(bestIdx)
         bestScore  := maxScore
        
         printf("[BOP] Round complete: best_offset=%d score=%d\n", 
           bestOffset, maxScore)
        
        // Reset scores and test counts for next round
        for (i <- 0 until numOffsets) {
          scoreTable(i) := 0.U
          testsPerOffset(i) := 0.U
        }
      }
      
      // Issue prefetch with best offset, scaled by confidence (bestScore)
      // Always issue (no gating), but use larger multiplier at higher confidence
      val lowConf  = bestScore < 4.U
      val medConf  = bestScore >= 4.U && bestScore < 16.U
      val highConf = bestScore >= 16.U

      // Effective offset in blocks: just use bestOffset (no multiplier)
      // This is safer for L1 cache to avoid pollution
      val effectiveOffset = bestOffset
      val pfBlockS = currentBlock.asSInt + effectiveOffset
      val pfBlock  = pfBlockS.asUInt
      val pfAddr   = pfBlock << log2Ceil(cacheBlockBytes)
      
      when (pf_queue.io.enq.ready && pfBlockS >= 0.S) {
        pf_queue.io.enq.valid := true.B
        pf_queue.io.enq.bits  := pfAddr
        printf("[BOP] Prefetch: addr=0x%x base_off=%d eff_off=%d score=%d\n", 
               pfAddr, bestOffset, effectiveOffset, bestScore)
      }
    }
  }
  
  pf_queue.io.deq.ready := io.prefetch.ready
  
  io.prefetch.valid := pf_queue.io.deq.valid
  io.prefetch.bits := DontCare
  io.prefetch.bits.addr := pf_queue.io.deq.bits
  io.prefetch.bits.uop := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := pf_cmd
  io.prefetch.bits.data := DontCare
  
  when (io.prefetch.fire) {
    printf("[BOP] Prefetch issued: addr=0x%x\n", io.prefetch.bits.addr)
  }
}

/**
  * SPP (Signature Path Prefetching)
  * 
  * Uses delta signatures to predict access patterns.
  * Maintains a signature table and pattern table for prediction.
  */
class SPPPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
  with HasBoomCoreParameters
{
  // Constants and params
  val stEntries = 64  // Signature Table Size
  val ptEntries = 256 // Pattern Table Size
  val sigBits = 12    // Signature Width
  
  val pageSize = 4096
  val pageOffsetBits = log2Ceil(pageSize)
  
  // Tables
  class STEntry extends Bundle {
    val tag = UInt((coreMaxAddrBits - pageOffsetBits).W)
    val sig = UInt(sigBits.W)
    val last_offset = UInt(pageOffsetBits.W)
    val valid = Bool()
  }
  
  class PTEntry extends Bundle {
    val delta = SInt(pageOffsetBits.W)
    val conf  = UInt(2.W) // 2-bit saturating counter
  }
  
  // ST is set-associative or hash-indexed. For simplicity, direct-mapped hash
  val st = Reg(Vec(stEntries, new STEntry)) 
  val pt = Reg(Vec(ptEntries, new PTEntry))

  // Queue
  val pf_queue = Module(new Queue(UInt(coreMaxAddrBits.W), 4))
  val pf_cmd   = Reg(UInt(M_SZ.W))

  // Lookahead State
  val lookahead_active = RegInit(false.B)
  val lookahead_sig    = Reg(UInt(sigBits.W))
  val lookahead_offset = Reg(UInt(pageOffsetBits.W))
  val lookahead_tag    = Reg(UInt((coreMaxAddrBits - pageOffsetBits).W))
  val lookahead_depth  = Reg(UInt(3.W))
  
  val max_depth = 4.U

  // Logic
  val req_addr = io.req_addr
  val req_page = req_addr >> pageOffsetBits
  val req_offset = req_addr(pageOffsetBits-1, 0)
  
  val st_idx = req_page(log2Ceil(stEntries)-1, 0)
  
  // Helpers
  def stHash(tag: UInt): UInt = tag(log2Ceil(stEntries)-1, 0)
  def ptHash(sig: UInt): UInt = sig(log2Ceil(ptEntries)-1, 0)
  def updateSignature(oldSig: UInt, delta: SInt): UInt = {
    val newSig = ((oldSig << 3) ^ delta.asUInt)(sigBits-1,0)
    newSig
  }

  // Defaults
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  // Initialization
  when (reset.asBool) {
    for (i <- 0 until stEntries) {
      st(i).valid := false.B
    }
    for (i <- 0 until ptEntries) {
      pt(i).conf := 0.U
      pt(i).delta := 0.S
    }
    lookahead_active := false.B
  }

  when (io.req_val) {
    pf_cmd := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
    
    val st_hit = st(st_idx).valid && st(st_idx).tag === req_page
    
    // By default, interrupt lookahead on new request to retrain/re-initiate
    lookahead_active := false.B

    when (st_hit) {
      // Calculate delta
      // Zero-extend offsets to prevent interpretation as negative numbers
      val old_offset_s = (0.U(1.W) ## st(st_idx).last_offset).asSInt
      val req_offset_s = (0.U(1.W) ## req_offset).asSInt
      val delta = req_offset_s - old_offset_s
      
      // Update Signature: sig = (sig << 3) ^ delta
      val old_sig = st(st_idx).sig
      val new_sig = updateSignature(old_sig, delta)
      
      // Update ST
      st(st_idx).sig := new_sig
      st(st_idx).last_offset := req_offset
      
      // Update PT (Train) based on OLD signature
      val pt_idx = ptHash(old_sig)
      val learned_delta = pt(pt_idx).delta
      val learned_conf  = pt(pt_idx).conf
      
      when (learned_delta === delta) {
        when (learned_conf < 3.U) { pt(pt_idx).conf := learned_conf + 1.U }
      } .otherwise {
        pt(pt_idx).conf := Mux(learned_conf === 0.U, 0.U, learned_conf - 1.U)
        when (learned_conf === 0.U) {
          pt(pt_idx).delta := delta
          pt(pt_idx).conf := 1.U
        }
      }
      
      // PREFETCH TRIGGER (Start of Lookahead)
      // Look up PT with new_sig
      val pf_pt_idx = ptHash(new_sig)
      val pf_delta  = pt(pf_pt_idx).delta
      val pf_conf   = pt(pf_pt_idx).conf
      
      when (pf_conf >= 1.U && pf_queue.io.enq.ready) { 
        // We have some confidence
        val pf_offset_s = req_offset_s + pf_delta
        
        // Check page boundaries (don't cross page in SPP usually, or check valid)
        when (pf_offset_s >= 0.S && pf_offset_s < pageSize.S) {
           val pf_addr = Cat(req_page, pf_offset_s.asUInt(pageOffsetBits-1,0))
           pf_queue.io.enq.valid := true.B
           pf_queue.io.enq.bits := pf_addr
           
           printf("[SPP] Prefetch (Trigger): addr=0x%x sig=0x%x delta=%d conf=%d\n", pf_addr, new_sig, pf_delta, pf_conf)
           
           // INITIATE LOOKAHEAD
           lookahead_active := true.B
           // Updates for NEXT cycle's prediction:
           // Speculate signature: new_sig updated with pf_delta
           lookahead_sig    := updateSignature(new_sig, pf_delta)
           lookahead_offset := pf_offset_s.asUInt
           lookahead_tag    := req_page
           lookahead_depth  := 1.U
        }
      }

    } .otherwise {
      // New Page, Allocation
      st(st_idx).valid := true.B
      st(st_idx).tag := req_page
      st(st_idx).last_offset := req_offset
      st(st_idx).sig := 0.U
    }
  } .elsewhen(lookahead_active) {
     // LOOKAHEAD LOOP
     // We are in a bubble (no new CPU request), follow the path
     
     val pf_pt_idx = ptHash(lookahead_sig)
     val pf_delta  = pt(pf_pt_idx).delta
     val pf_conf   = pt(pf_pt_idx).conf
     
     // Continue if confident and queue ready
     // Require slightly higher confidence for deep lookahead? (Canonical suggests yes, let's say >= 2 for depth>1, or kept simple >=1)
     val confident = pf_conf >= 1.U
     
     when (confident && pf_queue.io.enq.ready && lookahead_depth < max_depth) {
        // Zero-extend before SInt conversion
        val la_offset_s = (0.U(1.W) ## lookahead_offset).asSInt
        val pf_offset_s = la_offset_s + pf_delta
        
        when (pf_offset_s >= 0.S && pf_offset_s < pageSize.S) {
           val pf_addr = Cat(lookahead_tag, pf_offset_s.asUInt(pageOffsetBits-1,0))
           pf_queue.io.enq.valid := true.B
           pf_queue.io.enq.bits := pf_addr
           
           printf("[SPP] Prefetch (Lookahead %d): addr=0x%x sig=0x%x delta=%d conf=%d\n", 
                  lookahead_depth, pf_addr, lookahead_sig, pf_delta, pf_conf)
           
           // Advance Lookahead
           lookahead_sig    := updateSignature(lookahead_sig, pf_delta)
           lookahead_offset := pf_offset_s.asUInt
           lookahead_depth  := lookahead_depth + 1.U
        } .otherwise {
           // Page boundary reached
           lookahead_active := false.B
        }
     } .otherwise {
        // Lost confidence or depth limit or Queue full (stall or drop?)
        // If queue full, we simply stall (don't clear active) -> implies retry next cycle
        // If not confident or depth reached, stop.
        when (!confident || lookahead_depth >= max_depth) {
           lookahead_active := false.B
        }
     }
  }
  
  // Dequeue logic
  pf_queue.io.deq.ready := io.prefetch.ready
  io.prefetch.valid := pf_queue.io.deq.valid
  io.prefetch.bits := DontCare
  io.prefetch.bits.is_hella := false.B
  io.prefetch.bits.addr := pf_queue.io.deq.bits
  io.prefetch.bits.uop := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := pf_cmd
  io.prefetch.bits.data := DontCare
}

class MultiDataPrefetcher(val types: Seq[String])(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher {
  val prefetchers = types.map {
    case "nl" => Module(new NLPrefetcher)
    case "strided" => Module(new StridePrefetcher)
    case "ampm" => Module(new AMPMPrefetcher)
    case "bop" => Module(new BOPPrefetcher)
    case "spp" => Module(new SPPPrefetcher)
    case _ => Module(new NullPrefetcher)
  }

  // Hook inputs
  prefetchers.foreach { pf =>
    pf.io.mshr_avail := io.mshr_avail
    pf.io.req_val := io.req_val
    pf.io.req_addr := io.req_addr
    pf.io.req_coh := io.req_coh
  }

  // Arbitrate outputs (Round-Robin)
  val arb = Module(new RRArbiter(new BoomDCacheReq, prefetchers.length))
  arb.io.in <> prefetchers.map(_.io.prefetch)
  io.prefetch <> arb.io.out
}
