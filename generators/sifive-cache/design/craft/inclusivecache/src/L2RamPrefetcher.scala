/*
 * L2-to-RAM Prefetcher
 *
 * This module implements prefetching between the L2 cache and outer memory (RAM).
 * It monitors outgoing Acquire requests (L2 misses) and issues prefetch hints
 * to fetch data from RAM before it's needed.
 *
 * Supports multiple prefetching algorithms:
 * - "nl": Next-line prefetcher - fetches the next cache block after each miss
 * - "strided": Strided prefetcher - detects repeated address deltas and prefetches ahead
 * - "stream": Stream prefetcher - detects sequential access patterns and prefetches ahead
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.subsystem.InclusiveCacheKey

/** Request bundle for L2-to-RAM prefetch operations */
class L2PrefetchRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val address = UInt(params.outer.bundle.addressBits.W)
}

/** IO bundle for L2-to-RAM prefetcher */
class L2RamPrefetcherIO(params: InclusiveCacheParameters) extends Bundle {
  // PRIMARY SNOOP: observe L1->L2 requests (sinkA) to detect actual CPU access patterns
  // This is the KEY input for stride detection - sees the real strided pattern!
  val l1_req_valid = Input(Bool())
  val l1_req_address = Input(UInt(params.inner.bundle.addressBits.W))
  val l1_req_opcode = Input(UInt(3.W))

  // SECONDARY SNOOP: observe L2 misses going to RAM (sourceA) to know what to prefetch
  val snoop_valid = Input(Bool())
  val snoop_address = Input(UInt(params.outer.bundle.addressBits.W))
  val snoop_opcode = Input(UInt(3.W))  // AcquireBlock or AcquirePerm

  // Snoop incoming Grant responses to know when prefetches complete
  val grant_valid = Input(Bool())
  val grant_source = Input(UInt(params.outer.bundle.sourceBits.W))

  // Prefetch request output (to be sent via channel A to RAM)
  val prefetch = Decoupled(new L2PrefetchRequest(params))

  // Backpressure signal: indicates if prefetch slots are available
  val can_prefetch = Input(Bool())
  
  // Performance Counter
  val prefetch_issued = Output(Bool())
}

/** Abstract base class for L2-to-RAM prefetchers */
abstract class L2RamPrefetcherBase(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new L2RamPrefetcherIO(params))

  // Default outputs
  io.prefetch.valid := false.B
  io.prefetch.bits := DontCare
  io.prefetch.bits.address := 0.U
  
  io.prefetch_issued := io.prefetch.fire
  
  params.ccover(io.prefetch.fire, "L2_PREFETCH_ISSUED", "L2 Prefetch Issued to RAM")
}

/** Next-Line Prefetcher: fetches the next cache block after each L2 miss */
class L2NLRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes

  val req_valid = RegInit(false.B)
  val req_addr = Reg(UInt(params.outer.bundle.addressBits.W))

  // On a new miss, compute next line address
  // NOTE: Don't gate on can_prefetch during snoop - always queue prefetch if legal
  // can_prefetch only gates actual issuance
  when(io.snoop_valid) {
    val nextAddr = io.snoop_address + blockBytes.U
    val legal = params.inValidAddressRange(nextAddr)
    when(legal) {
      req_valid := true.B
      req_addr := nextAddr
      printf("[L2 RAM NL PREFETCH] snoop addr=0x%x next=0x%x\n", io.snoop_address, nextAddr)
    }
  }.elsewhen(io.prefetch.fire) {
    req_valid := false.B
  }

  io.prefetch.valid := req_valid && io.can_prefetch
  io.prefetch.bits.address := req_addr

  when(io.prefetch.fire) {
    printf("[L2 RAM NL PREFETCH] ISSUE addr=0x%x\n", req_addr)
  }
}

/** Strided Prefetcher: detects repeated address deltas and prefetches ahead
  *
  * KEY IMPROVEMENT: This prefetcher monitors L1->L2 requests (sinkA) to detect
  * the actual CPU access patterns, rather than just L2->RAM misses.
  * This allows detection of strided patterns BEFORE L1 filtering obscures them.
  *
  * Uses a multi-entry stride table indexed by address region to track multiple
  * independent streams simultaneously. This allows detection of strided patterns
  * even when multiple arrays are accessed in interleaved fashion.
  */
class L2StridedRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes
  val blockOffBits = log2Ceil(blockBytes)

  // Configuration from InclusiveCacheKey
  val cacheParams = p(InclusiveCacheKey)
  val degree = cacheParams.prefetchDegree
  val distance = cacheParams.prefetchDistance

  // Stride table configuration - 2-Way Set Associative
  // Total entries = 32 (same as before), but organized as 16 sets * 2 ways
  val numSets = 16
  val ways = 2
  val setBits = log2Ceil(numSets) // 4
  val regionBits = 8
  // Tag covers address bits above (region + set + blockOff)
  // Address structure: [ ... Tag ... | Region (8) | Set (4) | Offset ... ]

  // Strided detection constants
  val stridedThreshold = 2  // Need 2 repeats for confidence
  val repeatWidth = 6
  val maxRepeat = (1 << repeatWidth) - 1
  val maxStrideBlocks = 32   // Support strides up to 2KB
  val deltaWidth = 16

  // =========================================================================
  // STATE REGISTERS (Associative)
  // =========================================================================
  val entry_valid   = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(ways)(false.B)))))
  val entry_tag     = Reg(Vec(numSets, Vec(ways, UInt((params.outer.bundle.addressBits - blockOffBits - regionBits - setBits).W))))
  val entry_prev    = Reg(Vec(numSets, Vec(ways, UInt((params.outer.bundle.addressBits - blockOffBits).W))))
  val entry_delta   = Reg(Vec(numSets, Vec(ways, SInt(deltaWidth.W))))
  val entry_repeat  = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(ways)(0.U(repeatWidth.W))))))
  val entry_conf    = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(ways)(0.U(4.W))))))
  val entry_hwm     = Reg(Vec(numSets, Vec(ways, UInt((params.outer.bundle.addressBits - blockOffBits).W))))
  val entry_active  = RegInit(VecInit(Seq.fill(numSets)(VecInit(Seq.fill(ways)(false.B)))))
  
  // Victim PLRU bits (1 bit per set pointing to the way to replace)
  val repl_way = RegInit(VecInit(Seq.fill(numSets)(0.U(1.W))))

  // =========================================================================
  // PREFETCH COMMAND QUEUE
  // =========================================================================
  class PrefetchCmd extends Bundle {
    val baseBlock = UInt((params.outer.bundle.addressBits - blockOffBits).W)
    val delta     = SInt(deltaWidth.W)
  }
  val perf_cmd_q = Module(new Queue(new PrefetchCmd, 4)) // Buffer 4 pending stream activations

  // =========================================================================
  // L1->L2 REQUEST SNOOPING (Primary - Stride Detection)
  // =========================================================================
  val l1_block = io.l1_req_address >> blockOffBits
  val l1_set   = l1_block(regionBits + setBits - 1, regionBits)
  val l1_tag   = l1_block >> (regionBits + setBits)

  // Associative Lookup
  val l1_hits = (0 until ways).map { w => entry_valid(l1_set)(w) && entry_tag(l1_set)(w) === l1_tag }
  val l1_hit = l1_hits.reduce(_ || _)
  val l1_hit_way = PriorityEncoder(l1_hits)
  
  // Allocation Logic (if miss)
  val l1_alloc_way = Mux(!entry_valid(l1_set)(0), 0.U,
                     Mux(!entry_valid(l1_set)(1), 1.U,
                         repl_way(l1_set))) // Victim

  val l1_update_way = Mux(l1_hit, l1_hit_way, l1_alloc_way)

  // Read State
  val l1_prev       = entry_prev(l1_set)(l1_update_way)
  val l1_stride     = entry_delta(l1_set)(l1_update_way)
  val l1_repeat     = entry_repeat(l1_set)(l1_update_way)
  val l1_conf       = entry_conf(l1_set)(l1_update_way)
  val l1_is_active  = entry_active(l1_set)(l1_update_way)

  // Delta Calc
  // Delta Calc
  val l1_rawDelta = (l1_block - l1_prev).asSInt
  val l1_paddedDelta = l1_rawDelta.pad(deltaWidth)
  val l1_deltaBlocks = Mux(l1_paddedDelta > ((1 << (deltaWidth-1)) - 1).S, 
                           ((1 << (deltaWidth-1)) - 1).S(deltaWidth.W),
                           Mux(l1_paddedDelta < (-(1 << (deltaWidth-1))).S,
                               (-(1 << (deltaWidth-1))).S(deltaWidth.W),
                               l1_paddedDelta(deltaWidth-1, 0).asSInt))

  // =========================================================================
  // PREFETCH TRIGGER LOGIC (Integrated into Snoop & Detection)
  // =========================================================================
  
  perf_cmd_q.io.enq.valid := false.B
  perf_cmd_q.io.enq.bits := DontCare

  // Helper function to trigger prefetch
  def checkAndTrigger(set: UInt, way: UInt, block: UInt, delta: SInt, conf: UInt) = {
     val active = entry_active(set)(way)
     val hwm    = entry_hwm(set)(way)
     
     // Only trigger if active and confident
     when(active && conf >= 2.U && perf_cmd_q.io.enq.ready) {
        val abs_delta = Mux(delta < 0.S, (-delta).asUInt, delta.asUInt)
        val lookahead = abs_delta * distance.U // Distance ahead of CURRENT access
        val target_block = Mux(delta > 0.S, block + lookahead, block - lookahead)
        
        // Check if we need to issue new prefetches
        // For positive stride: Target must be > HWM
        // For negative stride: Target must be < HWM
        val needed = Mux(delta > 0.S, target_block > hwm, target_block < hwm || hwm === block)
        
        when(needed) {
           perf_cmd_q.io.enq.valid := true.B
           perf_cmd_q.io.enq.bits.baseBlock := target_block
           perf_cmd_q.io.enq.bits.delta := delta
           
           // Update HWM to the end of this new burst
           val final_block = (target_block.asSInt + (degree.S * delta)).asUInt
           entry_hwm(set)(way) := final_block
           
           printf("[L2 STRIDED] QUEUE PREFETCH set=%d way=%d block=0x%x target=0x%x\n", set, way, block << blockOffBits, target_block << blockOffBits)
        }
     }
  }

  when(io.l1_req_valid) {
    when(l1_hit) {
      // Update LRU (point to other way)
      repl_way(l1_set) := ~l1_hit_way

      val stride_match = l1_deltaBlocks === l1_stride && l1_deltaBlocks =/= 0.S
      val abs_delta = Mux(l1_deltaBlocks < 0.S, (-l1_deltaBlocks).asUInt, l1_deltaBlocks.asUInt)
      val stride_ok = abs_delta <= maxStrideBlocks.U && abs_delta > 0.U

      when(stride_match && stride_ok) {
        // Confirmed Stride
        entry_repeat(l1_set)(l1_update_way) := Mux(l1_repeat === maxRepeat.U, l1_repeat, l1_repeat + 1.U)
        entry_conf(l1_set)(l1_update_way)   := Mux(l1_conf === 15.U, l1_conf, l1_conf + 1.U)
        
        when(l1_repeat >= stridedThreshold.U && !l1_is_active) {
          entry_active(l1_set)(l1_update_way) := true.B
          entry_hwm(l1_set)(l1_update_way) := l1_block // Initialize HWM to current
          printf("[L2 STRIDED] STREAM DETECTED set=%d way=%d stride=%d\n", l1_set, l1_update_way, l1_deltaBlocks)
        }
        
        // TRIGGER PREFETCH ON HIT (Continuous Prefetching)
        // ensure we use the REGISTERED state for consistency or updated?
        // Use current values
        checkAndTrigger(l1_set, l1_update_way, l1_block, l1_deltaBlocks, entry_conf(l1_set)(l1_update_way))

      }.otherwise {
        // Stride Changed
        entry_delta(l1_set)(l1_update_way) := l1_deltaBlocks
        entry_repeat(l1_set)(l1_update_way) := 1.U
        when(l1_conf > 0.U) { entry_conf(l1_set)(l1_update_way) := l1_conf - 1.U }
        when(l1_conf === 0.U) { entry_active(l1_set)(l1_update_way) := false.B }
      }
      entry_prev(l1_set)(l1_update_way) := l1_block
      
    }.otherwise {
      // New Entry ( Allocate )
      val w = l1_alloc_way
      repl_way(l1_set) := ~w
      
      entry_valid(l1_set)(w) := true.B
      entry_tag(l1_set)(w)   := l1_tag
      entry_prev(l1_set)(w)  := l1_block
      entry_delta(l1_set)(w) := 0.S
      entry_repeat(l1_set)(w):= 0.U
      entry_conf(l1_set)(w)  := 0.U
      entry_active(l1_set)(w):= false.B
      entry_hwm(l1_set)(w)   := l1_block
    }
  }

  // =========================================================================
  // L2 MISS SNOOPING (Secondary - Trigger Prefetch)
  // =========================================================================
  val miss_block = io.snoop_address >> blockOffBits
  val miss_set   = miss_block(regionBits + setBits - 1, regionBits)
  val miss_tag   = miss_block >> (regionBits + setBits)

  val miss_hits  = (0 until ways).map { w => entry_valid(miss_set)(w) && entry_tag(miss_set)(w) === miss_tag }
  val miss_hit   = miss_hits.reduce(_ || _)
  val miss_way   = PriorityEncoder(miss_hits)
  
  // Also trigger on Misses (in case L1 requests were filtered or initial cold miss)
  when(io.snoop_valid && miss_hit) {
     checkAndTrigger(miss_set, miss_way, miss_block, entry_delta(miss_set)(miss_way), entry_conf(miss_set)(miss_way))
  }

  // =========================================================================
  // PREFETCH ISSUER (Consumes Queue)
  // =========================================================================
  val cmd_valid = perf_cmd_q.io.deq.valid
  val cmd_bits  = perf_cmd_q.io.deq.bits
  
  val issue_cnt = RegInit(0.U(log2Ceil(degree + 1).W))
  val issue_active = RegInit(false.B)
  val issue_addr   = Reg(UInt((params.outer.bundle.addressBits - blockOffBits).W))
  val issue_delta  = Reg(SInt(deltaWidth.W))

  // Queue Dequeue Logic
  perf_cmd_q.io.deq.ready := !issue_active // Ready to pop if not currently issuing a burst

  when(cmd_valid && !issue_active) {
    issue_active := true.B
    issue_cnt    := 0.U
    issue_addr   := cmd_bits.baseBlock
    issue_delta  := cmd_bits.delta
  }

  // Output to RAM
  val prefetch_full_addr = issue_addr << blockOffBits
  val legal = params.inValidAddressRange(prefetch_full_addr)
  
  io.prefetch.valid := issue_active && legal && io.can_prefetch
  io.prefetch.bits.address := prefetch_full_addr

  when(issue_active) {
    val fire = io.prefetch.ready && io.can_prefetch

    when(!legal) {
      // Skip illegal address - consumes a slot count but doesn't issue
       issue_addr := (issue_addr.asSInt + issue_delta).asUInt
       issue_cnt  := issue_cnt + 1.U
       when(issue_cnt + 1.U >= degree.U) {
          issue_active := false.B
       }
    } .elsewhen(fire) {
       // Issued successfully
       printf("[L2 STRIDED PREFETCH] ISSUE addr=0x%x\n", prefetch_full_addr)
       issue_addr := (issue_addr.asSInt + issue_delta).asUInt
       issue_cnt  := issue_cnt + 1.U
       when(issue_cnt + 1.U >= degree.U) {
          issue_active := false.B
       }
    }
  }
}

/** AMPM (Access Map Pattern Matching) Prefetcher */
class L2AMPMRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes
  val blockOffBits = log2Ceil(blockBytes)
  val addressBits = params.inner.bundle.addressBits

  // Configuration
  val zoneSize = 4096  // 4KB zones (1 page)
  val blocksPerZone = zoneSize / blockBytes
  val numZones = 16    // Number of tracked zones
  
  // Access Map Entry
  class AccessMapEntry extends Bundle {
    val valid     = Bool()
    val zoneTag   = UInt((addressBits - log2Ceil(zoneSize)).W)
    val accessMap = UInt(blocksPerZone.W)  // Bitmap of accessed blocks
  }
  
  // Access Map Table (fully associative, LRU replacement)
  val accessMapTable = RegInit(VecInit(Seq.fill(numZones)(0.U.asTypeOf(new AccessMapEntry))))
  val lruCounter = RegInit(VecInit(Seq.fill(numZones)(0.U(log2Ceil(numZones).W))))
  
  // Prefetch queue
  val pf_queue = Module(new Queue(UInt(params.outer.bundle.addressBits.W), 8))
  
  // Address decomposition
  val current_addr = io.l1_req_address
  val zoneTag = current_addr >> log2Ceil(zoneSize)
  val blockOffsetBits = log2Ceil(blocksPerZone)
  val blockOffset = ((current_addr >> blockOffBits) & ((1 << blockOffsetBits) - 1).U)(blockOffsetBits-1, 0)
  
  // Find matching zone or allocate new one
  val zoneHits = VecInit(accessMapTable.map(e => e.valid && e.zoneTag === zoneTag))
  val zoneHitIdx = PriorityEncoder(zoneHits)
  val zoneHit = zoneHits.reduce(_ || _)
  
  // Find LRU entry for replacement
  val lruIdx = PriorityEncoder(VecInit(lruCounter.map(_ === 0.U)))
  
  // Default: no enqueue
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  when (io.l1_req_valid) {
    when (zoneHit) {
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
      
      // Pattern Matching
      val prevBlockAccessed = (oldMap >> (blockOffset - 1.U)) & 1.U
      val forwardPattern = blockOffset > 0.U && prevBlockAccessed === 1.U
      
      val nextBlockAccessed = (oldMap >> (blockOffset + 1.U)) & 1.U
      val backwardPattern = blockOffset < (blocksPerZone - 1).U && nextBlockAccessed === 1.U
      
      when (forwardPattern && pf_queue.io.enq.ready) {
        val pfOffset = blockOffset + 1.U
        when (pfOffset < blocksPerZone.U && ((oldMap >> pfOffset) & 1.U) === 0.U) {
          val pfAddr = Cat(zoneTag, pfOffset) << blockOffBits
          pf_queue.io.enq.valid := true.B
          pf_queue.io.enq.bits := pfAddr
          printf("[L2 AMPM PF] Forward prefetch: zone=0x%x offset=%d addr=0x%x\n", 
                 zoneTag, pfOffset, pfAddr)
        }
      } .elsewhen (backwardPattern && pf_queue.io.enq.ready) {
        val pfOffset = blockOffset - 1.U
        when (pfOffset > 0.U && ((oldMap >> pfOffset) & 1.U) === 0.U) {
          val pfAddr = Cat(zoneTag, pfOffset) << blockOffBits
          pf_queue.io.enq.valid := true.B
          pf_queue.io.enq.bits := pfAddr
          printf("[L2 AMPM PF] Backward prefetch: zone=0x%x offset=%d addr=0x%x\n", 
                 zoneTag, pfOffset, pfAddr)
        }
      }
      
    } .otherwise {
      // Zone miss - allocate new entry
      printf("[L2 AMPM PF] New zone: tag=0x%x offset=%d\n", zoneTag, blockOffset)
      accessMapTable(lruIdx).valid := true.B
      accessMapTable(lruIdx).zoneTag := zoneTag
      accessMapTable(lruIdx).accessMap := 1.U << blockOffset
      lruCounter(lruIdx) := (numZones - 1).U
    }
  }
  
  // Dequeue prefetch requests to output
  pf_queue.io.deq.ready := io.prefetch.ready && io.can_prefetch
  
  io.prefetch.valid := pf_queue.io.deq.valid && io.can_prefetch
  io.prefetch.bits.address := pf_queue.io.deq.bits
  
  when (io.prefetch.fire) {
    printf("[L2 AMPM PF] Prefetch issued: addr=0x%x\n", io.prefetch.bits.address)
  }
}

/** BOP (Best Offset Prefetcher) */
class L2BOPRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes
  val blockOffBits = log2Ceil(blockBytes)
  val addressBits = params.inner.bundle.addressBits

  // Candidate offsets to test (in cache blocks)
  val candidateOffsets = Seq(1, 2, 3, 4, 6, 8, 10, 12, 16, -1, -2, -3, -4)
  val numOffsets = candidateOffsets.length
  
  // Recent Requests Table (RR Table)
  val rrTableSize = 64
  val rrTable = Reg(Vec(rrTableSize, UInt((addressBits - blockOffBits).W)))
  val rrHead = RegInit(0.U(log2Ceil(rrTableSize).W))
  
  // Score table
  val scoreTable = RegInit(VecInit(Seq.fill(numOffsets)(0.U(8.W))))
  
  // Best offset and current test state
  val bestOffset = RegInit(1.S(8.W))
  val testIdx = RegInit(0.U(log2Ceil(numOffsets).W))
  val testRound = RegInit(0.U(8.W))
  val roundLength = 256.U
  
  // Prefetch queue
  val pf_queue = Module(new Queue(UInt(params.outer.bundle.addressBits.W), 4))
  
  // Current address in block units
  val current_addr = io.l1_req_address
  val currentBlock = current_addr >> blockOffBits
  
  // Default: no enqueue
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  when (io.l1_req_valid) {
      // Add current block to RR table
      rrTable(rrHead) := currentBlock
      rrHead := Mux(rrHead === (rrTableSize - 1).U, 0.U, rrHead + 1.U)
      
      // Test current candidate offset
      val candidateOffsetsVec = VecInit(candidateOffsets.map(_.S(8.W)))
      val testOffset = candidateOffsetsVec(testIdx)
      val testBlock = (currentBlock.asSInt - testOffset).asUInt
      
      // Search RR table for testBlock
      val rrHits = VecInit(rrTable.map(_ === testBlock))
      when (rrHits.reduce(_ || _)) {
        when (scoreTable(testIdx) < 255.U) {
          scoreTable(testIdx) := scoreTable(testIdx) + 1.U
        }
      }
      
      // Move to next offset candidate
      testIdx := Mux(testIdx === (numOffsets - 1).U, 0.U, testIdx + 1.U)
      
      // Check if round is complete
      testRound := testRound + 1.U
      when (testRound === roundLength) {
        testRound := 0.U
        
        // Find best offset
        val maxScore = scoreTable.reduce((a, b) => Mux(a > b, a, b))
        val bestIdx = scoreTable.indexWhere(_ === maxScore)
        
        val candidateOffsetsVec = VecInit(candidateOffsets.map(_.S(8.W)))
        bestOffset := candidateOffsetsVec(bestIdx)
        
        printf("[L2 BOP] Round complete: best_offset=%d score=%d\n", 
               bestOffset, maxScore)
        
        // Reset scores
        for (i <- 0 until numOffsets) {
          scoreTable(i) := 0.U
        }
      }
      
      // Issue prefetch with best offset
      val pfBlock = (currentBlock.asSInt + bestOffset).asUInt
      val pfAddr = pfBlock << blockOffBits
      
      when (pf_queue.io.enq.ready) {
        pf_queue.io.enq.valid := true.B
        pf_queue.io.enq.bits := pfAddr
        printf("[L2 BOP] Prefetch: addr=0x%x offset=%d\n", pfAddr, bestOffset)
      }
  }
  
  pf_queue.io.deq.ready := io.prefetch.ready && io.can_prefetch
  
  io.prefetch.valid := pf_queue.io.deq.valid && io.can_prefetch
  io.prefetch.bits.address := pf_queue.io.deq.bits
  
  when (io.prefetch.fire) {
    printf("[L2 BOP] Prefetch issued: addr=0x%x\n", io.prefetch.bits.address)
  }
}

/** SPP (Signature Path Prefetching) */
class L2SPPRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes
  val blockOffBits = log2Ceil(blockBytes)
  val addressBits = params.inner.bundle.addressBits

  // Configuration
  val signatureBits = 12
  val stTableSize = 256
  val ptTableSize = 512
  val deltaHistoryLen = 4
  
  // Signature Table Entry
  class STEntry extends Bundle {
    val valid     = Bool()
    val pageTag   = UInt((addressBits - 12).W)
    val lastBlock = UInt(6.W)
    val signature = UInt(signatureBits.W)
  }
  
  // Pattern Table Entry
  class PTEntry extends Bundle {
    val valid      = Bool()
    val delta      = SInt(7.W)
    val confidence = UInt(3.W)
  }
  
  // Tables
  val stTable = RegInit(VecInit(Seq.fill(stTableSize)(0.U.asTypeOf(new STEntry))))
  val ptTable = RegInit(VecInit(Seq.fill(ptTableSize)(0.U.asTypeOf(new PTEntry))))
  
  // Queue
  val pf_queue = Module(new Queue(UInt(params.outer.bundle.addressBits.W), 8))
  
  // Lookahead State
  val lookahead_active = RegInit(false.B)
  val lookahead_sig    = Reg(UInt(signatureBits.W))
  val lookahead_block  = Reg(UInt(6.W))
  val lookahead_tag    = Reg(UInt((addressBits - 12).W))
  val lookahead_depth  = Reg(UInt(3.W))
  
  val max_depth = 4.U
  
  // Address decomposition
  val current_addr = io.l1_req_address
  val pageTag = current_addr >> 12
  val blockOffset = (current_addr >> blockOffBits) & 63.U
  
  // Hash functions
  def stHash(tag: UInt): UInt = tag(log2Ceil(stTableSize)-1, 0)
  def ptHash(sig: UInt): UInt = sig(log2Ceil(ptTableSize)-1, 0)
  
  // Compute new signature
  def updateSignature(oldSig: UInt, delta: SInt): UInt = {
    val shifted = oldSig << 3
    val newSig = shifted ^ delta.asUInt
    newSig(signatureBits-1, 0)
  }
  
  // Default: no enqueue
  pf_queue.io.enq.valid := false.B
  pf_queue.io.enq.bits := DontCare
  
  when (reset.asBool) {
      lookahead_active := false.B
  }

  when (io.l1_req_valid) {
      val stIdx = stHash(pageTag)
      val stEntry = stTable(stIdx)
      
      // Interrupt lookahead on demand
      lookahead_active := false.B

      when (stEntry.valid && stEntry.pageTag === pageTag) {
        // ST hit
        // Zero-extend to avoid negative interpretation of upper half blocks
        val blockOffset_s = (0.U(1.W) ## blockOffset).asSInt
        val lastBlock_s   = (0.U(1.W) ## stEntry.lastBlock).asSInt
        val delta = blockOffset_s - lastBlock_s
        
        val oldSig = stEntry.signature
        val newSig = updateSignature(oldSig, delta)
        
        // Update ST entry
        stTable(stIdx).lastBlock := blockOffset
        stTable(stIdx).signature := newSig
        
        // Update Pattern Table
        val ptIdx = ptHash(oldSig)
        val ptEntry = ptTable(ptIdx)
        
        when (ptEntry.valid && ptEntry.delta === delta) {
          when (ptEntry.confidence < 7.U) {
            ptTable(ptIdx).confidence := ptEntry.confidence + 1.U
          }
        } .otherwise {
          // Mismatch
          val currentConf = Mux(ptEntry.valid, ptEntry.confidence, 0.U)
          
          when (currentConf === 0.U) {
             ptTable(ptIdx).valid := true.B
             ptTable(ptIdx).delta := delta
             ptTable(ptIdx).confidence := 1.U
          } .otherwise {
             ptTable(ptIdx).confidence := currentConf - 1.U
          }
        }
        
        // PREFETCH Generation (Trigger)
        val predPtIdx = ptHash(newSig)
        val predEntry = ptTable(predPtIdx)
        
        when (predEntry.valid && predEntry.confidence > 1.U) {
          val prefetchBlockS = blockOffset_s + predEntry.delta
          
          when (prefetchBlockS >= 0.S && prefetchBlockS < 64.S && pf_queue.io.enq.ready) {
            val prefetchBlock = prefetchBlockS.asUInt(5, 0)
            val pfAddr = Cat(pageTag, prefetchBlock) << blockOffBits
            
            pf_queue.io.enq.valid := true.B
            pf_queue.io.enq.bits := pfAddr
            printf("[L2 SPP] Prefetch (Trigger): sig=0x%x delta=%d addr=0x%x conf=%d\n", 
                   newSig, predEntry.delta, pfAddr, predEntry.confidence)
                   
            // INITIATE LOOKAHEAD
            lookahead_active := true.B
            lookahead_sig    := updateSignature(newSig, predEntry.delta)
            lookahead_block  := prefetchBlock
            lookahead_tag    := pageTag
            lookahead_depth  := 1.U
          }
        }
        
      } .otherwise {
        // ST miss
        stTable(stIdx).valid := true.B
        stTable(stIdx).pageTag := pageTag
        stTable(stIdx).lastBlock := blockOffset
        stTable(stIdx).signature := 0.U
        printf("[L2 SPP] New page: tag=0x%x block=%d\n", pageTag, blockOffset)
      }
  } .elsewhen(lookahead_active) {
      // LOOKAHEAD LOOP
      val pf_pt_idx = ptHash(lookahead_sig)
      val pf_entry  = ptTable(pf_pt_idx)
      
      val confident = pf_entry.valid && pf_entry.confidence >= 2.U
      
      when (confident && pf_queue.io.enq.ready && lookahead_depth < max_depth) {
         val next_delta = pf_entry.delta
         val next_block_s = lookahead_block.asSInt + next_delta
         
         when (next_block_s >= 0.S && next_block_s < 64.S) {
             // Enqueue
             val next_block = next_block_s.asUInt(5, 0)
             val pfAddr = Cat(lookahead_tag, next_block) << blockOffBits
             
             pf_queue.io.enq.valid := true.B
             pf_queue.io.enq.bits := pfAddr
             
             printf("[L2 SPP] Prefetch (Lookahead %d): addr=0x%x sig=0x%x delta=%d conf=%d\n", 
                    lookahead_depth, pfAddr, lookahead_sig, next_delta, pf_entry.confidence)
             
             // Advance
             lookahead_sig    := updateSignature(lookahead_sig, next_delta)
             lookahead_block  := next_block
             lookahead_depth  := lookahead_depth + 1.U
         } .otherwise {
             lookahead_active := false.B
         }
      } .otherwise {
         when (!confident || lookahead_depth >= max_depth) {
            lookahead_active := false.B
         }
      }
  }
  
  pf_queue.io.deq.ready := io.prefetch.ready && io.can_prefetch
  
  io.prefetch.valid := pf_queue.io.deq.valid && io.can_prefetch
  io.prefetch.bits.address := pf_queue.io.deq.bits
  
  when (io.prefetch.fire) {
    printf("[L2 SPP] Prefetch issued: addr=0x%x\n", io.prefetch.bits.address)
  }
}

/** Null Prefetcher: does nothing (used when prefetching is disabled) */
class L2NullRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  // All outputs already set to defaults in base class
}

/** Multi-Prefetcher: combines multiple prefetchers */
class L2MultiRamPrefetcher(params: InclusiveCacheParameters, types: Seq[String])(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val prefetchers = types.map {
    case "nl" => Module(new L2NLRamPrefetcher(params))
    case "strided" => Module(new L2StridedRamPrefetcher(params))
    case "ampm" => Module(new L2AMPMRamPrefetcher(params))
    case "bop" => Module(new L2BOPRamPrefetcher(params))
    case "spp" => Module(new L2SPPRamPrefetcher(params))
    case _ => Module(new L2NullRamPrefetcher(params))
  }

  // Hook up inputs
  prefetchers.foreach { pf =>
    pf.io.l1_req_valid := io.l1_req_valid
    pf.io.l1_req_address := io.l1_req_address
    pf.io.l1_req_opcode := io.l1_req_opcode
    pf.io.snoop_valid := io.snoop_valid
    pf.io.snoop_address := io.snoop_address
    pf.io.snoop_opcode := io.snoop_opcode
    pf.io.grant_valid := io.grant_valid
    pf.io.grant_source := io.grant_source
    pf.io.can_prefetch := io.can_prefetch
  }

  // Arbitrate outputs (Round-Robin)
  val arb = Module(new RRArbiter(new L2PrefetchRequest(params), prefetchers.length))
  arb.io.in <> prefetchers.map(_.io.prefetch)
  io.prefetch <> arb.io.out
  
  io.prefetch_issued := prefetchers.map(_.io.prefetch_issued).reduce(_ || _)
}

/** Factory object for creating L2 RAM prefetchers based on configuration */
object L2RamPrefetcher {
  def apply(params: InclusiveCacheParameters)(implicit p: Parameters): L2RamPrefetcherBase = {
    val cacheParams = p(InclusiveCacheKey)
    if (!cacheParams.enablePrefetch || cacheParams.prefetchTypes.isEmpty) {
      Module(new L2NullRamPrefetcher(params))
    } else {
      Module(new L2MultiRamPrefetcher(params, cacheParams.prefetchTypes))
    }
  }
}
