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
}

/** Abstract base class for L2-to-RAM prefetchers */
abstract class L2RamPrefetcherBase(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new L2RamPrefetcherIO(params))

  // Default outputs
  io.prefetch.valid := false.B
  io.prefetch.bits := DontCare
  io.prefetch.bits.address := 0.U
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

  // Stride table configuration - sized for good coverage
  val numEntries = 32
  val indexBits = log2Ceil(numEntries)
  val regionBits = 8

  // Strided detection constants
  val stridedThreshold = 2  // Need 2 repeats for confidence
  val repeatWidth = 6
  val maxRepeat = (1 << repeatWidth) - 1
  val maxStrideBlocks = 32   // Support strides up to 2KB (32 * 64B)
  val maxDistanceBlocks = 256
  val deltaWidth = 16

  // =========================================================================
  // STRIDE TABLE: Tracks patterns from L1->L2 requests (the actual CPU pattern!)
  // =========================================================================
  val entry_valid = RegInit(VecInit(Seq.fill(numEntries)(false.B)))
  val entry_tag = Reg(Vec(numEntries, UInt((params.outer.bundle.addressBits - blockOffBits - regionBits).W)))
  val entry_prev_block = Reg(Vec(numEntries, UInt((params.outer.bundle.addressBits - blockOffBits).W)))
  val entry_delta = Reg(Vec(numEntries, SInt(deltaWidth.W)))
  val entry_repeat_cnt = RegInit(VecInit(Seq.fill(numEntries)(0.U(repeatWidth.W))))
  val entry_confidence = RegInit(VecInit(Seq.fill(numEntries)(0.U(4.W))))  // 4-bit confidence
  val entry_hwm = Reg(Vec(numEntries, UInt((params.outer.bundle.addressBits - blockOffBits).W)))
  val entry_active = RegInit(VecInit(Seq.fill(numEntries)(false.B)))  // Per-entry activation

  // Prefetch queue (multiple outstanding prefetch targets)
  val prefetch_valid = RegInit(false.B)
  val prefetch_addr = Reg(UInt(params.outer.bundle.addressBits.W))
  val prefetch_delta = Reg(SInt(deltaWidth.W))
  val prefetch_entry = Reg(UInt(indexBits.W))
  val prefetch_issued = RegInit(0.U(log2Ceil(degree + 1).W))

  // =========================================================================
  // L1->L2 REQUEST SNOOPING (PRIMARY - for stride detection)
  // =========================================================================
  val l1_block = io.l1_req_address >> blockOffBits
  val l1_index = l1_block(regionBits + indexBits - 1, regionBits)
  val l1_tag = l1_block >> (regionBits + indexBits)

  val l1_hit = entry_valid(l1_index) && (entry_tag(l1_index) === l1_tag)
  val l1_prev = entry_prev_block(l1_index)
  val l1_stride = entry_delta(l1_index)

  // Compute delta from L1 requests (the REAL strided pattern!)
  val l1_rawDelta = (l1_block.asSInt - l1_prev.asSInt)
  val l1_deltaBlocks = Mux(l1_rawDelta > ((1 << (deltaWidth-1)) - 1).S, 
                           ((1 << (deltaWidth-1)) - 1).S(deltaWidth.W),
                           Mux(l1_rawDelta < (-(1 << (deltaWidth-1))).S,
                               (-(1 << (deltaWidth-1))).S(deltaWidth.W),
                               l1_rawDelta(deltaWidth-1, 0).asSInt))

  when(io.l1_req_valid) {
    when(l1_hit) {
      val stride_match = l1_deltaBlocks === l1_stride && l1_deltaBlocks =/= 0.S
      val cur_repeat = entry_repeat_cnt(l1_index)
      val cur_conf = entry_confidence(l1_index)
      val abs_delta = Mux(l1_deltaBlocks < 0.S, (-l1_deltaBlocks).asUInt, l1_deltaBlocks.asUInt)
      val stride_ok = abs_delta <= maxStrideBlocks.U && abs_delta > 0.U

      when(stride_match && stride_ok) {
        // Matching stride - increase confidence
        entry_repeat_cnt(l1_index) := Mux(cur_repeat === maxRepeat.U, cur_repeat, cur_repeat + 1.U)
        entry_confidence(l1_index) := Mux(cur_conf === 15.U, cur_conf, cur_conf + 1.U)

        // Activate stream when confident
        when(cur_repeat >= stridedThreshold.U && !entry_active(l1_index)) {
          entry_active(l1_index) := true.B
          entry_hwm(l1_index) := l1_block
          printf("[L2 STRIDED] STREAM DETECTED idx=%d stride=%d blocks (%d bytes)\n",
                 l1_index, l1_deltaBlocks, abs_delta << blockOffBits)
        }

      }.otherwise {
        // Stride changed - update delta, decay confidence
        entry_delta(l1_index) := l1_deltaBlocks
        entry_repeat_cnt(l1_index) := 0.U
        when(cur_conf > 0.U) {
          entry_confidence(l1_index) := cur_conf - 1.U
        }
        when(cur_conf === 0.U) {
          entry_active(l1_index) := false.B
        }
      }
      entry_prev_block(l1_index) := l1_block

    }.otherwise {
      // New entry
      entry_valid(l1_index) := true.B
      entry_tag(l1_index) := l1_tag
      entry_prev_block(l1_index) := l1_block
      entry_delta(l1_index) := 0.S
      entry_repeat_cnt(l1_index) := 0.U
      entry_confidence(l1_index) := 0.U
      entry_active(l1_index) := false.B
      entry_hwm(l1_index) := l1_block
    }
  }

  // =========================================================================
  // L2 MISS SNOOPING (SECONDARY - triggers prefetch for active streams)
  // =========================================================================
  val miss_block = io.snoop_address >> blockOffBits
  val miss_index = miss_block(regionBits + indexBits - 1, regionBits)
  val miss_tag = miss_block >> (regionBits + indexBits)

  val miss_hit = entry_valid(miss_index) && (entry_tag(miss_index) === miss_tag)
  val stream_active = miss_hit && entry_active(miss_index)
  val stream_delta = entry_delta(miss_index)
  val stream_conf = entry_confidence(miss_index)

  when(io.snoop_valid && stream_active && stream_conf >= 2.U) {
    // L2 miss on an active stream - trigger prefetching!
    val abs_delta = Mux(stream_delta < 0.S, (-stream_delta).asUInt, stream_delta.asUInt)
    val lookahead = abs_delta * distance.U
    val target_block = Mux(stream_delta > 0.S,
                           miss_block + lookahead,
                           miss_block - lookahead)
    val current_hwm = entry_hwm(miss_index)

    // Check if we should prefetch (beyond what we've already prefetched)
    val should_prefetch = Mux(stream_delta > 0.S,
                              target_block > current_hwm,
                              target_block < current_hwm || current_hwm === miss_block)

    when(should_prefetch && !prefetch_valid) {
      prefetch_valid := true.B
      prefetch_addr := target_block << blockOffBits
      prefetch_delta := stream_delta
      prefetch_entry := miss_index
      prefetch_issued := 0.U
      printf("[L2 STRIDED] PREFETCH TRIGGER miss=0x%x target=0x%x stride=%d hwm=0x%x\n",
             io.snoop_address, target_block << blockOffBits, stream_delta, current_hwm << blockOffBits)
    }
  }

  // =========================================================================
  // PREFETCH ISSUANCE
  // =========================================================================
  val pref_block = prefetch_addr >> blockOffBits
  val legal = params.inValidAddressRange(prefetch_addr)
  val can_issue = prefetch_valid && legal && (prefetch_issued < degree.U) && io.can_prefetch

  io.prefetch.valid := can_issue
  io.prefetch.bits.address := prefetch_addr

  when(io.prefetch.fire) {
    val abs_delta = Mux(prefetch_delta < 0.S, (-prefetch_delta).asUInt, prefetch_delta.asUInt)
    printf("[L2 STRIDED PREFETCH] addr=0x%x stride=%d issued=%d/%d\n",
           prefetch_addr, prefetch_delta, prefetch_issued + 1.U, degree.U)

    // Update HWM
    entry_hwm(prefetch_entry) := pref_block

    // Advance to next prefetch address
    prefetch_addr := (prefetch_addr.asSInt + (prefetch_delta << blockOffBits)).asUInt
    prefetch_issued := prefetch_issued + 1.U

    // Check if we've issued enough
    when(prefetch_issued + 1.U >= degree.U) {
      prefetch_valid := false.B
    }
  }

  // Timeout: if prefetch_valid but can't issue for too long, give up
  val timeout_cnt = RegInit(0.U(8.W))
  when(prefetch_valid && !io.prefetch.fire) {
    timeout_cnt := timeout_cnt + 1.U
    when(timeout_cnt === 255.U) {
      prefetch_valid := false.B
      timeout_cnt := 0.U
    }
  }.otherwise {
    timeout_cnt := 0.U
  }
}

/** Stream Prefetcher: detects sequential access patterns and prefetches ahead
  *
  * This prefetcher detects when consecutive L2 misses access sequential cache blocks
  * and then prefetches multiple blocks ahead in the same direction.
  */
class L2StreamRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  val blockBytes = params.cache.blockBytes
  val blockOffBits = log2Ceil(blockBytes)

  // Configuration
  val cacheParams = p(InclusiveCacheKey)
  val degree = cacheParams.prefetchDegree

  // State registers
  val prev_valid = RegInit(false.B)
  val prev_addr = Reg(UInt(params.outer.bundle.addressBits.W))
  val consec_cnt = RegInit(0.U(8.W))

  val active = RegInit(false.B)
  val next_pref = Reg(UInt(params.outer.bundle.addressBits.W))
  val issued = RegInit(0.U(log2Ceil(degree + 1).W))

  // Detect sequential access (next block)
  val sequential = io.snoop_address === prev_addr + blockBytes.U

  when(io.snoop_valid) {
    when(prev_valid && sequential) {
      consec_cnt := consec_cnt + 1.U
      printf("[L2 STREAM RAM] sequential access, cnt=%d\n", consec_cnt + 1.U)
    }.otherwise {
      consec_cnt := 0.U
    }
    prev_addr := io.snoop_address
    prev_valid := true.B

    // Activate after seeing 2 consecutive sequential accesses
    // NOTE: Don't gate activation on can_prefetch - we want to detect patterns
    // even when we can't issue prefetches right now. can_prefetch only gates issuance.
    when(consec_cnt >= 1.U && !active) {
      active := true.B
      next_pref := io.snoop_address + blockBytes.U
      issued := 0.U
      printf("[L2 STREAM RAM] ACTIVATE next_pref=0x%x\n", io.snoop_address + blockBytes.U)
    }
  }

  // Issue prefetches when active
  val legal = params.inValidAddressRange(next_pref)
  when(active && (issued >= degree.U)) {
    active := false.B
    printf("[L2 STREAM RAM] DEACTIVATE degree exhausted\n")
  }

  when(active && !legal) {
    active := false.B
    printf("[L2 STREAM RAM] DEACTIVATE illegal address\n")
  }

  io.prefetch.valid := active && legal && (issued < degree.U) && io.can_prefetch
  io.prefetch.bits.address := next_pref

  when(io.prefetch.fire) {
    printf("[L2 STREAM RAM PREFETCH] ISSUE addr=0x%x issued=%d\n", next_pref, issued + 1.U)
    next_pref := next_pref + blockBytes.U
    issued := issued + 1.U
  }
}

/** Null Prefetcher: does nothing (used when prefetching is disabled) */
class L2NullRamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends L2RamPrefetcherBase(params) {
  // All outputs already set to defaults in base class
}

/** Factory object for creating L2 RAM prefetchers based on configuration */
object L2RamPrefetcher {
  def apply(params: InclusiveCacheParameters)(implicit p: Parameters): L2RamPrefetcherBase = {
    val cacheParams = p(InclusiveCacheKey)
    if (!cacheParams.enablePrefetch) {
      Module(new L2NullRamPrefetcher(params))
    } else {
      cacheParams.prefetchType match {
        case "nl" => Module(new L2NLRamPrefetcher(params))
        case "strided" => Module(new L2StridedRamPrefetcher(params))
        case "stream" => Module(new L2StreamRamPrefetcher(params))
        case _ => Module(new L2NullRamPrefetcher(params))
      }
    }
  }
}
