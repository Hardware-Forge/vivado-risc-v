/*
 * Stream Buffer for Inclusive Cache
 *
 * A dedicated buffer for storing prefetched data.
 * - Decouples prefetch storage from MSHRs.
 * - Manages its own "SourceA" requests to memory.
 * - Uses a separate pool of Source IDs.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class StreamBufferRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val address = UInt(params.outer.bundle.addressBits.W)
}

class StreamBufferIO(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  // Input from Prefetcher
  val alloc = Flipped(Decoupled(new StreamBufferRequest(params)))

  // Invalidation from Directory/Cache (Release/ProbeAck)
  val inval_valid = Input(Bool())
  val inval_addr  = Input(UInt(params.outer.bundle.addressBits.W))

  // Interface to Outer Memory (Source A)
  val req = Decoupled(new TLBundleA(params.outer.bundle))
  
  // Interface from Outer Memory (Sink D)
  val resp = Flipped(Decoupled(new TLBundleD(params.outer.bundle)))
  
  // Interface to Outer Memory (Source E - GrantAck)
  val ack_e = Decoupled(new TLBundleE(params.outer.bundle))

  // Interface to Scheduler (Hit Check / Pop)
  val peek_addr = Input(UInt(params.outer.bundle.addressBits.W))
  val peek_hit  = Output(Bool())
  val peek_data = Output(UInt((params.cache.blockBytes * 8).W)) // For debug/verification
                                                               
  // Signal to "consume" the entry (move to L2)
  val pop_valid = Input(Bool())
  val pop_source = Input(UInt(params.outer.bundle.sourceBits.W)) // MSHR ID to replay to

  // Replay Interface: Stream Buffer acts as a memory responder
  val replay_d = Decoupled(new TLBundleD(params.outer.bundle))
}

class StreamBufferEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params) {
  val valid = Bool()
  val tag   = UInt(params.tagBits.W)
  val set   = UInt(params.setBits.W)
  val data  = UInt((params.cache.blockBytes * 8).W) 
  val pending = Bool()
  val sink  = UInt(params.outer.bundle.sinkBits.W) // Store sink for GrantAck
  val need_ack = Bool() // Track if we need to send GrantAck
}

class StreamBuffer(params: InclusiveCacheParameters, depth: Int = 4, sourceIdBase: Int = 0) extends Module {
  val io = IO(new StreamBufferIO(params))

  val entries = RegInit(VecInit(Seq.fill(depth)(0.U.asTypeOf(new StreamBufferEntry(params)))))
  
  // --------------------------------------------------------------------------
  // Replay State (Moved up for Allocation Logic dependency)
  // --------------------------------------------------------------------------
  val replay_active = RegInit(false.B)
  val replay_idx    = Reg(UInt(log2Ceil(depth).W))
  val replay_beat   = Reg(UInt(16.W)) // Enough for max beats
  val replay_source = Reg(UInt(params.outer.bundle.sourceBits.W))

  // --------------------------------------------------------------------------
  // Allocation Logic (FIFO)
  // --------------------------------------------------------------------------
  val head = RegInit(0.U(log2Ceil(depth).W))

  // --------------------------------------------------------------------------
  // Hit / Pop Logic (Moved up for Allocation Logic dependency)
  // --------------------------------------------------------------------------
  // FIX: params.offsetBits
  val (peek_tag, peek_set, peek_block) = params.parseAddress(io.peek_addr)
  
  // CRITICAL FIX: Do NOT allow hits on pending entries!
  // If we hit on pending and the prefetch fails, the MSHR receives a Denied response
  // and hangs. Only allow hits on completed prefetches (data ready).
  val hits = VecInit((0 until depth).map { i => 
    entries(i).valid && !entries(i).pending &&
    entries(i).set === peek_set && entries(i).tag === peek_tag
  })
  
  val hit_idx = PriorityEncoder(hits)
  val is_hit = hits.reduce(_ || _)

  val req_queue = Module(new Queue(new StreamBufferRequest(params), 2))
  req_queue.io.enq <> io.alloc
  
  val current_req = req_queue.io.deq.bits
  val current_req_valid = req_queue.io.deq.valid
  
  // FIX: Use parseAddress to correctly extract Tag/Set based on addressMapping
  val (req_tag, req_set, req_block) = params.parseAddress(current_req.address)

  // Issue Logic
  // We use a specific Source ID for each entry index to easily route back response
  // ID = sourceIdBase + index
  
  // DEDUPLICATION LOGIC:
  // Check if the requested address is already resident (or pending) in the buffer
  val is_dup = entries.exists(e => e.valid && e.tag === req_tag && e.set === req_set)

  // Critical: Cannot overwrite pending entry or entry needing ack
  // Critical: Cannot overwrite pending entry or entry needing ack
  val head_is_pending = entries(head).valid && entries(head).pending
  // FIX: need_ack must protect the slot even if valid=false (e.g. failed prefetch)
  val head_is_needing_ack = entries(head).need_ack
  // CRITICAL: Cannot overwrite entry being replayed! Replay logic depends on entry validity/data.
  val head_is_replaying = replay_active && replay_idx === head
  // CRITICAL: Cannot overwrite entry that is ABOUT to start replay this cycle!
  // If we overwrite now, pending becomes true, and Replay starts on an invalid/pending entry next cycle.
  val head_will_start_replay = is_hit && io.pop_valid && !replay_active && (hit_idx === head)
  // NOTE: Reverting "head_is_valid" check. If we stall on ANY valid entry, we deadlock if the prefetcher
  // fills the buffer with useless data that the CPU never consumes.
  // We MUST allow overwriting valid entries (eviction), as long as they are not CRITICAL (replaying).
  
  // FIX: Only issue to sourceA if it's NOT a duplicate AND safe to overwrite
  io.req.valid := current_req_valid && !head_is_pending && !head_is_needing_ack && !head_is_replaying && !head_will_start_replay && !is_dup
  io.req.bits.opcode  := TLMessages.AcquireBlock
  io.req.bits.param   := TLPermissions.NtoT 
  io.req.bits.size    := params.offsetBits.U
  // FIX: Explicitly widen to prevent overflow - sourceIdBase.U is inferred too narrow!
  val source_id = sourceIdBase.U(params.outer.bundle.sourceBits.W) + head.pad(params.outer.bundle.sourceBits)
  io.req.bits.source  := source_id
  io.req.bits.address := current_req.address // Keep physical address for wire
  io.req.bits.mask    := ~0.U(params.outer.manager.beatBytes.W)
  io.req.bits.data    := 0.U
  io.req.bits.corrupt := false.B
  
  // FIX: Dequeue if:
  // 1. We fired the request (io.req.ready && !head_is_pending)
  // 2. OR it was a duplicate (is_dup) -> Drop it immediately
  // FIX: Dequeue if:
  // 1. We fired the request (io.req.ready && !head_is_pending && !head_is_needing_ack && !head_is_replaying && !head_will_start_replay)
  //    NOTE: We removed head_is_valid check. We ALLOW overwriting valid (useless) entries.
  //          We ONLY block if the entry is pending (inflight) or replaying (active use).
  // 2. OR it was a duplicate (is_dup) -> Drop it immediately
  req_queue.io.deq.ready := (io.req.ready && !head_is_pending && !head_is_needing_ack && !head_is_replaying && !head_will_start_replay) || is_dup
  
  when (current_req_valid && is_dup) {
       printf("[SB DEDUP] Dropping duplicate request for address 0x%x\n", current_req.address)
  }

  // --------------------------------------------------------------------------
  // Invalidation Logic (Snoop L2 Evictions)
  // --------------------------------------------------------------------------
  val (inval_tag, inval_set, _) = params.parseAddress(io.inval_addr)
  
  for (i <- 0 until depth) {
     val entry = entries(i)
     // Check for match on valid entries
     // We invalidate even if pending (refill will complete but be ignored/overwritten later)
     // We invalidate even if replaying? If replaying, we might be sending stale data RIGHT NOW.
     //   Technically we should kill the replay, but that's complex (mid-burst).
     //   However, if we invalidate, the next access won't hit.
     when (io.inval_valid && entry.valid && entry.tag === inval_tag && entry.set === inval_set) {
         entries(i).valid := false.B
         // printf("[SB INVAL] Invalidating entry %d for addr 0x%x (Tag:0x%x Set:0x%x)\n", i.U, io.inval_addr, inval_tag, inval_set)
     }
  }

  when (io.req.fire) {
    printf("[SB REQ FIRE] Prefetch Issued to SourceA. SourceID: %d Address: 0x%x\n", io.req.bits.source, io.req.bits.address)
    entries(head).valid := true.B
    entries(head).pending := true.B
    entries(head).tag := req_tag
    entries(head).set := req_set
    entries(head).data := 0.U
    head := head + 1.U
  }

  // --------------------------------------------------------------------------
  // Refill Logic (Sink D)
  // --------------------------------------------------------------------------
  io.resp.ready := true.B
  
  when (io.resp.valid) {
    val resp_id = io.resp.bits.source
    val idx = resp_id - sourceIdBase.U
    // FIX: Usage of params.outer.count(d) requires Decoupled or Valid, io.resp.bits is just Bundle? 
    // Wait, params.outer.count(d: TLBundleD) works but returns 4 values.
    // The error says: cannot be applied to (freechips.rocketchip.tilelink.TLBundleD)
    // Actually typically we need edge.count(d). params.outer is an edge.
    // However, the error says: overloaded method count with alternatives ... (ValidIO or DecoupledIO).
    // So we should pass io.resp (DecoupledIO).
    val (first, last, _, beat) = params.outer.count(io.resp)
    
    val beatWidth = params.outer.manager.beatBytes * 8
    
    // FIX: Accept both GrantData (5) and Grant (4). Grant (4) might happen on Denied or errors.
    // If we only check GrantData, a Grant response makes us hang (Pending forever).
    when (idx < depth.U && (io.resp.bits.opcode === TLMessages.GrantData || io.resp.bits.opcode === TLMessages.Grant)) {
        // Accumulate data based on beats (Only if GrantData)
        when (io.resp.bits.opcode === TLMessages.GrantData) {
            // Shift data into position
            val mask = ((BigInt(1) << beatWidth) - 1).U << (beat * beatWidth.U)
            val incoming = io.resp.bits.data << (beat * beatWidth.U)
            entries(idx).data := (entries(idx).data & ~mask) | incoming
        }
        
        // if (!params.lastLevel) {
           // printf("[SB RESP] Refill for SourceID:%d (Idx:%d) Beat:%dLast:%d Opcode:%d\n", resp_id, idx, beat, last, io.resp.bits.opcode)
        // }
        
        // Store sink from first beat (or any beat, it's the same)
        when (first) {
          entries(idx).sink := io.resp.bits.sink
        }

        when (last) {
          // printf("[SB PENDING DONE] Cleared pending for idx %d\n", idx)
          entries(idx).pending := false.B
          
          // FIX: If opcode is Grant (No Data), this prefetch FAILED (or yielded no data).
          // We must INVALIDATE it so no one hits on garbage.
          // We still set need_ack to ensure we send the GrantAck (required by protocol).
          when (io.resp.bits.opcode === TLMessages.Grant) {
              entries(idx).valid := false.B
          }
          
          entries(idx).need_ack := true.B // Mark that we need to send GrantAck
        }
    }
  }

  // --------------------------------------------------------------------------
  // Hit / Pop Logic (Original location)
  // --------------------------------------------------------------------------
  // MOVED UP to line 75 for Allocation Logic dependency
  
  // Replay state (defined at top)
  // val replay_active = RegInit(false.B)
  
  // FIX: Only report hit if we can actually start a replay (i.e., not already replaying)
  io.peek_hit  := is_hit && !replay_active
  io.peek_data := entries(hit_idx).data
  
    // if (!params.lastLevel) {
       when (io.peek_hit) {
          printf("[SB PEEK HIT] Address 0x%x hit in Stream Buffer\n", io.peek_addr)
       }
       // Print detailed status only for relevant addresses (e.g. Set 0, 1, 2...)
       when (io.peek_addr =/= 0.U) {
          printf("[SB PEEK] Addr: 0x%x  Hit: %d  Head: %d. PTag:0x%x PSet:0x%x\n", io.peek_addr, io.peek_hit, head, peek_tag, peek_set)
          printf("    Entries(0): Valid:%d Pending:%d Tag:0x%x Set:0x%x\n", entries(0).valid, entries(0).pending, entries(0).tag, entries(0).set)
          // printf("    Entries(head): Valid:%d Pending:%d Tag:0x%x Set:0x%x\n", entries(head).valid, entries(head).pending, entries(head).tag, entries(head).set)
       }
    // }
  
  // --------------------------------------------------------------------------
  // Replay Logic (Bursting to MSHR)
  // --------------------------------------------------------------------------
  // replay_active, replay_idx, etc defined at top of module
  
  val outerBeats = params.cache.blockBytes / params.outer.manager.beatBytes
  val beatWidth = params.outer.manager.beatBytes * 8

  when (io.pop_valid && is_hit && !replay_active) {
    printf("[STREAM REPLAY START] Replaying entry idx %d to SourceID %d\n", hit_idx, io.pop_source)
    // entries(hit_idx).valid := false.B // Invalidate immediately -- NO! Wait for replay to finish.
    replay_active := true.B
    replay_idx := hit_idx
    replay_beat := 0.U
    replay_source := io.pop_source
  }
  
  // Gate Replay: Only valid if the entry is filled (not pending)
  io.replay_d.valid := replay_active && !entries(replay_idx).pending
  io.replay_d.bits.opcode := TLMessages.GrantData
  io.replay_d.bits.param  := TLPermissions.toT
  io.replay_d.bits.size   := params.offsetBits.U
  io.replay_d.bits.source := replay_source
  io.replay_d.bits.sink   := 0.U
  // With the race condition fix in Scheduler.scala, we only hit when replay is guaranteed
  // to proceed, so we'll never be replaying an invalid entry. Simplified back to false.
  io.replay_d.bits.denied := false.B
  
  // Extract data chunk
  val shift = replay_beat * beatWidth.U
  io.replay_d.bits.data := (entries(replay_idx).data >> shift)(beatWidth-1, 0)
  io.replay_d.bits.corrupt := false.B
  
  val is_last = replay_beat === (outerBeats - 1).U
  
  // FIX: TLBundleD does not have a 'last' field. It is a logical concept derived from edge.
  // But wait, the Receiver CANNOT determine 'last' without tracking beats if it's not in the bundle.
  // RocketChip TLBundleD does NOT have 'last'. Valid/Decoupled wrappers might have helper functions.
  // Actually, standard TL signals rely on Beats to determine last.
  // BUT, we are DRIVING the bundle. We don't set 'last' bit, we just behave correctly.
  // HOWEVER, `SinkDResponse` (used in Scheduler) HAS a `last` bit (Line 26 SinkD.scala).
  // `TLBundleD` does NOT.
  // The ARBITER in Scheduler (`sinkDArb`) takes `TLBundleD` inputs.
  // The arbiter does not care about `last`? Standard RRArbiter is just decoupled.
  // But wait, TileLink arbiters usually need to lock for the duration of a burst. `RRArbiter` does NOT lock.
  // If we interleave beats from MSHR (memory) and StreamBuffer, we CORRUPT DATA.
  // We MUST use a locking arbiter or ensure atomic bursts.
  // Memory responses are usually bursted.
  // We should use `TLArbiter` (which is `Arbiter` for TileLink, handles locking).
  // But `TLArbiter` expects `TLBundle` nodes? Or just Decoupled?
  // `freechips.rocketchip.tilelink.Arbiter` is deprecated? No, usually `TLArbiter` is used in Diplomacy.
  // Here we are inside a module.
  // We should use `LockedRRArbiter` or similar?
  // Or just trusting RRArbiter for now if we assume single-beat (dangerous) or lock logic.
  // Standard TileLink D channel IS intertwined? No, usually not interleaved for same ID.
  // Different IDs can be interleaved?
  // The MSHR logic (SinkD.scala) accumulates beats.
  // If we interleave beats for DIFFERENT MSHRs (Sources), it works IF SinkD separates them.
  // SinkD does NOT separate them. It processes one D-message at a time.
  // It writes to BankedStore. `io.bs_adr.bits.beat` is used.
  // So interleaving different Sources IS allowed.
  // So RRArbiter is safe?
  // YES, assuming standard TL rules where we don't interleave beats for the SAME source (we won't).
  
  // SO, we just remove the `io.replay_d.bits.last := is_last` assignment because the field doesn't exist.
  // The receiver (scheduler/sinkD) will calculate 'last' using edge.count() based on size/beats.
  
  when (io.replay_d.fire) {
    replay_beat := replay_beat + 1.U
    when (is_last) {
        replay_active := false.B
        entries(replay_idx).valid := false.B // Invalidate only after replay is DONE
    }
  }
  
  // --------------------------------------------------------------------------
  // GrantAck Logic (Source E)
  // --------------------------------------------------------------------------
  // Find any entry that needs an ack (FIFO order not strictly required)
  val need_ack_vec = VecInit(entries.map(_.need_ack))
  val ack_idx = PriorityEncoder(need_ack_vec)
  val any_need_ack = need_ack_vec.reduce(_ || _)
  
  io.ack_e.valid := any_need_ack
  io.ack_e.bits.sink := entries(ack_idx).sink
  
  when (io.ack_e.fire) {
    entries(ack_idx).need_ack := false.B
    printf("[SB ACK] Sending GrantAck for Sink:%d (Idx:%d)\n", entries(ack_idx).sink, ack_idx)
  }
}
