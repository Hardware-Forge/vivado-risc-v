/*
 * Stream Buffer for BOOM L1 DCache
 *
 * A dedicated buffer for storing prefetched data.
 * - Decouples prefetch storage from MSHRs.
 * - Manages its own TileLink requests to L2.
 * - Uses a separate pool of Source IDs.
 */

package boom.lsu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.rocket._

import boom.common._

class StreamBufferRequest(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val address = UInt(coreMaxAddrBits.W)
}

class StreamBufferIO(implicit edge: TLEdgeOut, p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  // Input from Prefetcher
  val alloc = Flipped(Decoupled(new StreamBufferRequest))

  // Interface to Outer Memory (Source A)
  val req = Decoupled(new TLBundleA(edge.bundle))
  
  // Interface from Outer Memory (Sink D)
  val resp = Flipped(Decoupled(new TLBundleD(edge.bundle)))
  
  // Interface to Outer Memory (Source E - GrantAck)
  val ack_e = Decoupled(new TLBundleE(edge.bundle))

  // Interface for Hit Check
  val peek_addr = Input(UInt(coreMaxAddrBits.W))
  val peek_hit  = Output(Bool())
  val peek_pending = Output(Bool()) // New: Expose pending state
  val peek_way_en = Output(UInt(nWays.W))
  val peek_data = Output(UInt((cacheBlockBytes * 8).W))
                                                               
  // Signal to "consume" the entry (transfer to L1 cache)
  val pop_valid = Input(Bool())

  // Replay Interface: Stream Buffer provides data for cache refill
  val refill = Decoupled(new L1DataWriteReq)
  val meta_write = Decoupled(new L1MetaWriteReq)

  // Invalidation port (from MSHR allocation)
  val inval = Flipped(Valid(UInt(coreMaxAddrBits.W)))
  val debug_inval_matches = Output(Bool()) // Debug output
}

object StreamBuffer {
  def apply(depth: Int, sourceIdBase: Int)(implicit edge: TLEdgeOut, p: Parameters): StreamBuffer = {
    new StreamBuffer(depth, sourceIdBase)
  }
}

class StreamBuffer(depth: Int = 4, sourceIdBase: Int = 0)(implicit edge: TLEdgeOut, p: Parameters) 
  extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new StreamBufferIO)

  require((sourceIdBase + depth) <= (1 << edge.bundle.sourceBits), 
    s"StreamBuffer depth $depth with base $sourceIdBase exceeds available Source IDs (${1 << edge.bundle.sourceBits})")

  // Entry storage
  val entries = RegInit(VecInit(Seq.fill(depth) {
    val entry = Wire(new Bundle {
      val valid = Bool()
      val tag   = UInt(tagBits.W)
      val idx   = UInt(idxBits.W)
      val data  = Vec(refillCycles, UInt(encRowBits.W))
      val pending = Bool()
      val issued_tl = Bool() // New: Track if TileLink request sent
      val sink  = UInt(edge.bundle.sinkBits.W)
      val need_ack = Bool()
      val way_en = UInt(nWays.W)
    })
    entry.valid := false.B
    entry.tag := 0.U
    entry.idx := 0.U
    entry.data := VecInit(Seq.fill(refillCycles)(0.U(encRowBits.W)))
    entry.pending := false.B
    entry.issued_tl := false.B
    entry.sink := 0.U
    entry.need_ack := false.B
    entry.way_en := 0.U
    entry
  }))
  
  // --------------------------------------------------------------------------
  // Allocation Logic (FIFO)
  // --------------------------------------------------------------------------
  // --------------------------------------------------------------------------
  // Allocation Logic (Immediate)
  // --------------------------------------------------------------------------
  val head = RegInit(0.U(log2Ceil(depth).W))
  
  // No Queue. Connect direct.
  val req_tag = io.alloc.bits.address >> untagBits
  val req_idx = io.alloc.bits.address(untagBits-1, blockOffBits)

  // Deduplication: Check if address already in buffer
  val is_dup = entries.exists(e => e.valid && e.tag === req_tag && e.idx === req_idx)

  // Allocation: Find free/replacable entry (head FIFO)
  // Head is busy if valid and (pending or need_ack) - cannot overwrite
  val head_is_busy = entries(head).valid && (entries(head).pending || entries(head).need_ack)
  
  io.alloc.ready := !head_is_busy || is_dup
  
  when (io.alloc.fire) {
    when (!is_dup) { // Only allocate if not dup
        printf("[SB ALLOC IMMEDIATE] addr=0x%x head=%d\n", io.alloc.bits.address, head)
        entries(head).valid := true.B
        entries(head).pending := true.B
        entries(head).issued_tl := false.B
        entries(head).tag := req_tag
        entries(head).idx := req_idx
        entries(head).need_ack := false.B
        head := Mux(head === (depth - 1).U, 0.U, head + 1.U)
    } .otherwise {
        printf("[SB ALLOC DUP] addr=0x%x\n", io.alloc.bits.address)
    }
  }

  // --------------------------------------------------------------------------
  // TileLink Issue Logic (Scan for pending && !issued)
  // --------------------------------------------------------------------------
  // Find an entry that needs to issue Acquire
  val issue_vec = VecInit(entries.map(e => e.valid && e.pending && !e.issued_tl))
  val issue_idx = PriorityEncoder(issue_vec)
  val can_issue = issue_vec.reduce(_ || _)
  
  io.req.valid := can_issue
  io.req.bits := edge.AcquireBlock(
    fromSource      = (sourceIdBase.U(edge.bundle.sourceBits.W) + issue_idx.pad(edge.bundle.sourceBits)),
    toAddress       = Cat(entries(issue_idx).tag, entries(issue_idx).idx) << blockOffBits,
    lgSize          = lgCacheBlockBytes.U,
    growPermissions = TLPermissions.NtoB)._2

  when (io.req.fire) {
     printf("[SB REQ FIRE] Issuing TL Acquire src=%d idx=%d addr=0x%x\n",
      io.req.bits.source, issue_idx, io.req.bits.address)
     entries(issue_idx).issued_tl := true.B
  }

  // --------------------------------------------------------------------------
  // Refill Logic (Sink D)
  // --------------------------------------------------------------------------
  io.resp.ready := true.B
  
  when (io.resp.valid) {
    val resp_source = io.resp.bits.source
    val idx = resp_source - sourceIdBase.U
    
    val (first, last, _, beat) = edge.count(io.resp)
    
    when (idx < depth.U && (io.resp.bits.opcode === TLMessages.GrantData || io.resp.bits.opcode === TLMessages.Grant)) {
      printf("[SB RESP] src=%d idx=%d opcode=%d beat=%d first=%d last=%d\n",
        resp_source, idx, io.resp.bits.opcode, beat, first, last)
      when (edge.hasData(io.resp.bits)) {
        entries(idx).data(beat) := io.resp.bits.data
      }
      
      when (first) {
        entries(idx).sink := io.resp.bits.sink
      }

      when (last) {
        printf("[SB PENDING] Clearing pending for idx=%d sink=%d\n", idx, io.resp.bits.sink)
        entries(idx).pending := false.B
        // GrantData (opcode 5) and Grant (opcode 4) require GrantAck
        entries(idx).need_ack := io.resp.bits.opcode === TLMessages.GrantData || 
                                  io.resp.bits.opcode === TLMessages.Grant
      }
    }
  }

  // Debug pending state
  when (io.req.fire) {
    printf("[SB PENDING] Setting pending for head=%d addr=0x%x\n", head, io.req.bits.address)
  }

  // --------------------------------------------------------------------------
  // Hit / Pop Logic
  // --------------------------------------------------------------------------
  val peek_tag = io.peek_addr >> untagBits
  val peek_idx = io.peek_addr(untagBits-1, blockOffBits)
  
  val hits = VecInit((0 until depth).map { i => 
    entries(i).valid && !entries(i).pending &&
    entries(i).idx === peek_idx && entries(i).tag === peek_tag
  })

  val hits_pending = VecInit((0 until depth).map { i => 
    entries(i).valid && entries(i).pending &&
    entries(i).idx === peek_idx && entries(i).tag === peek_tag
  })
  
  val hit_idx = PriorityEncoder(hits)
  val is_hit = hits.reduce(_ || _)
  val is_pending_hit = hits_pending.reduce(_ || _) // Check if any pending entry matches
  
  // Replay state
  val replay_active = RegInit(false.B)
  val replay_idx    = Reg(UInt(log2Ceil(depth).W))
  val replay_beat   = Reg(UInt(log2Ceil(refillCycles + 1).W))
  
  io.peek_hit  := is_hit && !replay_active
  io.peek_pending := is_pending_hit
  io.peek_way_en := entries(hit_idx).way_en
  io.peek_data := entries(hit_idx).data.asUInt
  
  // --------------------------------------------------------------------------
  // Replay Logic (Write data to cache)
  // --------------------------------------------------------------------------
  // Auto-Install Logic
  // Look for entries that are valid, full, and not waiting for anything
  val auto_install_vec = VecInit(entries.zipWithIndex.map { case (e, i) =>
    e.valid && !e.pending && !e.need_ack
  })
  val auto_install_idx = PriorityEncoder(auto_install_vec)
  val can_auto_install = auto_install_vec.reduce(_ || _)

  // Priority:
  // 1. Explicit Pop (Demand Miss)
  // 2. Auto Install (Background)
  
  when (!replay_active) {
    when (io.pop_valid && is_hit) {
      printf("[SB POP] Starting Demand Replay idx=%d\n", hit_idx)
      replay_active := true.B
      replay_idx := hit_idx
      replay_beat := 0.U
    } /*.elsewhen (can_auto_install && io.refill.ready) {
      printf("[SB AUTO] Starting Auto-Install idx=%d\n", auto_install_idx)
      replay_active := true.B
      replay_idx := auto_install_idx
      replay_beat := 0.U
    }*/
  }
  
  when (io.peek_hit) {
    printf("[SB HIT] addr=0x%x idx=%d pop=%d is_hit=%d replay=%d\n", io.peek_addr, hit_idx, io.pop_valid, is_hit, replay_active)
  }
  
  val replay_block_addr = Cat(entries(replay_idx).tag, entries(replay_idx).idx) << blockOffBits
  
  io.refill.valid := replay_active
  io.refill.bits.addr   := replay_block_addr | (replay_beat << rowOffBits)
  io.refill.bits.way_en := entries(replay_idx).way_en
  io.refill.bits.wmask  := ~(0.U(rowWords.W))
  io.refill.bits.data   := entries(replay_idx).data(replay_beat)
  
  val is_last_beat = replay_beat === (refillCycles - 1).U
  
  when (io.refill.fire) {
    replay_beat := replay_beat + 1.U
    when (is_last_beat) {
      replay_active := false.B
      entries(replay_idx).valid := false.B
    }
  }
  
  // Meta write (after data refill complete)
  io.meta_write.valid := false.B
  io.meta_write.bits := DontCare
  // TODO: Connect meta_write after refill completes
  
  // --------------------------------------------------------------------------
  // GrantAck Logic (Source E)
  // --------------------------------------------------------------------------
  val need_ack_vec = VecInit(entries.map(_.need_ack))
  val ack_idx = PriorityEncoder(need_ack_vec)
  val any_need_ack = need_ack_vec.reduce(_ || _)
  
  io.ack_e.valid := any_need_ack
  io.ack_e.bits := edge.GrantAck(entries(ack_idx).sink)
  
  when (io.ack_e.fire) {
    printf("[SB ACK] Sending GrantAck sink=%d idx=%d\n", entries(ack_idx).sink, ack_idx)
    entries(ack_idx).need_ack := false.B
  }

  // --------------------------------------------------------------------------
  // Invalidation Logic
  // --------------------------------------------------------------------------
  val inval_tag = io.inval.bits >> untagBits
  val inval_idx = io.inval.bits(untagBits-1, blockOffBits)
  
  io.debug_inval_matches := false.B

  when (io.inval.valid) {
    printf("[SB INVAL RX] addr=0x%x\n", io.inval.bits)
    for (i <- 0 until depth) {
      // Check for match - ONLY invalidate if entry is complete (not pending)
      // This prevents MSHR allocs from killing in-flight prefetches
      when (entries(i).valid && !entries(i).pending && entries(i).tag === inval_tag && entries(i).idx === inval_idx) {
        entries(i).valid := false.B
        io.debug_inval_matches := true.B
        printf("[SB INVAL] Invalidating entry %d addr=0x%x\n", i.U, io.inval.bits)
      }
    }
  }
}
