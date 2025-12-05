/*
 * Lightweight PrefetchEngine skeleton for L2-side prefetching.
 *
 * This module accepts `SourceARequest`-shaped prefetch requests and
 * forwards them into the scheduler's arbitration for channel-A. It is
 * intentionally minimal: it provides a stable place to implement tag
 * checks, throttling, deduplication, and source-id reservation logic.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._

/** PrefetchEngine: accept high-level prefetch requests and present them
  * as `SourceARequest`s to the scheduler. Later this module should be
  * extended to perform tag checks, coalescing, rate-limiting and to
  * manage a dedicated source-id range for prefetch traffic.
  */
class PrefetchEngine(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // Input prefetch requests (address already expanded to tag/set form by caller)
    val req_in = Flipped(Decoupled(new SourceARequest(params)))
    // Output prefetch requests (deduplicated/throttled) to be sent to scheduler
    val req_out = Decoupled(new SourceARequest(params))
    // Grants coming back on the TL D channel, useful to decrement `issued` when a prefetch completes
    val mem_grant = Flipped(Valid(new SinkDResponse(params)))
    // Optional: expose simple counters / status for observability
    val issued = Output(UInt(32.W))
    // Directory query interface: issue a read (Valid) and observe the result (Valid)
    val dir_read = Output(Valid(new DirectoryRead(params)))
    val dir_result = Flipped(Valid(new DirectoryResult(params)))
  })

  // Basic parameters
  val recentSize = 8
  val recentTags = RegInit(VecInit(Seq.fill(recentSize)(0.U(params.tagBits.W))))
  val recentVal = RegInit(VecInit(Seq.fill(recentSize)(false.B)))
  val headPtr = RegInit(0.U(log2Ceil(recentSize).W))

  // Simple dedupe check
  def isDup(tag: UInt) = {
    recentVal
      .zip(recentTags)
      .map { case (v, t) => v && (t === tag) }
      .reduce(_ || _)
  }

  // Throttle: max outstanding issued in flight
  val degree = (p(freechips.rocketchip.subsystem.InclusiveCacheKey).prefetchDegree max 1)
  val issuedCnt = RegInit(0.U(32.W))
  val cooldown = RegInit(0.U(16.W))

  io.issued := issuedCnt

  // Directory query state: when a candidate prefetch arrives we consult
  // the directory to avoid issuing prefetches for resident lines.
  val pending = RegInit(false.B)
  val pendingReq = Reg(new SourceARequest(params))

  // Default directory signals
  io.dir_read.valid := false.B
  io.dir_read.bits := DontCare

  // Ensure prefetched requests use a reserved source id (one beyond the outer MSHR range)
  val prefetchSource = InclusiveCacheParameters.out_mshrs(params.cache, params.micro).U
  io.req_out.bits := DontCare
  // Always set a reserved source ID for prefetch requests
  io.req_out.bits.source := prefetchSource
  // default: not valid unless explicitly driven below
  io.req_out.valid := false.B
  val canIssue = !isDup(io.req_in.bits.tag) && (cooldown === 0.U) && (issuedCnt < degree.U)

  // When not already pending a directory lookup, start one for an incoming candidate
  when (!pending && io.req_in.valid && canIssue) {
    pending := true.B
    pendingReq := io.req_in.bits
    io.dir_read.valid := true.B
    io.dir_read.bits.set := io.req_in.bits.set
    io.dir_read.bits.tag := io.req_in.bits.tag
  }

  // By default don't consume req_in; consumption happens on skip (hit) or on successful issue
  io.req_in.ready := false.B

  // If we have a pending lookup and the directory responded, decide
  // whether to skip (hit) or issue (miss).
  when (pending && io.dir_result.valid) {
    when (io.dir_result.bits.hit) {
      // Line already present: drop the prefetch candidate
      pending := false.B
      // mark the input as accepted
      io.req_in.ready := true.B
      // update recentTags to avoid immediate duplicates
      recentTags(headPtr) := pendingReq.tag
      recentVal(headPtr) := true.B
      headPtr := Mux(headPtr === (recentSize-1).U, 0.U, headPtr + 1.U)
      printf("[L2 PREFETCH ENGINE] SKIP prefetch hit tag=0x%x set=0x%x\n", pendingReq.tag, pendingReq.set)
    } .otherwise {
      // Directory miss: attempt to issue the prefetch
      io.req_out.valid := true.B
      io.req_out.bits := pendingReq
      io.req_out.bits.source := prefetchSource
      when (io.req_out.fire) {
        pending := false.B
        // consume input now that we've issued
        io.req_in.ready := true.B
        // push into recent tags
        recentTags(headPtr) := io.req_out.bits.tag
        recentVal(headPtr) := true.B
        headPtr := Mux(headPtr === (recentSize-1).U, 0.U, headPtr + 1.U)
        issuedCnt := issuedCnt + 1.U
        cooldown := p(freechips.rocketchip.subsystem.InclusiveCacheKey).prefetchDistance.U
        printf("[L2 PREFETCH ENGINE] ISSUE(tag=0x%x set=0x%x) issued=%d\n", io.req_out.bits.tag, io.req_out.bits.set, issuedCnt)
      }
    }
  }

  when (io.req_out.fire) {
    // push into recent tags
    recentTags(headPtr) := io.req_out.bits.tag
    recentVal(headPtr) := true.B
    headPtr := Mux(headPtr === (recentSize-1).U, 0.U, headPtr + 1.U)
    issuedCnt := issuedCnt + 1.U
    // set the source id to the reserved prefetch source before emission
    io.req_out.bits.source := prefetchSource
    // start cooldown
    cooldown := p(freechips.rocketchip.subsystem.InclusiveCacheKey).prefetchDistance.U
    printf("[L2 PREFETCH ENGINE] ISSUE tag=0x%x set=0x%x block=%d issued=%d\n", io.req_out.bits.tag, io.req_out.bits.set, io.req_out.bits.block.asUInt, issuedCnt)
  }

  // Decrement issued counter when the corresponding grant completes.
  // Only decrement once per completed grant (detect final beat via `.last`).
  // Decrement when scheduler's SinkD reports a completed grant for prefetch source
  when (io.mem_grant.valid && io.mem_grant.bits.last) {
    when (issuedCnt =/= 0.U) {
      issuedCnt := issuedCnt - 1.U
      printf("[L2 PREFETCH ENGINE] COMPLETED grant for source=%d issued=%d\n", io.mem_grant.bits.source, issuedCnt - 1.U)
    }
  }

  // rudimentary cooldown counter
  when (cooldown =/= 0.U) { cooldown := cooldown - 1.U }
}
