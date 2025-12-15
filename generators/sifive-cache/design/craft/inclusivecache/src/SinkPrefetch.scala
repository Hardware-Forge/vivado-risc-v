/*
 * SinkPrefetch
 *
 * This module converts prefetch requests into FullRequests that can be
 * processed by the cache's MSHR allocation logic, just like regular
 * SinkA requests. This allows prefetched data to be stored in the L2 cache.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.tilelink._

class SinkPrefetch(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val req = Decoupled(new FullRequest(params))
    // Prefetch address input from prefetcher
    val prefetch = Flipped(Decoupled(UInt(params.outer.bundle.addressBits.W)))
  })

  // Parse the address into tag, set, offset
  val (tag, set, offset) = params.parseAddress(io.prefetch.bits)

  // Simple pass-through with request formation
  // We use a register to hold the request while waiting for it to be accepted
  val req_valid = RegInit(false.B)
  val req_bits = Reg(new FullRequest(params))

  when (io.prefetch.fire) {
    req_valid := true.B
    req_bits.prio := VecInit(true.B, false.B, false.B)  // Priority A (lowest, like regular requests)
    req_bits.control := false.B
    req_bits.prefetch := true.B  // Mark as prefetch - no response needed
    req_bits.opcode := TLMessages.AcquireBlock  // Request for shared/exclusive data
    req_bits.param := TLPermissions.NtoB        // None to Branch (shared read)
    req_bits.size := log2Ceil(params.cache.blockBytes).U  // Full cache line
    req_bits.source := 0.U  // Prefetch has no inner source to respond to
    req_bits.tag := tag
    req_bits.set := set
    req_bits.offset := 0.U  // Always start of block
    req_bits.put := 0.U     // No put data
  }.elsewhen (io.req.fire) {
    req_valid := false.B
  }

  io.req.valid := req_valid
  io.req.bits := req_bits

  // Can accept new prefetch when we don't have a pending request
  io.prefetch.ready := !req_valid || io.req.fire

  when (io.prefetch.fire) {
    printf("[SINK PREFETCH] Injecting addr=0x%x set=%d tag=0x%x\n",
           io.prefetch.bits, set, tag)
  }
}
