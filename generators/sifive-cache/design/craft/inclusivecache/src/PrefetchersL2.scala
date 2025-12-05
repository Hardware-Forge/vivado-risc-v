package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.tilelink._

/** L2 Next-Line Prefetcher: watches incoming accesses and generates a
  * prefetch request for the next cache block.
  */
class L2NLPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // Input: observed TileLink A access from inner side
    val in_valid = Input(Bool())
    val in_addr = Input(UInt(params.outer.bundle.addressBits.W))
    // Output: SourceARequest to engine
    val out = Decoupled(new SourceARequest(params))
  })

  io.out.valid := false.B
  io.out.bits := DontCare

  when (io.in_valid) {
    val nextAddr = io.in_addr + params.cache.blockBytes.U
    val parsed = params.parseAddress(nextAddr)
    io.out.valid := true.B
    io.out.bits.tag := parsed._1
    io.out.bits.set := parsed._2
    io.out.bits.param := 0.U
    io.out.bits.source := 0.U // will be remapped by downstream engine/scheduler
    io.out.bits.block := false.B
    printf("[L2 NL PREFETCHER] in_addr=0x%x next=0x%x tag=0x%x set=0x%x\n", io.in_addr, nextAddr, parsed._1, parsed._2)
  }
}

class L2StridedPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in_addr = Input(UInt(params.outer.bundle.addressBits.W))
    val out = Decoupled(new SourceARequest(params))
  })

  // Simplified strided detection: store previous block address
  val prev_valid = RegInit(false.B)
  val prev_addr = Reg(UInt(params.outer.bundle.addressBits.W))
  val prev_delta = RegInit(0.S((params.addressBits - params.offsetBits + 1).W))
  val repeat_cnt = RegInit(0.U(6.W))
  val threshold = 2.U
  val active = RegInit(false.B)
  val next_pref = Reg(UInt(params.outer.bundle.addressBits.W))
  val issued = RegInit(0.U(4.W))
  val degree = p(freechips.rocketchip.subsystem.InclusiveCacheKey).prefetchDegree

  io.out.valid := false.B
  io.out.bits := DontCare

  val block_bits = params.offsetBits
  val deltaBlocksS = ((io.in_addr >> block_bits).asSInt - (prev_addr >> block_bits).asSInt)
  when (io.in_valid) {
    when (prev_valid && deltaBlocksS === prev_delta && deltaBlocksS =/= 0.S) {
      repeat_cnt := repeat_cnt + 1.U
    } .otherwise {
      repeat_cnt := 0.U
      when (prev_valid) { prev_delta := deltaBlocksS }
      active := false.B
      issued := 0.U
    }
    prev_addr := io.in_addr
    prev_valid := true.B
    when (repeat_cnt >= threshold && !active) {
      active := true.B
      val lookahead = (prev_delta.asUInt << 1)
      val next_block = (io.in_addr >> block_bits) + lookahead
      next_pref := next_block << block_bits
      issued := 0.U
      printf("[L2 STRIDED PREFETCHER] ACTIVATED in_addr=0x%x prev_delta=%d next_pref=0x%x\n", io.in_addr, prev_delta.asUInt, next_pref)
    }
  }

  when (active && (issued < degree.U)) {
    val parsed = params.parseAddress(next_pref)
    io.out.valid := true.B
    io.out.bits.tag := parsed._1
    io.out.bits.set := parsed._2
    io.out.bits.param := 0.U
    io.out.bits.source := 0.U
    io.out.bits.block := false.B
    printf("[L2 STRIDED PREFETCHER] in_addr=0x%x next=0x%x tag=0x%x set=0x%x issued=%d\n", prev_addr, next_pref, parsed._1, parsed._2, issued)
    when (io.out.fire) {
      next_pref := next_pref + (prev_delta.asUInt << block_bits)
      issued := issued + 1.U
    }
  }
}

class L2StreamPrefetcher(params: InclusiveCacheParameters)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in_valid = Input(Bool())
    val in_addr = Input(UInt(params.outer.bundle.addressBits.W))
    val out = Decoupled(new SourceARequest(params))
  })

  val prev_valid = RegInit(false.B)
  val prev_addr = Reg(UInt(params.outer.bundle.addressBits.W))
  val consec_cnt = RegInit(0.U(8.W))
  val active = RegInit(false.B)
  val next_pref = Reg(UInt(params.outer.bundle.addressBits.W))
  val issued = RegInit(0.U(4.W))
  val degree = p(freechips.rocketchip.subsystem.InclusiveCacheKey).prefetchDegree

  io.out.valid := false.B
  io.out.bits := DontCare

  val block_bits = params.offsetBits
  val sequential = io.in_addr === prev_addr + params.cache.blockBytes.U
  when (io.in_valid) {
    when (prev_valid && sequential) { consec_cnt := consec_cnt + 1.U } .otherwise { consec_cnt := 0.U }
    prev_addr := io.in_addr
    prev_valid := true.B
    when (consec_cnt >= 1.U && !active) {
      active := true.B
      next_pref := io.in_addr + params.cache.blockBytes.U
      issued := 0.U
    }
  }

  when (active && (issued < degree.U)) {
    val parsed = params.parseAddress(next_pref)
    io.out.valid := true.B
    io.out.bits.tag := parsed._1
    io.out.bits.set := parsed._2
    io.out.bits.param := 0.U
    io.out.bits.source := 0.U
    io.out.bits.block := false.B
    when (io.out.fire) {
      next_pref := next_pref + params.cache.blockBytes.U
      issued := issued + 1.U
    }
  }
}
