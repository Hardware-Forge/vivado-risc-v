/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.experimental.dataview.BundleUpcastable
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem.InclusiveCacheKey

class InclusiveCacheBankScheduler(params: InclusiveCacheParameters) extends Module
{
  implicit val p: Parameters = params.p

  val io = IO(new Bundle {
    val in = Flipped(TLBundle(params.inner.bundle))
    val out = TLBundle(params.outer.bundle)
    // Way permissions
    val ways = Flipped(Vec(params.allClients, UInt(params.cache.ways.W)))
    val divs = Flipped(Vec(params.allClients, UInt((InclusiveCacheParameters.lfsrBits + 1).W)))
    // Control port
    val req = Flipped(Decoupled(new SinkXRequest(params)))
    val resp = Decoupled(new SourceXRequest(params))
    val counters = Output(new Bundle {
      val l2_hit     = Bool()
      val l2_miss    = Bool()
      val mshr_alloc = Bool()
      // L2 prefetcher performance counters
      // pf_issued: L2 prefetch requests sent toward RAM
      // pf_used:   L2 prefetch hits serviced from the stream buffer
      val pf_issued = Bool()
      val pf_used   = Bool()
    })
  })

  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  // Instantiate L2-to-RAM prefetcher
  val prefetcher = L2RamPrefetcher(params)
  prefetcher.io.can_prefetch := true.B

  io.out.c <> sourceC.io.c
  // io.out.e <> sourceE.io.e // MOVED: Now goes through arbiter to include StreamBuffer acks
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d // RESTORED: This is L2->L1 response, unrelated to SinkD
  // sinkD logic moved to Arbiter below
  io.resp <> sourceX.io.x

  val sinkA = Module(new SinkA(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))
  // val sinkPrefetch = Module(new SinkPrefetch(params)) // REMOVED

  sinkA.io.a <> io.in.a
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  // sinkD.io.d <> io.out.d // REMOVED: Managed by Demux
  sinkX.io.x <> io.req

  io.out.b.ready := true.B // disconnected

  val directory = Module(new Directory(params))
  val bankedStore = Module(new BankedStore(params))
  val requests = Module(new ListBuffer(ListBufferParameters(new QueuedRequest(params), 3*params.mshrs, params.secondary, false)))
  val mshrs = Seq.fill(params.mshrs) { Module(new MSHR(params)) }
  val abc_mshrs = mshrs.init.init
  val bc_mshr = mshrs.init.last
  val c_mshr = mshrs.last
  val nestedwb = Wire(new NestedWriteback(params))

  // Deliver messages from Sinks to MSHRs
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.sinkc.valid := sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set
    m.io.sinkd.valid := sinkD.io.resp.valid && sinkD.io.resp.bits.source === i.U
    m.io.sinke.valid := sinkE.io.resp.valid && sinkE.io.resp.bits.sink   === i.U
    m.io.sinkc.bits := sinkC.io.resp.bits
    m.io.sinkd.bits := sinkD.io.resp.bits
    m.io.sinke.bits := sinkE.io.resp.bits
    m.io.nestedwb := nestedwb
  }

  // If the pre-emption BC or C MSHR have a matching set, the normal MSHR must be blocked
  val mshr_stall_abc = abc_mshrs.map { m =>
    (bc_mshr.io.status.valid && m.io.status.bits.set === bc_mshr.io.status.bits.set) ||
    ( c_mshr.io.status.valid && m.io.status.bits.set ===  c_mshr.io.status.bits.set)
  }
  val mshr_stall_bc =
    c_mshr.io.status.valid && bc_mshr.io.status.bits.set === c_mshr.io.status.bits.set
  val mshr_stall_c = false.B
  val mshr_stall = mshr_stall_abc :+ mshr_stall_bc :+ mshr_stall_c


  val stall_abc = (mshr_stall_abc zip abc_mshrs) map { case (s, m) => s && m.io.status.valid }
  if (!params.lastLevel || !params.firstLevel)
    params.ccover(stall_abc.reduce(_||_), "SCHEDULER_ABC_INTERLOCK", "ABC MSHR interlocked due to pre-emption")
  if (!params.lastLevel)
    params.ccover(mshr_stall_bc && bc_mshr.io.status.valid, "SCHEDULER_BC_INTERLOCK", "BC MSHR interlocked due to pre-emption")

  // Consider scheduling an MSHR only if all the resources it requires are available
  val mshr_request = Cat((mshrs zip mshr_stall).map { case (m, s) =>
    m.io.schedule.valid && !s &&
      (sourceA.io.req.ready || !m.io.schedule.bits.a.valid) &&
      (sourceB.io.req.ready || !m.io.schedule.bits.b.valid) &&
      (sourceC.io.req.ready || !m.io.schedule.bits.c.valid) &&
      (sourceD.io.req.ready || !m.io.schedule.bits.d.valid) &&
      (sourceE.io.req.ready || !m.io.schedule.bits.e.valid) &&
      (sourceX.io.req.ready || !m.io.schedule.bits.x.valid) &&
      (directory.io.write.ready || !m.io.schedule.bits.dir.valid)
  }.reverse)

  // Round-robin arbitration of MSHRs
  val robin_filter = RegInit(0.U(params.mshrs.W))
  val robin_request = Cat(mshr_request, mshr_request & robin_filter)
  val mshr_selectOH2 = ~(leftOR(robin_request) << 1) & robin_request
  val mshr_selectOH = mshr_selectOH2(2*params.mshrs-1, params.mshrs) | mshr_selectOH2(params.mshrs-1, 0)
  val mshr_select = OHToUInt(mshr_selectOH)
  val schedule = Mux1H(mshr_selectOH, mshrs.map(_.io.schedule.bits))
  val scheduleTag = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.tag))
  val scheduleSet = Mux1H(mshr_selectOH, mshrs.map(_.io.status.bits.set))

  // When an MSHR wins the schedule, it has lowest priority next time
  when (mshr_request.orR) { robin_filter := ~rightOR(mshr_selectOH) }

  // Fill in which MSHR sends the request
  schedule.a.bits.source := mshr_select
  
  // Forward delcaration for Stream Buffer hit logic (defined later)
  val stream_hit_wire = Wire(Bool())
  schedule.c.bits.source := Mux(schedule.c.bits.opcode(1), mshr_select, 0.U) // only set for Release[Data] not ProbeAck[Data]
  schedule.d.bits.sink   := mshr_select

  // FIX: Suppress SourceA if Stream Buffer Hit
  sourceA.io.req.valid := schedule.a.valid && !stream_hit_wire
  sourceB.io.req.valid := schedule.b.valid
  sourceC.io.req.valid := schedule.c.valid
  sourceD.io.req.valid := schedule.d.valid
  sourceE.io.req.valid := schedule.e.valid
  sourceX.io.req.valid := schedule.x.valid

  sourceA.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.a.bits)) := schedule.a.bits
  sourceB.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.b.bits)) := schedule.b.bits
  sourceC.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.c.bits)) := schedule.c.bits
  sourceD.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.d.bits)) := schedule.d.bits
  sourceE.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.e.bits)) := schedule.e.bits
  sourceX.io.req.bits.viewAsSupertype(chiselTypeOf(schedule.x.bits)) := schedule.x.bits

  directory.io.write.valid := schedule.dir.valid
  directory.io.write.bits.viewAsSupertype(chiselTypeOf(schedule.dir.bits)) := schedule.dir.bits

  // Forward meta-data changes from nested transaction completion
  val select_c  = mshr_selectOH(params.mshrs-1)
  val select_bc = mshr_selectOH(params.mshrs-2)
  nestedwb.set   := Mux(select_c, c_mshr.io.status.bits.set, bc_mshr.io.status.bits.set)
  nestedwb.tag   := Mux(select_c, c_mshr.io.status.bits.tag, bc_mshr.io.status.bits.tag)
  nestedwb.b_toN       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := select_bc && bc_mshr.io.schedule.bits.dir.valid && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  nestedwb.b_clr_dirty := select_bc && bc_mshr.io.schedule.bits.dir.valid
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  // Pick highest priority request
  // Priority: sinkC > sinkX > sinkA
  val request = Wire(Decoupled(new FullRequest(params)))
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkC.io.req.valid)
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits,
                  sinkA.io.req.bits)) // Removed sinkPrefetch
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkX.io.req.valid
  // sinkPrefetch.io.req.ready ... REMOVED

  // If no MSHR has been assigned to this set, we need to allocate one
  val setMatches = Cat(mshrs.map { m => m.io.status.valid && m.io.status.bits.set === request.bits.set }.reverse)
  val alloc = !setMatches.orR // NOTE: no matches also means no BC or C pre-emption on this set
  // If a same-set MSHR says that requests of this type must be blocked (for bounded time), do it
  val blockB = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  val blockC = Mux1H(setMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)
  // If a same-set MSHR says that requests of this type must be handled out-of-band, use special BC|C MSHR
  // ... these special MSHRs interlock the MSHR that said it should be pre-empted.
  val nestB  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  val nestC  = Mux1H(setMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  // Prevent priority inversion; we may not queue to MSHRs beyond our level
  val prioFilter = Cat(request.bits.prio(2), !request.bits.prio(0), ~0.U((params.mshrs-2).W))
  val lowerMatches = setMatches & prioFilter
  // If we match an MSHR <= our priority that neither blocks nor nests us, queue to it.
  val queue = lowerMatches.orR && !nestB && !nestC && !blockB && !blockC

  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // It might happen that lowerMatches has >1 bit if the two special MSHRs are in-use
  // We want to Q to the highest matching priority MSHR.
  val lowerMatches1 =
    Mux(lowerMatches(params.mshrs-1), 1.U << (params.mshrs-1),
    Mux(lowerMatches(params.mshrs-2), 1.U << (params.mshrs-2),
    lowerMatches))

  // If this goes to the scheduled MSHR, it may need to be bypassed
  // Alternatively, the MSHR may be refilled from a request queued in the ListBuffer
  val selected_requests = Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & requests.io.valid
  val a_pop = selected_requests((0 + 1) * params.mshrs - 1, 0 * params.mshrs).orR
  val b_pop = selected_requests((1 + 1) * params.mshrs - 1, 1 * params.mshrs).orR
  val c_pop = selected_requests((2 + 1) * params.mshrs - 1, 2 * params.mshrs).orR
  val bypassMatches = (mshr_selectOH & lowerMatches1).orR &&
                      Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
  val may_pop = a_pop || b_pop || c_pop
  val bypass = request.valid && queue && bypassMatches
  val will_reload = schedule.reload && (may_pop || bypass)
  val will_pop = schedule.reload && may_pop && !bypass

  params.ccover(mshr_selectOH.orR && bypass, "SCHEDULER_BYPASS", "Bypass new request directly to conflicting MSHR")
  params.ccover(mshr_selectOH.orR && will_reload, "SCHEDULER_RELOAD", "Back-to-back service of two requests")
  params.ccover(mshr_selectOH.orR && will_pop, "SCHEDULER_POP", "Service of a secondary miss")

  // Repeat the above logic, but without the fan-in
  mshrs.zipWithIndex.foreach { case (m, i) =>
    val sel = mshr_selectOH(i)
    m.io.schedule.ready := sel
    val a_pop = requests.io.valid(params.mshrs * 0 + i)
    val b_pop = requests.io.valid(params.mshrs * 1 + i)
    val c_pop = requests.io.valid(params.mshrs * 2 + i)
    val bypassMatches = lowerMatches1(i) &&
                        Mux(c_pop || request.bits.prio(2), !c_pop, Mux(b_pop || request.bits.prio(1), !b_pop, !a_pop))
    val may_pop = a_pop || b_pop || c_pop
    val bypass = request.valid && queue && bypassMatches
    val will_reload = m.io.schedule.bits.reload && (may_pop || bypass)
    m.io.allocate.bits.viewAsSupertype(chiselTypeOf(requests.io.data)) := Mux(bypass, WireInit(new QueuedRequest(params), init = request.bits), requests.io.data)
    m.io.allocate.bits.set := m.io.status.bits.set
    m.io.allocate.bits.repeat := m.io.allocate.bits.tag === m.io.status.bits.tag
    m.io.allocate.valid := sel && will_reload
  }

  // Determine which of the queued requests to pop (supposing will_pop)
  val prio_requests = ~(~requests.io.valid | (requests.io.valid >> params.mshrs) | (requests.io.valid >> 2*params.mshrs))
  val pop_index = OHToUInt(Cat(mshr_selectOH, mshr_selectOH, mshr_selectOH) & prio_requests)
  requests.io.pop.valid := will_pop
  requests.io.pop.bits  := pop_index
  
  if (!params.lastLevel) { // Use this guard to avoid printing in other instantiations if any
    when (will_pop) {
      printf("[SCHED POP] Popping index %d. MSHR %d. Reload: %d. MayPop: %d. Bypass: %d.\n", 
             pop_index, mshr_select, schedule.reload, may_pop, bypass)
    }
    when (mshr_selectOH.orR && !will_pop && schedule.reload) {
       printf("[SCHED NOPOP] MSHR %d Reloading but NOT popping. MayPop: %d. Bypass: %d.\n", mshr_select, may_pop, bypass)
    }
  }

  // Reload from the Directory if the next MSHR operation changes tags
  val lb_tag_mismatch = scheduleTag =/= requests.io.data.tag
  val mshr_uses_directory_assuming_no_bypass = schedule.reload && may_pop && lb_tag_mismatch
  val mshr_uses_directory_for_lb = will_pop && lb_tag_mismatch
  val mshr_uses_directory = will_reload && scheduleTag =/= Mux(bypass, request.bits.tag, requests.io.data.tag)

  // Is there an MSHR free for this request?
  val mshr_validOH = Cat(mshrs.map(_.io.status.valid).reverse)
  val mshr_free = (~mshr_validOH & prioFilter).orR

  // Fanout the request to the appropriate handler (if any)
  val bypassQueue = schedule.reload && bypassMatches
  val request_alloc_cases =
     (alloc && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid)
  request.ready := request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready))
  
  params.ccover(request.valid && request_alloc_cases, "SCHEDULER_ALLOC", "Allocate new MSHR (Primary Miss)")
  
  val alloc_uses_directory = request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory
  directory.io.read.valid := mshr_uses_directory || alloc_uses_directory
  directory.io.read.bits.set := Mux(mshr_uses_directory_for_lb, scheduleSet,          request.bits.set)
  directory.io.read.bits.tag := Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag)

  // Enqueue the request if not bypassed directly into an MSHR
  requests.io.push.valid := request.valid && queue && !bypassQueue
  requests.io.push.bits.data  := request.bits
  requests.io.push.bits.index := Mux1H(
    request.bits.prio, Seq(
      OHToUInt(lowerMatches1 << params.mshrs*0),
      OHToUInt(lowerMatches1 << params.mshrs*1),
      OHToUInt(lowerMatches1 << params.mshrs*2)))

  val mshr_insertOH = ~(leftOR(~mshr_validOH) << 1) & ~mshr_validOH & prioFilter
  (mshr_insertOH.asBools zip mshrs) map { case (s, m) =>
    when (request.valid && alloc && s && !mshr_uses_directory_assuming_no_bypass) {
      m.io.allocate.valid := true.B
      m.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
      m.io.allocate.bits.repeat := false.B
    }
  }

  when (request.valid && nestB && !bc_mshr.io.status.valid && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    bc_mshr.io.allocate.valid := true.B
    bc_mshr.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
    bc_mshr.io.allocate.bits.repeat := false.B
    assert (!request.bits.prio(0))
  }
  bc_mshr.io.allocate.bits.prio(0) := false.B

  when (request.valid && nestC && !c_mshr.io.status.valid && !mshr_uses_directory_assuming_no_bypass) {
    c_mshr.io.allocate.valid := true.B
    c_mshr.io.allocate.bits.viewAsSupertype(chiselTypeOf(request.bits)) := request.bits
    c_mshr.io.allocate.bits.repeat := false.B
    assert (!request.bits.prio(0))
    assert (!request.bits.prio(1))
  }
  c_mshr.io.allocate.bits.prio(0) := false.B
  c_mshr.io.allocate.bits.prio(1) := false.B

  // Fanout the result of the Directory lookup
  val dirTarget = Mux(alloc, mshr_insertOH, Mux(nestB,(BigInt(1) << (params.mshrs-2)).U,(BigInt(1) << (params.mshrs-1)).U))
  val directoryFanout = params.dirReg(RegNext(Mux(mshr_uses_directory, mshr_selectOH, Mux(alloc_uses_directory, dirTarget, 0.U))))
  mshrs.zipWithIndex.foreach { case (m, i) =>
    m.io.directory.valid := directoryFanout(i)
    m.io.directory.bits := directory.io.result.bits
  }

  // MSHR response meta-data fetch
  sinkC.io.way :=
    Mux(bc_mshr.io.status.valid && bc_mshr.io.status.bits.set === sinkC.io.set,
      bc_mshr.io.status.bits.way,
      Mux1H(abc_mshrs.map(m => m.io.status.valid && m.io.status.bits.set === sinkC.io.set),
            abc_mshrs.map(_.io.status.bits.way)))
  sinkD.io.way := VecInit(mshrs.map(_.io.status.bits.way))(sinkD.io.source)
  sinkD.io.set := VecInit(mshrs.map(_.io.status.bits.set))(sinkD.io.source)

  // Beat buffer connections between components
  sinkA.io.pb_pop <> sourceD.io.pb_pop
  sourceD.io.pb_beat := sinkA.io.pb_beat
  sinkC.io.rel_pop <> sourceD.io.rel_pop
  sourceD.io.rel_beat := sinkC.io.rel_beat

  // BankedStore ports
  bankedStore.io.sinkC_adr <> sinkC.io.bs_adr
  bankedStore.io.sinkC_dat := sinkC.io.bs_dat
  bankedStore.io.sinkD_adr <> sinkD.io.bs_adr
  bankedStore.io.sinkD_dat := sinkD.io.bs_dat
  bankedStore.io.sourceC_adr <> sourceC.io.bs_adr
  bankedStore.io.sourceD_radr <> sourceD.io.bs_radr
  bankedStore.io.sourceD_wadr <> sourceD.io.bs_wadr
  bankedStore.io.sourceD_wdat := sourceD.io.bs_wdat
  sourceC.io.bs_dat := bankedStore.io.sourceC_dat
  sourceD.io.bs_rdat := bankedStore.io.sourceD_rdat

  // SourceD data hazard interlock
  sourceD.io.evict_req := sourceC.io.evict_req
  sourceD.io.grant_req := sinkD  .io.grant_req
  sourceC.io.evict_safe := sourceD.io.evict_safe
  sinkD  .io.grant_safe := sourceD.io.grant_safe

  // ========================================================================
  // L2-to-RAM Prefetcher Connections
  // ========================================================================
  // PRIMARY: Connect prefetcher to observe L1->L2 requests (sinkA) for stride detection
  // This is the KEY improvement - detect patterns from actual CPU accesses!
  // Reconstruct address from tag, set, offset components
  val l1_req_address = params.expandAddress(
    sinkA.io.req.bits.tag,
    sinkA.io.req.bits.set,
    sinkA.io.req.bits.offset
  )
  prefetcher.io.l1_req_valid := sinkA.io.req.valid
  prefetcher.io.l1_req_address := l1_req_address
  prefetcher.io.l1_req_opcode := sinkA.io.req.bits.opcode

  // SECONDARY: Connect prefetcher to observe L2 misses (sourceA) to trigger prefetches
  prefetcher.io.snoop_valid := sourceA.io.snoop_valid
  prefetcher.io.snoop_address := sourceA.io.snoop_address
  prefetcher.io.snoop_opcode := sourceA.io.snoop_opcode

  // Connect prefetcher to observe incoming Grant responses (for stride detection)
  prefetcher.io.grant_valid := sinkD.io.resp.valid
  prefetcher.io.grant_source := sinkD.io.resp.bits.source

  // STREAM BUFFER: Decouple prefetch storage from MSHRs
  // Allocation: 16 entries, starting at source ID = params.mshrs (after MSHR B/C)
  val streamBuffer = Module(new StreamBuffer(params, params.micro.streamBufferEntries, params.mshrs))
  
  prefetcher.io.can_prefetch := true.B // Always allow prefetch generation
  streamBuffer.io.alloc.valid := prefetcher.io.prefetch.valid
  streamBuffer.io.alloc.bits.address := prefetcher.io.prefetch.bits.address
  prefetcher.io.prefetch.ready := streamBuffer.io.alloc.ready

  // INVALIDATION CONNECTION:
  // Hook up L2 evictions (SourceC) to invalidation port.
  // When L2 evicts a block (Release) or writes back (ProbeAckData),
  // we must invalidate any matching entry in StreamBuffer to prevent stale data.
  // sourceC.io.c is Decoupled, so use .fire and .bits
  streamBuffer.io.inval_valid := io.out.c.fire
  streamBuffer.io.inval_addr  := io.out.c.bits.address

  // 1. SourceA Arbitration (MSHR vs StreamBuffer)
  // Round-robin for fair bandwidth sharing. For bandwidth-bound workloads like STREAM,
  // this ensures prefetches get issued, keeping the StreamBuffer populated.
  val sourceArb = Module(new RRArbiter(new TLBundleA(params.outer.bundle), 2))
  sourceArb.io.in(0) <> sourceA.io.a       // MSHR requests
  sourceArb.io.in(1) <> streamBuffer.io.req // StreamBuffer requests
  io.out.a <> sourceArb.io.out

  // 2. SinkD Demux (MSHR vs StreamBuffer)
  // Determine if response is for StreamBuffer (high IDs)
  val isStreamBufferResp = io.out.d.bits.source >= params.mshrs.U
  
  streamBuffer.io.resp.valid := io.out.d.valid && isStreamBufferResp
  streamBuffer.io.resp.bits := io.out.d.bits
  
  // SinkD comes from EITHER io.out.d (if MSHR) OR StreamBuffer.io.replay_d (if Hit)
  // CRITICAL: We must use a LOCKED arbiter to prevent interleaving beats of different bursts.
  val sinkDArb = Module(new RRArbiter(new TLBundleD(params.outer.bundle), 2))
  
  // Input sources
  val flow_d_valid = io.out.d.valid && !isStreamBufferResp
  val flow_s_valid = streamBuffer.io.replay_d.valid
  
  // Locking logic (Robustified)
  // We must LOCK onto a source until the burst is complete (out_last).
  // This prevents switching mid-burst if validity drops ("bubble").
  val locked = RegInit(false.B)
  val locked_choice = Reg(UInt(1.W)) // 0=Memory, 1=StreamBuffer

  val arb_choice = sinkDArb.io.chosen
  val (out_first, out_last, _, out_beat) = params.outer.count(sinkDArb.io.out)

  when (!locked && sinkDArb.io.out.fire) {
    // Start of new burst
    when (!out_last) {
      locked := true.B
      locked_choice := arb_choice
      if (!params.lastLevel) {
         printf("[SINKD LOCK] Locking on %d (0=Mem, 1=SB)\n", arb_choice)
      }
    }
  }

  when (locked && sinkDArb.io.out.fire && out_last) {
    locked := false.B
    if (!params.lastLevel) {
       // printf("[SINKD LOCK] Releasing lock\n")
    }
  }

  // FORCE arbitration choice when locked
  // If locked, we MASK valid of the other source to 0.
  // This forces RRArbiter to pick the locked source (if it has valid data) or wait.
  // It prevents switching to the other source.
  val mask_d = locked && locked_choice === 1.U // Locked on SB -> Mask Mem
  val mask_s = locked && locked_choice === 0.U // Locked on Mem -> Mask SB

  sinkDArb.io.in(0).valid := flow_d_valid && !mask_d
  sinkDArb.io.in(0).bits  := io.out.d.bits
  sinkDArb.io.in(1).valid := flow_s_valid && !mask_s
  sinkDArb.io.in(1).bits  <> streamBuffer.io.replay_d.bits
  streamBuffer.io.replay_d.ready := sinkDArb.io.in(1).ready && !mask_s

  // Ready logic:
  // Downstream ready comes from sinkDArb (which handles routing).
  // Logic for upstream `io.out.d.ready`:
  // If isStreamBufferResp -> Direct connection (always true)
  // Else (MSHR Resp) -> Check arbiter input 0.
  io.out.d.ready := Mux(isStreamBufferResp, streamBuffer.io.resp.ready, sinkDArb.io.in(0).ready && !mask_d)
  
  sinkD.io.d <> sinkDArb.io.out
  // Tell SinkD if this response came from Stream Buffer (arbiter input 1)
  // Use locked status if locked, otherwise check current arbiter choice
  sinkD.io.from_stream_buffer := (locked && locked_choice === 1.U) || (!locked && sinkDArb.io.chosen === 1.U && sinkDArb.io.out.fire)

  // 3. Stream Buffer Hit Logic (Bypass SourceA)
  // FIX: SourceARequest does not have 'address'. It has tag/set.
  // We need to reconstruct.
  streamBuffer.io.peek_addr := params.expandAddress(schedule.a.bits.tag, schedule.a.bits.set, 0.U)
  
  // We only peek for AcquireBlock/Perm (not Put, etc - though MSHR A is usually Acquire)
  // FIX: opcode is in 'bits' of SourceARequest? No, SourceARequest (line 24 in SourceA.scala) DOES NOT have opcode or address.
  // It has 'tag', 'set', 'param', 'source', 'block'.
  // We need to reconstruct address or check tag/set directly.
  // We need to infer opcode from 'block' bit?
  // SourceA.scala:53: a.bits.opcode := Mux(io.req.bits.block, TLMessages.AcquireBlock, TLMessages.AcquirePerm)
  val sche_s_req = schedule.a.bits
  val can_hit_stream = schedule.a.valid && (sche_s_req.block || !sche_s_req.block) // All SourceA requests are Acquires in this design
  
  // RACE CONDITION FIX: Only allow stream_hit when we're CERTAIN replay can proceed.
  // Issue: `locked` is a register (previous cycle's value). The arbiter might be starting
  // a Memory burst in THIS cycle, making the lock stale. By the time replay fires, it's masked.
  // Solution: Only hit when NO Memory data is pending at the arbiter input.
  // If Memory (input 0) has no valid data, there's nothing to lock on, so replay is safe.
  val memory_data_pending = sinkDArb.io.in(0).valid // Memory has data waiting
  val can_use_stream_buffer = !locked && !memory_data_pending
  val stream_hit = can_hit_stream && streamBuffer.io.peek_hit && can_use_stream_buffer
  stream_hit_wire := stream_hit
  
  when (stream_hit) {
    printf("[STREAM HIT] Address 0x%x matched in StreamBuffer. Suppressing SourceA. SourceID=%d\n", streamBuffer.io.peek_addr, schedule.a.bits.source)
  }

  // Pop StreamBuffer immediately on hit
  streamBuffer.io.pop_valid := stream_hit
  streamBuffer.io.pop_source := schedule.a.bits.source
  
  // 4. SourceE Arbitration (MSHR GrantAcks vs StreamBuffer GrantAcks)
  val sourceEArb = Module(new RRArbiter(new TLBundleE(params.outer.bundle), 2))
  sourceEArb.io.in(0) <> sourceE.io.e       // MSHR GrantAcks
  sourceEArb.io.in(1) <> streamBuffer.io.ack_e  // StreamBuffer GrantAcks
  io.out.e <> sourceEArb.io.out

  // L2 directory hit / miss
  io.counters.l2_hit  := mshrs.map(_.io.counters.l2_hit).reduce(_||_)
  io.counters.l2_miss := mshrs.map(_.io.counters.l2_miss).reduce(_||_)

  // MSHR allocations (primary L2 misses)
  io.counters.mshr_alloc := request.valid && request_alloc_cases

  // L2 prefetcher performance: issued vs used (via Stream Buffer)
  io.counters.pf_issued := prefetcher.io.prefetch_issued
  io.counters.pf_used   := stream_hit

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
  def json: String = s"""{"addresses":[${addresses}],${simple},"directory":${directory.json},"subbanks":${bankedStore.json}}"""
}
