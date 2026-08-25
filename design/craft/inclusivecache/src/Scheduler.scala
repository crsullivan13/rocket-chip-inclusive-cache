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
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import chisel3.experimental.dataview._

class InclusiveCacheBankScheduler(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val in = Flipped(TLBundle(params.inner.bundle))
    val out = TLBundle(params.outer.bundle)
    // Way permissions
    val ways = Flipped(Vec(params.allClients, UInt(params.cache.ways.W)))
    val divs = Flipped(Vec(params.allClients, UInt((InclusiveCacheParameters.lfsrBits + 1).W)))
    // Control port
    val req = Flipped(Decoupled(new SinkXRequest(params)))
    val resp = Decoupled(new SourceXRequest(params))
  })

  val sourceA = Module(new SourceA(params))
  val sourceB = Module(new SourceB(params))
  val sourceC = Module(new SourceC(params))
  val sourceD = Module(new SourceD(params))
  val sourceE = Module(new SourceE(params))
  val sourceX = Module(new SourceX(params))

  io.out.a <> sourceA.io.a
  io.out.c <> sourceC.io.c
  io.out.e <> sourceE.io.e
  io.in.b <> sourceB.io.b
  io.in.d <> sourceD.io.d
  io.resp <> sourceX.io.x

  val sinkA = Module(new SinkA(params))
  val sinkC = Module(new SinkC(params))
  val sinkD = Module(new SinkD(params))
  val sinkE = Module(new SinkE(params))
  val sinkX = Module(new SinkX(params))

  sinkA.io.a <> io.in.a
  sinkC.io.c <> io.in.c
  sinkE.io.e <> io.in.e
  sinkD.io.d <> io.out.d
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
    // Phase 4: under line granularity more than one MSHR can share a set, so a ProbeAck must
    // also be tag-qualified before it's allowed to update an MSHR's probe FSM (probes_done /
    // w_[rp]probeack*) -- otherwise a same-set MSHR that is not the actual probe target could
    // spuriously observe the ack (see docs/scheduler-mshr-analysis.md §7.3, "ProbeAck
    // disambiguation": the old set-only gate was sound only because at most one MSHR could
    // ever own a set at all). probeTag was added in Phase 1 and is already proven safe to
    // compare even pre-metaValid -- see the Phase 3 comment on probeOH below, same argument
    // applies here. Structurally a no-op when lineGranularMSHR is false: at most one MSHR can
    // match on set alone in that configuration, and its probeTag is provably the ack's tag.
    m.io.sinkc.valid := m.io.status.valid && sinkC.io.resp.valid && sinkC.io.resp.bits.set === m.io.status.bits.set &&
                        (!params.micro.lineGranularMSHR.B || sinkC.io.resp.bits.tag === m.io.status.bits.probeTag)
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
  schedule.c.bits.source := Mux(schedule.c.bits.opcode(1), mshr_select, 0.U) // only set for Release[Data] not ProbeAck[Data]
  schedule.d.bits.sink   := mshr_select

  sourceA.io.req.valid := schedule.a.valid
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
  // Only a B-priority transaction may broadcast a b_* metadata side effect. require(lastLevel)
  // makes outer probes impossible, so nestB never fires and bc_mshr is reachable only as
  // ordinary overflow capacity for C-priority requests (prioFilter bit mshrs-2 is !prio(0),
  // so mshr_insertOH may land there once every abc MSHR is busy). Broadcasting b_clr_dirty
  // for such a Release would clear dirty in any MSHR matching (set, tag) -- today only
  // bc_mshr itself, and only on the cycle it retires, so every consumer has already sampled
  // the pre-update meta. Harmless, but only by that margin, and set-exclusivity is exactly
  // what line-granular allocation removes. Gate the path off where it cannot legitimately fire.
  //
  val nestedwb_b = (!params.lastLevel).B && select_bc && bc_mshr.io.schedule.bits.dir.valid
  nestedwb.b_toN       := nestedwb_b && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.INVALID
  nestedwb.b_toB       := nestedwb_b && bc_mshr.io.schedule.bits.dir.bits.data.state === MetaData.BRANCH
  nestedwb.b_clr_dirty := nestedwb_b
  nestedwb.c_set_dirty := select_c  &&  c_mshr.io.schedule.bits.dir.valid && c_mshr.io.schedule.bits.dir.bits.data.dirty

  // Pick highest priority request
  val request = Wire(Decoupled(new FullRequest(params)))
  request.valid := directory.io.ready && (sinkA.io.req.valid || sinkX.io.req.valid || sinkC.io.req.valid)
  request.bits := Mux(sinkC.io.req.valid, sinkC.io.req.bits,
                  Mux(sinkX.io.req.valid, sinkX.io.req.bits, sinkA.io.req.bits))
  sinkC.io.req.ready := directory.io.ready && request.ready
  sinkX.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid
  sinkA.io.req.ready := directory.io.ready && request.ready && !sinkC.io.req.valid && !sinkX.io.req.valid

  // Phase 4 (line-granular MSHR allocation): sameSetOH is the raw, tag-oblivious CAM -- every
  // live MSHR sharing this request's set, regardless of which line it owns. It is still
  // needed below for way-exclusion (a genuinely per-*set* resource) and for the Phase 0
  // false-conflict coverage point; it is exactly the old setMatches.
  val sameSetOH = Cat(mshrs.map(m => m.io.status.valid && m.io.status.bits.set === request.bits.set).reverse)

  // lineMatches is the ownership CAM that actually gates alloc/queue/block/nest: the set of
  // MSHRs that own the requested *line* -- same tag, or the victim tag of an MSHR mid-eviction
  // (docs/line-granular-mshr-plan.md §2, ownsLine). It replaces setMatches everywhere setMatches
  // used to mean "the MSHR I must serialize behind", and degrades to sameSetOH when
  // lineGranularMSHR is false, making this whole block a no-op transform of the original logic
  // in that configuration.
  val lineMatches = Cat(mshrs.map { m => m.io.status.valid && params.ownsLine(m.io.status.bits, request.bits.set, request.bits.tag) }.reverse)
  val alloc = !lineMatches.orR // NOTE: no matches also means no BC or C pre-emption on this line
  // If a same-line MSHR says that requests of this type must be blocked (for bounded time), do it
  val blockB = Mux1H(lineMatches, mshrs.map(_.io.status.bits.blockB)) && request.bits.prio(1)
  val blockC = Mux1H(lineMatches, mshrs.map(_.io.status.bits.blockC)) && request.bits.prio(2)
  // If a same-line MSHR says that requests of this type must be handled out-of-band, use special BC|C MSHR
  // ... these special MSHRs interlock the MSHR that said it should be pre-empted.
  val nestB  = Mux1H(lineMatches, mshrs.map(_.io.status.bits.nestB))  && request.bits.prio(1)
  val nestC  = Mux1H(lineMatches, mshrs.map(_.io.status.bits.nestC))  && request.bits.prio(2)
  // Prevent priority inversion; we may not queue to MSHRs beyond our level
  val prioFilter = Cat(request.bits.prio(2), !request.bits.prio(0), ~0.U((params.mshrs-2).W))
  val lowerMatches = lineMatches & prioFilter
  // If we match an MSHR <= our priority that neither blocks nor nests us, queue to it.
  val queue = lowerMatches.orR && !nestB && !nestC && !blockB && !blockC

  // Allocation gating (H1/H2/H6, plan §4.4): a fresh MSHR that will need a victim way must not
  // race another same-set MSHR for that way, and must not be allocated at all while another
  // same-set MSHR hasn't yet learned its own way (it could end up claiming any of them). This
  // unknownWay term is also exactly what covers the §2 requirement that an MSHR still be
  // treated as owning its whole set, for allocation purposes, before its directory result
  // returns: ownsLine itself deliberately does NOT special-case !metaValid (see the comment on
  // ownsLine in Parameters.scala) -- unknownWay is the mechanism that actually blocks forward
  // progress during that window, not lineMatches.
  val unknownWay = (sameSetOH & Cat(mshrs.map(!_.io.status.bits.metaValid).reverse)).orR
  val busyWays = mshrs.zipWithIndex.map { case (m, i) =>
    Mux(sameSetOH(i), UIntToOH(m.io.status.bits.way, params.cache.ways), 0.U) }.reduce(_|_)

  // Only a real A-channel request can miss and therefore need a victim way. A Release always
  // hits a resident line (MSHR.scala asserts new_meta.hit for prio(2)) and a flush that misses
  // does nothing, so B/C are exempt -- channel C must never be blocked on a resource that only
  // A-channel progress can free.
  val needsVictim = request.bits.prio(0) && !request.bits.control.flush
  val wayBlocked  = needsVictim && (unknownWay || busyWays.andR)

  // MUST-RESOLVE (plan §4.4/§7.1 discussion, resolved): alloc and queue must remain mutually
  // exclusive now that both are derived from lineMatches instead of the old setMatches. They
  // are, by construction -- alloc requires lineMatches empty, queue requires lowerMatches (a
  // subset of lineMatches) nonempty -- exactly mirroring the old setMatches-based exclusivity,
  // just at line granularity. The case that used to make this trivial (a single shared CAM)
  // is also the case most likely to silently break it if a future edit re-derives queue from
  // sameSetOH by mistake (a same-set-different-line request would then be both freshly
  // allocated AND pushed into another MSHR's secondary queue in the same cycle -- the same
  // request processed twice). Tripwire it.
  assert (!(request.valid && alloc && queue), "alloc and queue must be mutually exclusive")

  if (!params.lastLevel) {
    params.ccover(request.valid && blockB, "SCHEDULER_BLOCKB", "Interlock B request while resolving set conflict")
    params.ccover(request.valid && nestB,  "SCHEDULER_NESTB", "Priority escalation from channel B")
  }
  if (!params.firstLevel) {
    params.ccover(request.valid && blockC, "SCHEDULER_BLOCKC", "Interlock C request while resolving set conflict")
    params.ccover(request.valid && nestC,  "SCHEDULER_NESTC", "Priority escalation from channel C")
  }
  params.ccover(request.valid && queue, "SCHEDULER_SECONDARY", "Enqueue secondary miss")

  // Phase 0 instrumentation (line-granular MSHR project): of the requests that sameSetOH
  // (formerly setMatches) would have serialized under the old scheme, how many conflict only
  // on the set index and not on the line? That fraction is exactly what line-granular
  // allocation recovers, whether or not lineGranularMSHR is presently enabled -- this remains
  // a fixed measurement of set-aliasing rate in the traffic, independent of what the scheduler
  // now does as a result of it. Note this counts cycles, not requests -- request.valid is held
  // while request.ready is low -- so it reads as "cycles of false serialization", directly
  // comparable to SCHEDULER_SECONDARY, which has the same shape.
  val trueLineMatch = mshrs.map { m =>
    m.io.status.valid &&
    m.io.status.bits.set === request.bits.set &&
    m.io.status.bits.tag === request.bits.tag
  }.reduce(_||_)
  params.ccover(request.valid && sameSetOH.orR && !trueLineMatch,
                "SCHEDULER_FALSE_CONFLICT",
                "Request serialized behind an MSHR holding a different line in the same set")

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
     (alloc && !wayBlocked && !mshr_uses_directory_assuming_no_bypass && mshr_free) ||
     (nestB && !mshr_uses_directory_assuming_no_bypass && !bc_mshr.io.status.valid && !c_mshr.io.status.valid) ||
     (nestC && !mshr_uses_directory_assuming_no_bypass && !c_mshr.io.status.valid)
  request.ready := request_alloc_cases || (queue && (bypassQueue || requests.io.push.ready))
  val alloc_uses_directory = request.valid && request_alloc_cases

  // When a request goes through, it will need to hit the Directory
  directory.io.read.valid := mshr_uses_directory || alloc_uses_directory
  val dirReadSet = Mux(mshr_uses_directory_for_lb, scheduleSet, request.bits.set)
  directory.io.read.bits.set := dirReadSet
  directory.io.read.bits.tag := Mux(mshr_uses_directory_for_lb, requests.io.data.tag, request.bits.tag)

  // Phase 4: drive the real way mask (§4.4). dirSameSetOH mirrors sameSetOH but against
  // whichever set is actually being read this cycle -- a fresh allocation reads
  // request.bits.set, an MSHR reloading a mismatched secondary miss reads scheduleSet -- minus
  // the reloading MSHR's own entry: it is releasing that way as it retires, so it may reuse it,
  // and the reload case is therefore guaranteed at least one free way.
  //
  // Unlike the allocation-side wayBlocked gate, this does not special-case unknownWay. A
  // reloading MSHR's directory read (mshr_uses_directory_for_lb) is not itself gated the way a
  // fresh alloc is -- it must proceed on the cycle the MSHR retires, all-or-nothing scheduling
  // (§5.1 of the analysis doc) leaves no room to stall it -- so if another same-set MSHR is
  // concurrently mid-resolution (!metaValid) at that moment, its `way` field here is stale
  // (last transaction's way, or undefined if never used) rather than a true reservation. Ruling
  // this race out by static reasoning alone was not possible in the time available: it
  // requires a fresh MSHR (allocated when the set had no unresolved neighbor, satisfying its
  // own wayBlocked check) whose directory result is still in flight while an unrelated,
  // already-resolved same-set MSHR independently reaches lb_tag_mismatch reload -- both are
  // individually reachable and nothing in the current design prevents them overlapping. Left
  // as-is per the plan's given formula; the physical-slot-uniqueness assertion below is the
  // backstop that will catch it in sim if it is ever actually hit, rather than corrupting data
  // silently.
  val dirSameSetOH = Cat(mshrs.map(m => m.io.status.valid && m.io.status.bits.set === dirReadSet).reverse) &
                     ~Mux(mshr_uses_directory_for_lb, mshr_selectOH, 0.U)
  val dirBusyWays = mshrs.zipWithIndex.map { case (m, i) =>
    Mux(dirSameSetOH(i), UIntToOH(m.io.status.bits.way, params.cache.ways), 0.U) }.reduce(_|_)
  directory.io.read.bits.wayMask := ~dirBusyWays

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
    // wayBlocked must gate the actual allocation here, not just request_alloc_cases /
    // request.ready above -- mshr_insertOH's zero-ness already accounts for mshr_free, but has
    // no notion of way availability, so without this term an MSHR would be allocated on a
    // cycle request.ready was actually held low for wayBlocked.
    when (request.valid && alloc && !wayBlocked && s && !mshr_uses_directory_assuming_no_bypass) {
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

  // MSHR response meta-data fetch (Phase 3: tag-qualified so the way CAM is unique per line,
  // not merely per set). probeTag is safe to compare here even for an MSHR whose directory
  // read has not yet returned (metaValid == false): probeTag = Mux(!w_rprobeacklast, meta.tag,
  // request.tag), and w_rprobeacklast can only be driven false in the same cycle meta_valid is
  // driven true (MSHR.scala's "Create execution plan" block), so !meta_valid implies
  // w_rprobeacklast === true, which forces probeTag to read the legitimately-loaded
  // request.tag rather than the stale meta.tag register. So no live ProbeAck can ever be
  // compared against a garbage probeTag.
  val probeOH = mshrs.map(m => m.io.status.valid &&
                              sinkC.io.set === m.io.status.bits.set &&
                              sinkC.io.tag === m.io.status.bits.probeTag)
  sinkC.io.way := Mux1H(probeOH, mshrs.map(_.io.status.bits.way))
  // Use the undelayed camValid (not the Queue-registered bs_adr.valid) so this check is
  // evaluated at the same cycle 'way' was actually latched, not one cycle later against
  // MSHR state (e.g. probeTag) that may have since legitimately advanced.
  assert (!sinkC.io.camValid || PopCount(probeOH) === 1.U)

  // §7.1 invariants (plan doc): no two MSHRs may ever own the same line, and no two MSHRs may
  // ever own the same physical (set, way) slot. Both are checked against every valid MSHR's
  // own (set, tag) / (set, way) as the reference point, so they exhaustively cover all pairs,
  // not just the incoming request's line. Both hold unconditionally, including with
  // lineGranularMSHR false -- where the first degrades to restating the old one-MSHR-per-set
  // invariant and the second is unaffected by the flag at all -- so they regress Phases 0-3 as
  // well as guard Phase 4.
  mshrs.foreach { m =>
    when (m.io.status.valid) {
      assert (PopCount(mshrs.map(o => o.io.status.valid &&
                                      params.ownsLine(o.io.status.bits, m.io.status.bits.set, m.io.status.bits.tag))) <= 1.U,
        "two MSHRs own the same line")
    }
    when (m.io.status.valid && m.io.status.bits.metaValid) {
      assert (PopCount(mshrs.map(o => o.io.status.valid && o.io.status.bits.metaValid &&
                                      o.io.status.bits.set === m.io.status.bits.set &&
                                      o.io.status.bits.way === m.io.status.bits.way)) <= 1.U,
        "two MSHRs own the same physical (set, way) slot")
    }
  }

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

  private def afmt(x: AddressSet) = s"""{"base":${x.base},"mask":${x.mask}}"""
  private def addresses = params.inner.manager.managers.flatMap(_.address).map(afmt _).mkString(",")
  private def setBits = params.addressMapping.drop(params.offsetBits).take(params.setBits).mkString(",")
  private def tagBits = params.addressMapping.drop(params.offsetBits + params.setBits).take(params.tagBits).mkString(",")
  private def simple = s""""reset":"${reset.pathName}","tagBits":[${tagBits}],"setBits":[${setBits}],"blockBytes":${params.cache.blockBytes},"ways":${params.cache.ways}"""
  def json: String = s"""{"addresses":[${addresses}],${simple},"directory":${directory.json},"subbanks":${bankedStore.json}}"""
}
