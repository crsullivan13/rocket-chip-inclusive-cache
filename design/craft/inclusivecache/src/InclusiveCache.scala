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

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.subsystem._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

import freechips.rocketchip.tile._
import freechips.rocketchip.util._

class InclusiveCache(
  val cache: CacheParameters,
  val micro: InclusiveCacheMicroParameters,
  control: Option[InclusiveCacheControlParameters] = None
  )(implicit p: Parameters)
    extends LazyModule
{
  val access = TransferSizes(1, cache.blockBytes)
  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)

  var resourcesOpt: Option[ResourceBindings] = None

  val dramActAcquire = BundleBridgeSource(() => new ActiveAcquire(cache.nRCID, cache.nMCID, node.edges.out.size, node.edges.out.head.bundle.addressBits))

  // Way partitioning registers, shared by all banks
  val waypart = LazyModule(new InclusiveCacheWayPart(cache.nRCID, cache.ways,
    InclusiveCacheParameters.all_mshrs(cache, micro), InclusiveCacheParameters.WayPartAddress))

  val device: SimpleDevice = new SimpleDevice("cache-controller", Seq("sifive,inclusivecache0", "cache")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

    override def describe(resources: ResourceBindings): Description = {
      resourcesOpt = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      // Find the outer caches
      val outer = node.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) {
          None
        } else {
          Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
        }

      val extra = Map(
        "cache-level"            -> ofInt(2),
        "cache-unified"          -> Nil,
        "cache-size"             -> ofInt(cache.sizeBytes * node.edges.in.size),
        "cache-sets"             -> ofInt(cache.sets * node.edges.in.size),
        "cache-block-size"       -> ofInt(cache.blockBytes),
        "sifive,mshr-count"      -> ofInt(InclusiveCacheParameters.all_mshrs(cache, micro)))
      Description(name, mapping ++ extra ++ nextlevel)
    }
  }

  val node: TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"L${cache.level} InclusiveCache",
      sourceId      = IdRange(0, InclusiveCacheParameters.out_mshrs(cache, micro)),
      supportsProbe = xfer)),
      requestFields = Seq(CBQRIFields()))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        resources          = Resource(device, "caches") +: m.resources,
        supportsAcquireB   = xfer,
        supportsAcquireT   = if (m.supportsAcquireT) xfer else TransferSizes.none,
        supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsLogical    = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsGet        = access,
        supportsPutFull    = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsHint       = access,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = cache.beatBytes,
      endSinkId  = InclusiveCacheParameters.all_mshrs(cache, micro),
      minLatency = 2,
      requestKeys = Seq(CBQRIKey))
    })

  val ctrls = control.map { c =>
    val nCtrls = if (c.bankedControl) p(SubsystemBankedCoherenceKey).nBanks else 1
    Seq.tabulate(nCtrls) { i => LazyModule(new InclusiveCacheControl(this,
      c.copy(address = c.address + i * InclusiveCacheParameters.L2ControlSize))) }
  }.getOrElse(Nil)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    // If you have a control port, you must have at least one cache port
    require (ctrls.isEmpty || !node.edges.in.isEmpty)

    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name} sourceId=[${c.sourceId.start}, ${c.sourceId.end}]")
      }
      println("")
    }

    val nRCID = cache.nRCID

    // Create the L2 Banks
    val mods = (node.in zip node.out).zipWithIndex map { case (((in, edgeIn), (out, edgeOut)), i) =>
      edgeOut.manager.managers.foreach { m =>
        require (m.supportsAcquireB.contains(xfer),
          s"All managers behind the L2 must support acquireB($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireB})!")
        if (m.supportsAcquireT) require (m.supportsAcquireT.contains(xfer),
          s"Any probing managers behind the L2 must support acquireT($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireT})!")
      }

      val params = InclusiveCacheParameters(cache, micro, !ctrls.isEmpty, edgeIn, edgeOut)
      val scheduler = Module(new InclusiveCacheBankScheduler(params, nRCID)).suggestName("inclusive_cache_bank_sched")

      scheduler.io.in <> in
      out <> scheduler.io.out
      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

      // Tie down default values in case there is no controller
      scheduler.io.req.valid := false.B
      scheduler.io.req.bits.address := 0.U
      scheduler.io.req.bits.invalidate := false.B
      scheduler.io.resp.ready := true.B

      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.
      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      scheduler
    }

    mods.foreach { sched =>
      sched.io.wayPartMasks := waypart.module.io.wayPartMasks
      sched.io.mshrCapMask := waypart.module.io.mshrCapMask
    }

    val llcReads = WireInit(VecInit(Seq.fill(mods.length)(false.B)))
    val llcRels = WireInit(VecInit(Seq.fill(mods.length)(false.B)))
    val llcProbeAcks = WireInit(VecInit(Seq.fill(mods.length)(false.B)))
    val llcPuts = WireInit(VecInit(Seq.fill(mods.length)(false.B)))
    mods.zip(node.edges.in).zipWithIndex.foreach{ case ((sched, edgeIn), i) =>
      val isFirstC = edgeIn.first(sched.io.in.c)
      val isFirstA = edgeIn.first(sched.io.in.a)
      llcReads(i) := sched.io.in.a.fire && sched.io.in.a.bits.opcode.isOneOf(TLMessages.AcquireBlock, TLMessages.Get)
      llcRels(i) := sched.io.in.c.fire && sched.io.in.c.bits.opcode === TLMessages.ReleaseData && isFirstC
      llcProbeAcks(i) := sched.io.in.c.fire && sched.io.in.c.bits.opcode === TLMessages.ProbeAckData && isFirstC
      llcPuts(i) := sched.io.in.a.fire && sched.io.in.a.bits.opcode.isOneOf(TLMessages.PutFullData, TLMessages.PutPartialData) && isFirstA
    }

    mods.indices.foreach{ i =>
      midas.targetutils.PerfCounter(llcReads(i), s"llc_bank${i}_reads", s"LLC bank ${i} reads")
      midas.targetutils.PerfCounter(llcRels(i), s"llc_bank${i}_releases", s"LLC bank ${i} releases")
      midas.targetutils.PerfCounter(llcProbeAcks(i), s"llc_bank${i}_probe_acks", s"LLC bank ${i} probe acks")
      midas.targetutils.PerfCounter(llcPuts(i), s"llc_bank${i}_puts", s"LLC bank ${i} puts")
    }

    // Per-RCID in/out reads/writes summed across banks
    for ( r <- 0 until nRCID ) {
      val rcidAccs = mods.map( sched => sched.io.in.a.fire && sched.io.in.a.bits.opcode.isOneOf(TLMessages.AcquireBlock, TLMessages.Get) && sched.io.in.a.bits.user.lift(CBQRIKey).map(_.rcid).getOrElse(0.U) === r.U )
      val rcidMisses = mods.map( sched => sched.io.out.a.fire && sched.io.out.a.bits.opcode.isOneOf(TLMessages.AcquireBlock, TLMessages.Get) && sched.io.out.a.bits.user.lift(CBQRIKey).map(_.rcid).getOrElse(0.U) === r.U )
      midas.targetutils.PerfCounter(PopCount(rcidAccs), s"llc_rcid${r}_reads", s"LLC AcquireBlock/Get accesses from RCID ${r}")
      midas.targetutils.PerfCounter(PopCount(rcidMisses), s"llc_rcid${r}_read_misses", s"LLC AcquireBlock/Get sent to DRAM for RCID ${r}")

      val rcidWrIn = mods.zip(node.edges.in).map{ case (sched, edgeIn) =>
        sched.io.in.c.fire && sched.io.in.c.bits.opcode.isOneOf(TLMessages.ReleaseData, TLMessages.ProbeAckData) && edgeIn.first(sched.io.in.c) && sched.io.in.c.bits.user.lift(CBQRIKey).map(_.rcid).getOrElse(0.U) === r.U
      }
      val rcidWrOut = mods.zip(node.edges.out).map{ case (sched, edgeOut) =>
        sched.io.out.c.fire && sched.io.out.c.bits.opcode === TLMessages.ReleaseData && edgeOut.first(sched.io.out.c) && sched.io.out.c.bits.user.lift(CBQRIKey).map(_.rcid).getOrElse(0.U) === r.U
      }
      midas.targetutils.PerfCounter(PopCount(rcidWrIn), s"llc_rcid${r}_writes", s"LLC ReleaseData/ProbeAckData from RCID ${r}")
      midas.targetutils.PerfCounter(PopCount(rcidWrOut), s"llc_rcid${r}_writebacks", s"LLC ReleaseData to DRAM attributed to RCID ${r}")
    }

    // Assumption that only one bank fires in a given cycle
    val firesAcquireBlockOrGet = mods.map(sched => sched.io.out.a.fire && sched.io.out.a.bits.opcode.isOneOf(TLMessages.AcquireBlock, TLMessages.Get))
    assert(PopCount(firesAcquireBlockOrGet) <= 1.U)

    // autocounter for outstanding reads
    val outstanding = RegInit(0.U(16.W))
    val recvsGrantData = mods.zip(node.edges.out).map{ case (sched, edgeOut) =>
      sched.io.out.d.fire && sched.io.out.d.bits.opcode === TLMessages.GrantData && edgeOut.first(sched.io.out.d)
    }
    outstanding := outstanding + PopCount(firesAcquireBlockOrGet) - PopCount(recvsGrantData)
    midas.targetutils.PerfCounter.identity(outstanding, "outstanding_llc_reads_to_dram", "Reads fired from LLC to DRAM, awaiting response")
    // end autocounter for outstanding reads

    // out.a has the restored (full) address
    (mods zip node.out).zipWithIndex.foreach { case ((sched, (out, _)), i) =>
      dramActAcquire.bundle.reads(i).valid := firesAcquireBlockOrGet(i)
      dramActAcquire.bundle.reads(i).bits.rcid := sched.io.out.a.bits.user(CBQRIKey).rcid
      dramActAcquire.bundle.reads(i).bits.mcid := sched.io.out.a.bits.user(CBQRIKey).mcid
      dramActAcquire.bundle.reads(i).bits.address := out.a.bits.address
    }

    ctrls.foreach { ctrl =>
      ctrl.module.io.flush_req.ready := false.B
      ctrl.module.io.flush_resp := false.B
      ctrl.module.io.flush_match := false.B
    }

    mods.zip(node.edges.in).zipWithIndex.foreach { case ((sched, edgeIn), i) =>
      val ctrl = if (ctrls.size > 1) Some(ctrls(i)) else ctrls.headOption
      ctrl.foreach { ctrl => {
        val contained = edgeIn.manager.managers.flatMap(_.address)
          .map(_.contains(ctrl.module.io.flush_req.bits)).reduce(_||_)
        when (contained) { ctrl.module.io.flush_match := true.B }

        sched.io.req.valid := contained && ctrl.module.io.flush_req.valid
        sched.io.req.bits.address := ctrl.module.io.flush_req.bits
        sched.io.req.bits.invalidate := ctrl.module.io.invalidate_req
        when (contained && sched.io.req.ready) { ctrl.module.io.flush_req.ready := true.B }

        when (sched.io.resp.valid) { ctrl.module.io.flush_resp := true.B }
        sched.io.resp.ready := true.B
      }}
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
