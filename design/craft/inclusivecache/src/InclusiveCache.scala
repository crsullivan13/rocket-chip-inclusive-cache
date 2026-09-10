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

import midas.targetutils.SynthesizePrintf

class ThrottleBundle(nDramBanks: Int) extends Bundle {
  val dramBank = Vec(nDramBanks, Bool())
}

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

  // create our own register node for regulation, easier than using the control node since it could be per-bank
  // maybe move this later to reduce number of additions to code
  val regulationDevice = new SimpleDevice("dram-memory-reg",Seq("rsicv,cbqri-bandwidth-memory", "rsicv,cbqri-bandwidth")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

    override def describe(resources: ResourceBindings): Description = {
      resourcesOpt = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      val extra = Map(
        "riscv,cbqri-rcid" -> ofInt(cache.nRCID),
        "riscv,cbqri-mcid" -> ofInt(cache.nMCID))
      Description(name, mapping ++ extra)
    }
  }

  val cbqriParams = PerBankBwParams(0x21000000, cache.nRCID, cache.nMCID, cache.cbqriVer, cache.nbwblks, cache.rpfx, cache.pfx, cache.mrbwb)
  val mmio = LazyModule(new CBQRIBwController(regulationDevice, cbqriParams))

  // val dramRegNode = BundleBridgeSource(() => new BRUPerBankTileIO(4, 16)) // TODO make number of domains one parameter everywhere

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
      supportsProbe = xfer)))
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
      minLatency = 2)
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
    val nMCID = cache.nMCID
    val mrbwb = cache.mrbwb
    val nDramBanks = cache.nDramBanks
    val dramBankBits = log2Ceil(nDramBanks)
    val dramBankOffset = cache.dramBankOffset

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
      val scheduler = Module(new InclusiveCacheBankScheduler(params, nRCID, nDramBanks, dramBankOffset)).suggestName("inclusive_cache_bank_sched")

      when (in.a.fire && in.a.bits.mcid === 0.U) {
        printf("LLC in: mcid %d | source %d | opcode %d | address %d\n", in.a.bits.mcid, in.a.bits.source, in.a.bits.opcode, in.a.bits.address)
      }

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

    val cntrBits = 17
    val bankReadCntrs = Seq.fill(nRCID)(Reg(Vec(nDramBanks, UInt(cntrBits.W))))
    val maxReads = Reg(Vec(nRCID, UInt(16.W)))

    val periodCount = RegInit(0.U(25.W))
    val periodReset = Wire(Bool())

    val mcidCounters = RegInit(VecInit(Seq.fill(nMCID)(0.U(62.W))))
    val mcidEvts = RegInit(VecInit(Seq.fill(nMCID)(BcMonCtlEvent.NONE)))
    val s_mcid_hold :: s_mcid_count :: s_mcid_reset :: Nil = Enum(3)
    val mcidStates = RegInit(VecInit(Seq.fill(nMCID)(s_mcid_hold)))

    mmio.module.io.bc_mon_resp.valid := false.B
    mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.OK
    mmio.module.io.bc_mon_resp.bits.hasData := false.B
    mmio.module.io.bc_mon_resp.bits.data := 0.U
    when ( mmio.module.io.bc_mon_command.valid ) {
      val opEnum = mmio.module.io.bc_mon_command.bits.op
      val opValid = ( opEnum === BcCtlOp.CONFIG ) || ( opEnum === BcCtlOp.READ )
      val evtEnum = mmio.module.io.bc_mon_command.bits.event
      val evtValid = ( evtEnum === BcMonCtlEvent.NONE ) || ( evtEnum === BcMonCtlEvent.READ_WRITE ) ||
                  ( evtEnum === BcMonCtlEvent.READ_ONLY ) || ( evtEnum === BcMonCtlEvent.WRITE_ONLY )
      val mcid = mmio.module.io.bc_mon_command.bits.mcid
      val mcidValid = mcid < nMCID.U

      mmio.module.io.bc_mon_resp.valid := true.B
      when ( opValid && mcidValid && evtValid ) {
        switch ( opEnum ) {
          is ( BcCtlOp.CONFIG ) {
            when ( evtEnum =/= BcMonCtlEvent.NONE ) {
              mcidStates(mcid) := s_mcid_reset
            } .otherwise {
              mcidStates(mcid) := s_mcid_hold
            }
            mcidEvts(mcid) := evtEnum
            mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.OK
          }
          is ( BcCtlOp.READ ) {
            mmio.module.io.bc_mon_resp.bits.hasData := true.B
            mmio.module.io.bc_mon_resp.bits.data := mcidCounters(mcid)
            mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.OK
          }
        }
      } .elsewhen ( !mcidValid ) {
        mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.INVALID_MCID
      } .elsewhen ( !evtValid ) {
        mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.INVALID_EVT_ID
      }.otherwise {
        mmio.module.io.bc_mon_resp.bits.status := BcMonCtlStatus.INVALID_OP
      }
    }

    mmio.module.io.bc_alloc_ctl_resp.valid := false.B
    mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.OK
    mmio.module.io.bc_alloc_ctl_resp.bits.hasData := false.B
    mmio.module.io.bc_alloc_ctl_resp.bits.data := 0.U
    when ( mmio.module.io.bc_alloc_ctl_command.valid ) {
      val opEnum = mmio.module.io.bc_alloc_ctl_command.bits.op
      val opValid = ( opEnum === BcCtlOp.CONFIG ) || ( opEnum === BcCtlOp.READ )
      val rcid = mmio.module.io.bc_alloc_ctl_command.bits.rcid
      val rcidValid = rcid < nRCID.U
      
      val rbwb = mmio.module.io.bc_alloc_ctl_command.bits.rbwb

      mmio.module.io.bc_alloc_ctl_resp.valid := true.B
      when ( opValid && rcidValid ) {
        switch ( opEnum ) {
          is ( BcCtlOp.CONFIG ) {
            val rbwbValid = rbwb > 0.U && rbwb <= mrbwb.U
            when ( rbwbValid ) {
              maxReads(rcid) := rbwb
              mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.OK
            } .otherwise {
              mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.INVALID_BWB
            }
          }
          is ( BcCtlOp.READ ) {
            mmio.module.io.bc_alloc_ctl_resp.bits.hasData := true.B
            mmio.module.io.bc_alloc_ctl_resp.bits.data := maxReads(rcid)
            mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.OK
          }
        }
      } .elsewhen ( !rcidValid ) {
        mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.INVALID_RCID
      }.otherwise {
        mmio.module.io.bc_alloc_ctl_resp.bits.status := BcAllocCtlStatus.INVALID_OP
      }
    }

    periodReset := periodCount >= mmio.module.io.periodLen
    periodCount := Mux(periodReset || !mmio.module.io.enGlobal, 0.U, periodCount + 1.U)

    // Assumption that only one bank fires in a given cycle
    assert(PopCount(mods.map(_.io.out.a.fire)) <= 1.U)
    val fires = mods.map(_.io.out.a.fire)
    val fireAny = fires.reduce(_||_)
    val firedMcid = Mux1H(fires, mods.map(_.io.out.a.bits.mcid))
    val firedRcid = Mux1H(fires, mods.map(_.io.out.a.bits.rcid))
    val firedBank = Mux1H(fires, mods.map(sched => sched.io.out.a.bits.address(dramBankOffset + dramBankBits - 1, dramBankOffset)))

    for ( i <- 0 until nMCID ) {
      val didMCIDFireAcquire = fireAny && firedMcid === i.U

      when ( mcidStates(i) === s_mcid_count ) {
        mcidCounters(i) := didMCIDFireAcquire + mcidCounters(i)
      } .elsewhen ( mcidStates(i) === s_mcid_hold ) {
        mcidCounters(i) := mcidCounters(i)
      } .elsewhen ( mcidStates(i) === s_mcid_reset ) {
        mcidCounters(i) := 0.U
        mcidStates(i) := s_mcid_count
      }
    }

    for ( i <- 0 until nRCID ) {
      val didRCIDFireAcquire = fireAny && firedRcid === i.U

      for ( j <- 0 until nDramBanks ) {
        val didTargetBank = didRCIDFireAcquire && firedBank === j.U
        val cntrMSB = bankReadCntrs(i)(j)(cntrBits - 1)
        bankReadCntrs(i)(j) := Mux(periodReset || !mmio.module.io.enGlobal, (maxReads(i) -& 1.U) - didTargetBank, 
          Mux(!cntrMSB, bankReadCntrs(i)(j) - didTargetBank, bankReadCntrs(i)(j)))

        val throttleBit = cntrMSB && mmio.module.io.enGlobal
        mods.foreach( sched => sched.io.throttle(i).dramBank(j) := throttleBit )
      }
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
