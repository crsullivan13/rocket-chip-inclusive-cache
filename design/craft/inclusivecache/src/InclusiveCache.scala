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

import freechips.rocketchip.subsystem.{SubsystemBankedCoherenceKey}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

import freechips.rocketchip.tile._

import midas.targetutils.SynthesizePrintf

// For memguard
import  freechips.rocketchip.interrupts._
import freechips.rocketchip.util.Annotated.interrupts
import freechips.rocketchip.subsystem.ExtMem

class OuterAcquireInfo() extends Bundle {
  val regulationDomain = UInt(2.W)
  val didFireAcquire = Bool()
}

// TODO: Unify/remove these info bundles, i.e. make the code not bad
class PerfEventInfo() extends Bundle {
  val domainId = UInt(2.W)
  val didEventOccur = Bool()
}

class PerfEvents() extends Bundle {
  val sinkAStall = new PerfEventInfo()
  val sinkCStall = new PerfEventInfo()
}

// class OuterReleaseInfo() extends Bundle {
//   val regulationDomain = UInt(2.W)
//   val didFireRelease = Bool()
// }

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
  val regulationDevice = new SimpleDevice("llc-mshr-reg",Seq("llc-mshr-reg"))

  val regnode = new TLRegisterNode(
    address = Seq(AddressSet(0x21000000, 0x7ff)),
    device = regulationDevice,
    beatBytes = 8)

  val dramRegNode = BundleBridgeSource(() => new BRUTileIO(4)) // TODO make number of domains one parameter everywhere

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

  val intSrc = IntSourceNode(IntSourcePortSimple(num = cache.numCPUs, resources = device.int))

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
    // Memguard
    val bundleParamsIn = node.in(0)._2.bundle
    val bundleParamsOut = node.out(0)._2.bundle
    val membase =  p(ExtMem).get.master.base
    val nBanks = p(SubsystemBankedCoherenceKey).nBanks

    // If you have a control port, you must have at least one cache port
    require (ctrls.isEmpty || !node.edges.in.isEmpty)

    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name}")
      }
      println("")
    }

/*
      Performance Counters that we added
    */
    val countInstFetch = RegInit(true.B)
    val AccessCounterReset = RegInit(false.B)
    val EnableInterrupt = Seq.fill(cache.numCPUs)(RegInit(false.B))
 
    /*
        Per-CacheBank counters
    */
    val PerBankMissCounters =  Seq.fill(nBanks)(RegInit(VecInit(Seq.fill(cache.numCPUs)(0.U(64.W)))))
    val PerBankAccessCounters = Seq.fill(nBanks)(RegInit(VecInit(Seq.fill(cache.numCPUs)(0.U(64.W)))))
 
    // Per-CPU counters
    val MissCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))
    val AccessCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))

    // Per-CPU Regulation Budgets
    val CoreBudgets = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))

    // only interrupt the core once per period
    val hasInterrupted = Seq.fill(cache.numCPUs)(RegInit(false.B))
    val coreDoInterrupt = Seq.fill(cache.numCPUs)(WireInit(false.B))

    // val memguardPeriodCntr = Reg(UInt(25.W))
    val memguardPeriodCntrReset = VecInit(Seq.fill(cache.numCPUs)(RegInit(false.B)))

    for (j <- 0 until cache.numCPUs)
    {
      when (memguardPeriodCntrReset(j)) // Reset all
      {
          for (i <- 0 until nBanks)
          {
              PerBankAccessCounters(i)(j) := 0.U
              PerBankMissCounters(i)(j) := 0.U
          }

          MissCounters(j) := 0.U
          AccessCounters(j) := 0.U
          hasInterrupted(j) := false.B

      }
      .otherwise // Calculate per core total accesses
      {
            val tmpSumMiss = VecInit(Seq.fill(nBanks)(0.U(64.W)))
            val tmpSumAccess = VecInit(Seq.fill(nBanks)(0.U(64.W)))
            tmpSumMiss(0) := PerBankMissCounters(0)(j)
            tmpSumAccess(0) := PerBankAccessCounters(0)(j)
            for (i <- 1 until nBanks)
            {
              tmpSumMiss(i) := PerBankMissCounters(i)(j) + tmpSumMiss(i-1)
              tmpSumAccess(i) := PerBankAccessCounters(i)(j) + tmpSumAccess(i-1)
            }

            MissCounters(j) := tmpSumMiss(nBanks - 1)
            AccessCounters(j) := tmpSumAccess(nBanks- 1)

      }
    }

    /* 
      Core will generate an interrupt if it is over budget and it is the first interrupt,
      or if there is a new period and we must interrupt to let it get rid of the throttle task
    */
    println(s"CACHE COUNTER intSrc.out.size = ${intSrc.out.length}, intSrc.out(0).size = ${intSrc.out(0)._1.length}")
    for (i <- 0 until cache.numCPUs)
    {
        val overBudget = MissCounters(i) >= CoreBudgets(i) && EnableInterrupt(i)        // we should take this out to do 1ms regulation
        coreDoInterrupt(i) := (overBudget && EnableInterrupt(i) && !hasInterrupted(i)) //|| (hasInterrupted(i) && periodCntrReset)
        when (!memguardPeriodCntrReset(i)) // do not drive signal twice
        {
          hasInterrupted(i) := coreDoInterrupt(i) || hasInterrupted(i)
        } 
        val (intOut, _) = intSrc.out(0) // does this need to be i as well? --> that causes an error
        intOut(i) := coreDoInterrupt(i)

    }
    val perBankEvent = Wire(Vec(2, new PerfEventInfo()))
    val nDomains = 4

    val perfEnable = RegInit(false.B)
    val perfLineRefill = Reg(Vec(nDomains, UInt(64.W)))
    val perfWriteBack = Reg(Vec(nDomains, UInt(64.W)))
    val perfSinkAStall = Reg(Vec(nDomains, UInt(64.W)))
    // val perfSinkCStall = Reg(Vec(nDomains, UInt(64.W)))

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
      val scheduler = Module(new InclusiveCacheBankScheduler(params)).suggestName("inclusive_cache_bank_sched")

      /*Performance Counters*/
      val inDomainID = Mux(in.a.fire, in.a.bits.domainId, Mux(in.c.fire, in.c.bits.domainId, 0.U))
      val outDomainID = Mux(scheduler.io.out.a.fire, scheduler.io.out.a.bits.domainId, Mux(scheduler.io.out.c.fire, scheduler.io.out.c.bits.domainId, 0.U))
      val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
      val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= membase.U
      val aIsRead = aIsAcquire || (aIsInstFetch && countInstFetch)
      val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= membase.U
      val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      val outaIsAcquire = scheduler.io.out.a.bits.opcode === TLMessages.AcquireBlock
      val outaIsInstFetch = scheduler.io.out.a.bits.opcode === TLMessages.Get && scheduler.io.out.a.bits.address >= membase.U
      val outCIsWb =  in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      val isMiss = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && scheduler.io.out.a.fire
      val isWbToDRAM = (outCIsWb && edgeOut.last(scheduler.io.out.c))
      val toDRAM = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && scheduler.io.out.a.fire
      val isAccess = ((aIsWrite || aIsRead || (aIsInstFetch && countInstFetch)) && in.a.fire) || (cIsWb && in.c.fire) 
      when (!memguardPeriodCntrReset(outDomainID))
      {
          when (toDRAM)
          {
              PerBankMissCounters(i)(outDomainID)  := PerBankMissCounters(i)(outDomainID) + 1.U
          }
      }

      when (!memguardPeriodCntrReset(inDomainID))
      {
          when (isAccess)
          {
              PerBankAccessCounters(i)(inDomainID) := PerBankAccessCounters(i)(inDomainID) + 1.U
          }
      }

      scheduler.io.in <> in
      out <> scheduler.io.out
      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare
      scheduler.io.perfEnable := perfEnable

      perBankEvent(i.U).didEventOccur := scheduler.io.perfEvents.sinkAStall.didEventOccur
      perBankEvent(i.U).domainId := scheduler.io.perfEvents.sinkAStall.domainId

      // Tie down default values in case there is no controller
      scheduler.io.req.valid := false.B
      scheduler.io.req.bits.address := 0.U
      scheduler.io.resp.ready := true.B


      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.
      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      scheduler
    }

    val enGlobal = RegInit(0.B)

    val outerAcquireCount = Reg(Vec(nDomains, UInt(64.W)))
    val acquireBudget = Reg(Vec(nDomains,UInt(64.W)))

    val periodCount = RegInit(0.U(64.W))
    val periodLength = Reg(UInt(64.W))
    val periodReset = Wire(Bool())

    periodReset := periodCount >= periodLength
    periodCount := Mux(periodReset || !enGlobal, 0.U, periodCount + 1.U)

    for ( i <- 0 until nDomains) {
      val anySinkAStall = perBankEvent.map(_.didEventOccur).reduce(_||_)
      val domain = perBankEvent.map(_.domainId === i.U).reduce(_||_)
      perfSinkAStall(i.U) := Mux(perfEnable, Mux(!domain, 0.U + perfSinkAStall(i.U), anySinkAStall + perfSinkAStall(i.U)), 0.U)
    }

    val activeAcquireDomains = mods.map( sched => {
      val didBankFireAcquire = sched.io.out.a.fire
      val firedDomainId = WireDefault(nDomains.U)

      when ( didBankFireAcquire ) {
        firedDomainId := sched.io.out.a.bits.domainId
        printf("LLC: read out fired domain %d\n", firedDomainId)
      }

      when ( sched.io.in.a.fire ) {
        printf("LLC: read in fired domain %d\n", sched.io.in.a.bits.domainId)
      }

      firedDomainId
    })

    val activeReleaseDomains = mods.map( sched => {
      val didBankFireRelease = node.out(0)._2.last(sched.io.out.c) && sched.io.out.c.bits.opcode === TLMessages.ReleaseData
      val firedDomainId = WireDefault(nDomains.U)

      when ( didBankFireRelease ) {
        firedDomainId := sched.io.out.c.bits.domainId
        printf("LLC: write out fired domain %d\n", firedDomainId)
      }

      when ( sched.io.in.c.fire ) {
        printf("LLC: write in fired domain %d\n", sched.io.in.c.bits.domainId)
      }

      firedDomainId
    })

    for ( i <- 0 until nDomains ) {
      val didDomainFireAcquire = activeAcquireDomains.map( id => id === i.U ).reduce(_||_)
      val didDomainFireRelease = activeReleaseDomains.map( id => id === i.U ).reduce(_||_)

      outerAcquireCount(i.U) := Mux(periodReset || !enGlobal, 0.U + didDomainFireAcquire, didDomainFireAcquire + outerAcquireCount(i.U))
      perfLineRefill(i.U) := Mux(perfEnable, didDomainFireAcquire + perfLineRefill(i.U), 0.U)
      perfWriteBack(i.U) := Mux(perfEnable, didDomainFireRelease + perfWriteBack(i.U), 0.U)
      
      when (perfEnable && didDomainFireAcquire) {
        printf("Domain %d line refill count: %d\n", i.U, perfLineRefill(i.U))
        printf("Domain %d reg count: %d\n", i.U, outerAcquireCount(i.U))
      }

      mods.foreach( sched => sched.io.throttle(i.U) := (outerAcquireCount(i.U) >= acquireBudget(i.U)) && enGlobal )

      dramRegNode.bundle.nThrottle(i.U) := (outerAcquireCount(i.U) >= acquireBudget(i.U)) && enGlobal
    }

    val enGlobalField = RegField(enGlobal.getWidth, enGlobal, RegFieldDesc("enGlobal", "Global Enable"))

    val periodLenRegField = RegField(periodLength.getWidth, periodLength, RegFieldDesc("periodLength", "Period length"))

    val maxReadRegField = acquireBudget.zipWithIndex.map { case (reg, i) => RegField(64, reg,
        RegFieldDesc(s"acquireBudget$i", s"Read budget for domain $i")) }

    // Memguard
    val LLCAccessCountersReg = AccessCounters.zipWithIndex.map{ case (reg, i) => 
        RegField.r(64, reg, RegFieldDesc(s"LLCAccessCounterReg${i}", s"Total LLC accesses for domainId=${i}"))
    }

    val LLCMissCountersReg = MissCounters.zipWithIndex.map{ case (reg, i) =>
        RegField.r(64, reg, RegFieldDesc(s"LLCMissCounterReg${i}", s"Total LLC misses for domainId=${i}"))
    }
    val CountInstFetchReg = RegField(countInstFetch.getWidth, countInstFetch, RegFieldDesc("countInstFetch", "Bool count instruction fetches in access counters"))
    val EnableIntRegs = EnableInterrupt.zipWithIndex.map { case (reg, i) =>
        RegField(64, reg, RegFieldDesc(s"EnableInterruptCore${i}", s"EnableInterruptsCore"))
    } 
    val CoreBudgetRegs = CoreBudgets.zipWithIndex.map { case (reg, i) => 
        RegField(64, reg, RegFieldDesc(s"CoreBudgetCore${i}", s"CoreBudget"))
    }
    val PeriodResetRegs = memguardPeriodCntrReset.zipWithIndex.map{ case (reg, i) => 
        RegField(64, reg, RegFieldDesc(s"PeriodLength${i}", s"PeriodLength${i}"))
    }
    val perfEnField = RegField(perfEnable.getWidth, perfEnable, RegFieldDesc("perfEnable", "Perf counter enable"))

    val lineRefillRegField = perfLineRefill.zipWithIndex.map { case (reg, i) => RegField.r(64, reg,
        RegFieldDesc(s"lineRefill$i", s"Line refill count for domain $i")) }

    val writeBackRegField = perfWriteBack.zipWithIndex.map { case (reg, i) => RegField.r(64, reg,
        RegFieldDesc(s"writeBack$i", s"Write back count for domain $i")) }

    val sinkAStallRegField = perfSinkAStall.zipWithIndex.map { case (reg, i) => RegField.r(64, reg,
        RegFieldDesc(s"sinkAStall$i", s"Sink A stall cycles for domain $i")) }

    regnode.regmap(
      0x000 -> Seq(enGlobalField),
      0x008 -> Seq(periodLenRegField),
      0x010 -> RegFieldGroup("AcquireBudget", Some("Per-domain max read config"), maxReadRegField),
      0x050 -> RegFieldGroup("DomainLLCAccess", Some("Per-domain LLC access totals"), LLCAccessCountersReg),
      0x100 -> RegFieldGroup("DomainLLCMiss", Some("Per-domain LLC miss totals"), LLCMissCountersReg),
      0x300 -> Seq(CountInstFetchReg),
      0x308 -> RegFieldGroup("CoreInterruptEnables", Some("Per-core interrupt enables"), EnableIntRegs),
      0x400 -> RegFieldGroup("CoreBudgets", Some("Per-core budgets"), CoreBudgetRegs),
      0x500 -> RegFieldGroup("PeriodMemguard", Some("Memguard period lengths"), PeriodResetRegs),
      0x650 -> Seq(perfEnField),
      0x660 -> RegFieldGroup("LineRefill", Some("Per-domain line refill count"), lineRefillRegField),
      0x680 -> RegFieldGroup("WriteBack", Some("Per-domain writeback count"), writeBackRegField),
      0x700 -> RegFieldGroup("SinkAStall", Some("Per-domain sinkA stall cycle count"), sinkAStallRegField),
    )

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
        when (contained && sched.io.req.ready) { ctrl.module.io.flush_req.ready := true.B }

        when (sched.io.resp.valid) { ctrl.module.io.flush_resp := true.B }
        sched.io.resp.ready := true.B
      }}
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
