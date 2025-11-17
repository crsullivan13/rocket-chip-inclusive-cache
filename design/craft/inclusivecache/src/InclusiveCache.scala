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

  //val dramRegNode = BundleBridgeSource(() => new BRUTileIO(4)) // TODO make number of domains one parameter everywhere, doesn't do anything

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
    
    val nLLCBanks = p(SubsystemBankedCoherenceKey).nBanks
    val llcBankOffset = 6
    val llcBankMask = nLLCBanks - 1

    val nDramBanks = 8
    val dramBankOffset = 16
    val dramBankMask = nDramBanks - 1

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

    // Memguard
    val countInstFetch = RegInit(true.B)
    val AccessCounterReset = RegInit(false.B)
    val EnableInterrupt = Seq.fill(cache.numCPUs)(RegInit(false.B))
 
    // per-core, per-bank counters
    val PerLLCBankCounters = Seq.fill(cache.numCPUs)(RegInit(VecInit(Seq.fill(nLLCBanks)(0.U(64.W)))))
    val PerDRAMBankCounters =  Seq.fill(cache.numCPUs)(RegInit(VecInit(Seq.fill(nDramBanks)(0.U(64.W)))))
 
    // // Per-CPU counters
    // val MissCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))
    // val AccessCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))

    // Per-CPU Regulation Budgets
    val CoreLLCBudgets = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))
    val CoreDRAMBudgets = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))

    // only interrupt the core once per period
    val hasInterrupted = Seq.fill(cache.numCPUs)(RegInit(false.B))
    val coreDoInterrupt = Seq.fill(cache.numCPUs)(WireInit(false.B))

    // val memguardPeriodCntr = Reg(UInt(25.W))
    val memguardPeriodReset = VecInit(Seq.fill(cache.numCPUs)(RegInit(false.B)))

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

      // /*Performance Counters*/
      // val inDomainID = Mux(in.a.fire, in.a.bits.domainId, Mux(in.c.fire, in.c.bits.domainId, 0.U))
      // val outDomainID = Mux(scheduler.io.out.a.fire, scheduler.io.out.a.bits.domainId, Mux(scheduler.io.out.c.fire, scheduler.io.out.c.bits.domainId, 0.U))
      // val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
      // val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= membase.U
      // val aIsRead = aIsAcquire || (aIsInstFetch && countInstFetch)
      // val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= membase.U
      // val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      // val outaIsAcquire = scheduler.io.out.a.bits.opcode === TLMessages.AcquireBlock
      // val outaIsInstFetch = scheduler.io.out.a.bits.opcode === TLMessages.Get && scheduler.io.out.a.bits.address >= membase.U
      // val outCIsWb =  in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      // val isMiss = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && scheduler.io.out.a.fire
      // val isWbToDRAM = (outCIsWb && edgeOut.last(scheduler.io.out.c))
      // val toDRAM = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && scheduler.io.out.a.fire
      // val isAccess = ((aIsWrite || aIsRead || (aIsInstFetch && countInstFetch)) && in.a.fire) || (cIsWb && in.c.fire) 
      // when (!memguardPeriodReset(outDomainID))
      // {
      //     when (toDRAM)
      //     {
      //         PerBankMissCounters(i)(outDomainID)  := PerBankMissCounters(i)(outDomainID) + 1.U
      //     }
      // }

      // when (!memguardPeriodReset(inDomainID))
      // {
      //     when (isAccess)
      //     {
      //         PerBankAccessCounters(i)(inDomainID) := PerBankAccessCounters(i)(inDomainID) + 1.U
      //     }
      // }

      scheduler.io.in <> in
      out <> scheduler.io.out
      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

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

    val activeDRAMAcquireCores = mods.map( sched => {
      val didBankFireAcquire = sched.io.out.a.fire
      val firedCore = WireDefault(cache.numCPUs.U)
      val bankTarget = WireDefault(nDramBanks.U)

      when ( didBankFireAcquire ) {
        firedCore := sched.io.out.a.bits.domainId
        bankTarget := ( sched.io.out.a.bits.address >> dramBankOffset ) & dramBankMask.U
      }

      (firedCore: UInt, bankTarget: UInt)
    })

    val activeLLCAcquireCores = mods.map( sched => {
      val didBankRecieveAcquire = sched.io.in.a.fire
      val firedCore = WireDefault(cache.numCPUs.U)
      val bankTarget = WireDefault(nLLCBanks.U)

      when ( didBankRecieveAcquire ) {
        firedCore := sched.io.in.a.bits.domainId
        bankTarget := ( sched.io.in.a.bits.address >> llcBankOffset ) & llcBankMask.U
      }

      (firedCore: UInt, bankTarget: UInt)
    })

    for (i <- 0 until cache.numCPUs) {
      when (memguardPeriodReset(i)) { // Reset All
        for (j <- 0 until nLLCBanks) {
          PerLLCBankCounters(i)(j) := 0.U
        }

        for (j <- 0 until nDramBanks) {
          PerDRAMBankCounters(i)(j) := 0.U
        }

        hasInterrupted(i) := false.B
      } .otherwise { // Calculate per core total accesses 
        val didCoreFireAcquire = activeDRAMAcquireCores.map{ case (core,_) => core === i.U }.reduce(_||_)

        for (j <- 0 until nLLCBanks) {
          val didTargetBank = activeLLCAcquireCores.map{ case (_, bank) => bank === j.U}.reduce(_||_) && didCoreFireAcquire
          PerLLCBankCounters(i)(j) := PerLLCBankCounters(i)(j) + didTargetBank
        }

        for (j <- 0 until nDramBanks) {
          val didTargetBank = activeDRAMAcquireCores.map{ case (_, bank) => bank === j.U}.reduce(_||_) && didCoreFireAcquire
          PerDRAMBankCounters(i)(j) := PerDRAMBankCounters(i)(j) + didTargetBank
        }
      }
    }

    /* 
      Core will generate an interrupt if it is over budget and it is the first interrupt,
      or if there is a new period and we must interrupt to let it get rid of the throttle task
    */
    println(s"CACHE COUNTER intSrc.out.size = ${intSrc.out.length}, intSrc.out(0).size = ${intSrc.out(0)._1.length}")
    val (intOut, _) = intSrc.out(0)
    for (i <- 0 until cache.numCPUs) {
        val overDRAMBudget = PerDRAMBankCounters(i).map( cntr => cntr >= CoreDRAMBudgets(i) ).reduce(_||_)
        coreDoInterrupt(i) := (overDRAMBudget && EnableInterrupt(i) && !hasInterrupted(i))
        when (!memguardPeriodReset(i)) { // do not drive signal twice
          hasInterrupted(i) := coreDoInterrupt(i) || hasInterrupted(i)
        } 
        
        intOut(i) := coreDoInterrupt(i)
    }

    // Memguard
    // val LLCAcessCounterRegs = PerLLCBankCounters.zipWithIndex.flatMap{ case (coreRegs, i) => 
    //   coreRegs.zipWithIndex.map{ case (bankReg, j) =>
    //     RegField.r(bankReg.getWidth, bankReg,
    //       RegFieldDesc(s"llcAccess${i}_${j}", s"LLC access count for core $i bank $j"))
    //   }  
    // }

    // val LLCMissCounterRegs = PerDRAMBankCounters.zipWithIndex.flatMap{ case (coreRegs, i) => 
    //   coreRegs.zipWithIndex.map{ case (bankReg, j) =>
    //     RegField.r(bankReg.getWidth, bankReg,
    //       RegFieldDesc(s"llcMiss${i}_${j}", s"LLC miss count for core $i bank $j"))
    //   }  
    // }

    val CountInstFetchReg = RegField(countInstFetch.getWidth, countInstFetch, RegFieldDesc("countInstFetch", "Bool count instruction fetches in access counters"))

    val EnableIntRegs = EnableInterrupt.zipWithIndex.map { case (reg, i) =>
        RegField(64, reg, RegFieldDesc(s"EnableInterruptCore${i}", s"EnableInterruptsCore"))
    }

    val CoreLLCBudgetRegs = CoreLLCBudgets.zipWithIndex.map { case (reg, i) => 
        RegField(64, reg, RegFieldDesc(s"CoreLLCBudgetCore${i}", s"CoreLLCBudget"))
    }
  
    val CoreDRAMBudgetRegs = CoreDRAMBudgets.zipWithIndex.map { case (reg, i) => 
        RegField(64, reg, RegFieldDesc(s"CoreDRAMBudgetCore${i}", s"CoreDRAMBudget"))
    }
  
    val PeriodResetRegs = memguardPeriodReset.zipWithIndex.map{ case (reg, i) => 
        RegField(64, reg, RegFieldDesc(s"PeriodLength${i}", s"PeriodLength${i}"))
    }

    regnode.regmap(
      // 0x050 -> RegFieldGroup("CoreLLCAccess", Some("Per-core LLC access totals"), LLCAcessCounterRegs),
      // 0x100 -> RegFieldGroup("CoreLLCMiss", Some("Per-core LLC miss totals"), LLCMissCounterRegs),
      0x300 -> Seq(CountInstFetchReg),
      0x308 -> RegFieldGroup("CoreInterruptEnables", Some("Per-core interrupt enables"), EnableIntRegs),
      0x400 -> RegFieldGroup("CoreLLCBudgets", Some("Per-core LLC budgets"), CoreLLCBudgetRegs),
      0x450 -> RegFieldGroup("CoreDRAMBudgets", Some("Per-core DRAM budgets"), CoreDRAMBudgetRegs),
      0x500 -> RegFieldGroup("PeriodMemguard", Some("Memguard period lengths"), PeriodResetRegs),
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
