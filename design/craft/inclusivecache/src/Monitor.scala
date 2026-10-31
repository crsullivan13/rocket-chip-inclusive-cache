package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

class MonitorReq(sourceBits: Int) extends Bundle {
  val source = UInt(sourceBits.W)
  val rcid   = UInt(InclusiveCacheMonitor.rcidBits.W)
}

// Events seen at one L2 bank this cycle
class MonitorBankTap(sourceBits: Int, inSourceBits: Int) extends Bundle {
  // Inner A ingress (before SinkA's buffer) and whether the bank had no usable MSHR free
  val aValid   = Bool()
  val aReady   = Bool()
  val aRcid    = UInt(InclusiveCacheMonitor.rcidBits.W)
  val mshrFull = Bool()
  // Inner reads: AcquireBlock/AcquirePerm/Get accept and the first beat of its response
  val svcIssue = Valid(new MonitorReq(inSourceBits))
  val svcDone  = Valid(UInt(inSourceBits.W))
  // Outer reads: AcquireBlock/Get issue and the first beat of its response
  val rdIssue = Valid(new MonitorReq(sourceBits))
  val rdDone  = Valid(UInt(sourceBits.W))
  // Outer writebacks: ReleaseData issue and its ReleaseAck
  val wrIssue = Valid(new MonitorReq(sourceBits))
  val wrDone  = Valid(UInt(sourceBits.W))
}

class MonitorEntry extends Bundle {
  val valid     = Bool()
  val start     = UInt(32.W)
  val rcid      = UInt(InclusiveCacheMonitor.rcidBits.W)
  val contended = Bool()
}

object InclusiveCacheMonitor {
  val rcidBits = 4 // TL rcid is 4 bits
  val rcidStride = 0x80
  val cyclesOffset = 0x800
}

// Per-RCID pressure counters, shared by all L2 banks:
//  - LLC service latency at the inner edge (hits and misses alike)
//  - DRAM read latency at the outer edge, split by whether another RCID had DRAM traffic in flight
//  - inner A backpressure while the bank had no usable MSHR free
// All counters are free-running; software takes deltas over its own window.
class InclusiveCacheMonitor(nRCID: Int, nBanks: => Int, nSources: Int, nInSources: => Int, address: BigInt)(implicit p: Parameters) extends LazyModule()(p) {
  import InclusiveCacheMonitor._
  require (nRCID <= 16 && cyclesOffset + 8 <= InclusiveCacheParameters.MonitorSize)

  val device = new SimpleDevice("llc-monitor", Seq("ku-csl,llc-monitor")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++ Map("ku-csl,rcids" -> Seq(ResourceInt(BigInt(nRCID)))))
    }
  }

  val regnode = TLRegisterNode(
    address   = Seq(AddressSet(address, InclusiveCacheParameters.MonitorSize - 1)),
    device    = device,
    beatBytes = 8)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val sourceBits = log2Up(nSources)
    val inSourceBits = log2Up(nInSources)
    val io = IO(new Bundle {
      val banks = Input(Vec(nBanks, new MonitorBankTap(sourceBits, inSourceBits)))
    })

    val now = RegInit(0.U(64.W))
    now := now + 1.U

    // Outstanding DRAM reads/writebacks per RCID, across all banks
    val wOut = log2Ceil(nBanks * nSources + 1)
    val rdOut = RegInit(VecInit(Seq.fill(nRCID)(0.U(wOut.W))))
    val wrOut = RegInit(VecInit(Seq.fill(nRCID)(0.U(wOut.W))))
    val active = VecInit((0 until nRCID).map { r => rdOut(r) =/= 0.U || wrOut(r) =/= 0.U })
    def otherActive(rcid: UInt): Bool = (active.asUInt & ~UIntToOH(rcid, nRCID)).orR

    case class Done(valid: Bool, rcid: UInt, lat: UInt, contended: Bool)
    case class SvcDone(valid: Bool, rcid: UInt, lat: UInt)
    case class WrDone(valid: Bool, rcid: UInt)

    // In-flight inner reads, outer reads and outer writebacks, indexed by (bank, source)
    val (svcDones, rdDones, wrDones) = io.banks.map { t =>
      val svcValid = RegInit(VecInit(Seq.fill(nInSources)(false.B)))
      val svcStart = Reg(Vec(nInSources, UInt(32.W)))
      val svcRcid  = Reg(Vec(nInSources, UInt(rcidBits.W)))
      val rdTab = RegInit(VecInit(Seq.fill(nSources)(0.U.asTypeOf(new MonitorEntry))))
      val wrValid = RegInit(VecInit(Seq.fill(nSources)(false.B)))
      val wrRcid = Reg(Vec(nSources, UInt(rcidBits.W)))

      val svcDone = SvcDone(t.svcDone.valid && svcValid(t.svcDone.bits), svcRcid(t.svcDone.bits),
        now(31, 0) - svcStart(t.svcDone.bits))
      val rdEntry = rdTab(t.rdDone.bits)
      val rdDone = Done(t.rdDone.valid && rdEntry.valid, rdEntry.rcid,
        now(31, 0) - rdEntry.start, rdEntry.contended || otherActive(rdEntry.rcid))
      val wrDone = WrDone(t.wrDone.valid && wrValid(t.wrDone.bits), wrRcid(t.wrDone.bits))

      when (t.svcDone.valid) { svcValid(t.svcDone.bits) := false.B }
      when (t.svcIssue.valid) {
        svcValid(t.svcIssue.bits.source) := true.B
        svcStart(t.svcIssue.bits.source) := now(31, 0)
        svcRcid(t.svcIssue.bits.source)  := t.svcIssue.bits.rcid
      }

      // Sticky: a read is contended if another RCID was active at any point while it was in flight
      rdTab.foreach { e => when (e.valid && otherActive(e.rcid)) { e.contended := true.B } }
      when (t.rdDone.valid) { rdEntry.valid := false.B }
      when (t.rdIssue.valid) {
        val e = rdTab(t.rdIssue.bits.source)
        e.valid     := true.B
        e.start     := now(31, 0)
        e.rcid      := t.rdIssue.bits.rcid
        e.contended := otherActive(t.rdIssue.bits.rcid)
      }

      when (t.wrDone.valid) { wrValid(t.wrDone.bits) := false.B }
      when (t.wrIssue.valid) {
        wrValid(t.wrIssue.bits.source) := true.B
        wrRcid(t.wrIssue.bits.source)  := t.wrIssue.bits.rcid
      }

      (svcDone, rdDone, wrDone)
    }.unzip3

    def counter() = RegInit(VecInit(Seq.fill(nRCID)(0.U(64.W))))
    val svcLat    = counter()
    val svcCnt    = counter()
    val cleanLat  = counter()
    val cleanCnt  = counter()
    val contLat   = counter()
    val contCnt   = counter()
    val stallMshr = counter()
    val want      = counter()

    def latSum(ds: Seq[(Bool, UInt)]): UInt = ds.map { case (v, lat) => Mux(v, lat, 0.U) }.reduce(_ +& _)

    for (r <- 0 until nRCID) {
      val svc = svcDones.map { d => d.valid && d.rcid === r.U }
      svcLat(r) := svcLat(r) + latSum(svc zip svcDones.map(_.lat))
      svcCnt(r) := svcCnt(r) + PopCount(svc)

      val rdIssues = io.banks.map { t => t.rdIssue.valid && t.rdIssue.bits.rcid === r.U }
      val wrIssues = io.banks.map { t => t.wrIssue.valid && t.wrIssue.bits.rcid === r.U }
      val mine = rdDones.map { d => d.valid && d.rcid === r.U }
      rdOut(r) := rdOut(r) + PopCount(rdIssues) - PopCount(mine)
      wrOut(r) := wrOut(r) + PopCount(wrIssues) - PopCount(wrDones.map { d => d.valid && d.rcid === r.U })

      val clean = (rdDones zip mine).map { case (d, m) => m && !d.contended }
      val cont  = (rdDones zip mine).map { case (d, m) => m &&  d.contended }
      cleanLat(r) := cleanLat(r) + latSum(clean zip rdDones.map(_.lat))
      cleanCnt(r) := cleanCnt(r) + PopCount(clean)
      contLat(r)  := contLat(r)  + latSum(cont zip rdDones.map(_.lat))
      contCnt(r)  := contCnt(r)  + PopCount(cont)

      val wants = io.banks.map { t => t.aValid && t.aRcid === r.U }
      want(r)      := want(r) + PopCount(wants)
      stallMshr(r) := stallMshr(r) + PopCount((io.banks zip wants).map { case (t, w) => w && !t.aReady && t.mshrFull })
    }

    // One rcidStride-byte block per RCID, then the cycle counter
    val perRcid = (0 until nRCID).flatMap { r =>
      val base = rcidStride * r
      def ctr(off: Int, c: Vec[UInt], name: String, desc: String) =
        (base + off) -> Seq(RegField.r(64, c(r), RegFieldDesc(s"${name}_$r", s"$desc for RCID $r")))
      Seq(
        ctr(0x00, cleanLat,  "clean_lat",  "Summed latency of DRAM reads that saw no other-RCID DRAM traffic"),
        ctr(0x08, cleanCnt,  "clean_cnt",  "Number of DRAM reads that saw no other-RCID DRAM traffic"),
        ctr(0x10, contLat,   "cont_lat",   "Summed latency of DRAM reads that overlapped other-RCID DRAM traffic"),
        ctr(0x18, contCnt,   "cont_cnt",   "Number of DRAM reads that overlapped other-RCID DRAM traffic"),
        ctr(0x20, svcLat,    "svc_lat",    "Summed LLC service latency of inner reads, accept to first response beat"),
        ctr(0x28, svcCnt,    "svc_cnt",    "Number of inner reads completed"),
        ctr(0x30, stallMshr, "stall_mshr", "Bank-cycles inner A was blocked while no usable MSHR was free"),
        ctr(0x38, want,      "want",       "Bank-cycles inner A was valid"),
        (base + 0x40) -> Seq(
          RegField.r(16, rdOut(r).pad(16), RegFieldDesc(s"rd_out_$r", s"Outstanding DRAM reads for RCID $r")),
          RegField.r(16, wrOut(r).pad(16), RegFieldDesc(s"wr_out_$r", s"Outstanding DRAM writebacks for RCID $r"))))
    }

    regnode.regmap((perRcid ++ Seq[RegField.Map](
      cyclesOffset -> Seq(RegField.r(64, now, RegFieldDesc("cycles", "Free-running L2 clock cycle counter")))
    )): _*)
  }
}
