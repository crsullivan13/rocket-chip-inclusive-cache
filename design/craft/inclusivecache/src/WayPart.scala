package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._

// Way partitioning and MSHR cap registers, shared by all L2 banks.
// The layout keeps the offsets of the old CBQRI block so existing software still works.
class InclusiveCacheWayPart(nRCID: Int, ways: Int, numMSHRs: Int, address: BigInt)(implicit p: Parameters) extends LazyModule()(p) {
  require (ways <= 64 && numMSHRs <= 64)

  val device = new SimpleDevice("llc-waypart", Seq("ku-csl,llc-waypart")) {
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      val extra = Map(
        "ku-csl,rcids" -> Seq(ResourceInt(BigInt(nRCID))),
        "ku-csl,ways"  -> Seq(ResourceInt(BigInt(ways))))
      Description(name, mapping ++ extra)
    }
  }

  val regnode = TLRegisterNode(
    address   = Seq(AddressSet(address, InclusiveCacheParameters.WayPartSize - 1)),
    device    = device,
    beatBytes = 8)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      // bit w set means way w is allocatable by that RCID; all-zero reads as all ways
      val wayPartMasks = Output(Vec(nRCID, UInt(ways.W)))
      // bit i set means MSHR i is usable in every bank; all-zero outer MSHRs reads as all usable
      val mshrCapMask  = Output(UInt(numMSHRs.W))
    })

    // Reset to all-ones so an RCID that software never programs is unpartitioned
    val wayPartMasks = RegInit(VecInit(Seq.fill(nRCID)(~0.U(ways.W))))
    val mshrCapMask  = RegInit(~0.U(numMSHRs.W))

    io.wayPartMasks := wayPartMasks.map { m => Mux(m.orR, m, ~0.U(ways.W)) }
    io.mshrCapMask  := Mux(mshrCapMask(numMSHRs-3, 0).orR, mshrCapMask, ~0.U(numMSHRs.W))

    // One 8-byte word per RCID: cc_block_alloc(rcid) at 0x120 + 8*rcid
    val cc_block_alloc = wayPartMasks.zipWithIndex.map { case (mask, i) =>
      (0x120 + 8 * i) -> Seq(RegField(ways, mask,
        RegFieldDesc(s"cc_block_alloc_$i", s"Way-partitioning mask for RCID $i; bit w set means way w is allocatable", reset = Some((BigInt(1) << ways) - 1))))
    }

    regnode.regmap((Seq[RegField.Map](
      0x110 -> Seq(RegField(numMSHRs, mshrCapMask,
        RegFieldDesc("mshrCapMask", "Mask of usable A/B/C MSHRs in each bank", reset = Some((BigInt(1) << numMSHRs) - 1))))
    ) ++ cc_block_alloc): _*)
  }
}
