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
import freechips.rocketchip.tilelink._
import midas.targetutils.SynthesizePrintf

class SourceARequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val tag    = UInt(params.tagBits.W)
  val set    = UInt(params.setBits.W)
  val param  = UInt(3.W)
  val source = UInt(params.outer.bundle.sourceBits.W)
  val block  = Bool()
  val domainId = UInt(2.W)
}

class SourceA(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new SourceARequest(params)))
    val a = Decoupled(new TLBundleA(params.outer.bundle))
    // val regEnable = Input(Bool())
    // val periodReset = Input(Bool())
    val domainAcquire = Output(Vec(4,Bool()))
    val throttleAcquire = Input(Vec(4,Bool()))
  })

  // ready must be a register, because we derive valid from ready
  require (!params.micro.outerBuf.a.pipe && params.micro.outerBuf.a.isDefined)

  val a = Wire(chiselTypeOf(io.a))
  io.a <> params.micro.outerBuf.a(a)

  io.req.ready := a.ready
  a.valid := io.req.valid && !(io.throttleAcquire(io.req.bits.domainId) && io.req.bits.block)
  params.ccover(a.valid && !a.ready, "SOURCEA_STALL", "Backpressured when issuing an Acquire")

  for ( i <- 0 until 4 ) {
    io.domainAcquire(i) := Mux(a.fire && a.bits.opcode === TLMessages.AcquireBlock && a.bits.domainId === i.U, 1.B, 0.B)
  }

  a.bits.domainId := io.req.bits.domainId
  a.bits.opcode  := Mux(io.req.bits.block, TLMessages.AcquireBlock, TLMessages.AcquirePerm)
  a.bits.param   := io.req.bits.param
  a.bits.size    := params.offsetBits.U
  a.bits.source  := io.req.bits.source
  a.bits.address := params.expandAddress(io.req.bits.tag, io.req.bits.set, 0.U)
  a.bits.mask    := ~0.U(params.outer.manager.beatBytes.W)
  a.bits.data    := 0.U
  a.bits.corrupt := false.B
}
