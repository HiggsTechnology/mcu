package Core.Stage1

import Core.{CoreModule, FetchInfo, Redirect}
import chisel3._
import chisel3.util._

class Ifu extends CoreModule {
  val io = IO(new Bundle{
    val redirect = Flipped(ValidIO(new Redirect))
    val pc   = Output(UInt(AddrWidth))
    val inst = Input(UInt(InstWidth))
    val out = DecoupledIO(new FetchInfo)
  })
  val pc_reset = PcStart.U(AddrWidth)
  val pc = RegInit(pc_reset)
  val npc = Wire(UInt(AddrWidth))
  npc := Mux(io.redirect.bits.mispred, io.redirect.bits.new_pc, pc + 4.U)
  pc := npc

  io.pc := pc
  val inst = io.inst
  val predecode = Module(new PreDecode)
  predecode.io.instr := inst

  io.out.bits.inst := inst
  io.out.bits.pc := pc

}