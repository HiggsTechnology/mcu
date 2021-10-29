package core.stage1

import chisel3.util.{DecoupledIO, RegEnable, ValidIO}
import core.{CoreModule, CtrlFlow, ICodeIO, RedirectIO}
import chisel3._


class IFU extends CoreModule {
  val io = IO(new Bundle{
    val redirect = Flipped(ValidIO(new RedirectIO))
    val icode = DecoupledIO(new ICodeIO)
    val out = DecoupledIO(new CtrlFlow)
  })
  val pc_reset = PcStart.U(AddrWidth)
  val pc = RegInit(pc_reset)
  val npc = Wire(UInt(AddrWidth))
  npc := Mux(io.redirect.bits.mispred, io.redirect.bits.new_pc, pc + 4.U)
  pc := npc

  io.icode.bits.pc := pc
  val instr = RegEnable(io.icode.bits.instr,io.icode.fire())
  val predecode = Module(new PreDecode)
  predecode.io.instr := instr




  io.out.bits.instr := instr
  io.out.bits.pc := pc

}