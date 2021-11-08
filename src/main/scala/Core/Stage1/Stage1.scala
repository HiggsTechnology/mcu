package Core.Stage1

import Core.{Config, CoreBundle, FetchPreInfo, RAMHelper, Redirect}
import chisel3._
import chisel3.util._


class IBus extends CoreBundle {
  val pc    = Output(UInt(AddrWidth))
  val inst  = Input(UInt(InstWidth))
}

class Stage1 extends Module with Config  {
  val io = IO(new Bundle{
    val redirect = Flipped(ValidIO(new Redirect))
    val out = DecoupledIO(new FetchPreInfo)
  })
  val mispred = io.redirect.bits.mispred && io.redirect.valid

  val pc_reset = PcStart.U(AddrWidth)
  val pcReg = RegInit(pc_reset)
  val pc = Wire(UInt(AddrWidth))

  

  val ram = Module(new RAMHelper)
  ram.io.clk := clock
  ram.io.en  := !reset.asBool()
  ram.io.rIdx := (pc - pc_reset) >> 3
  ram.io.wIdx := DontCare
  ram.io.wen  := false.B
  ram.io.wdata := DontCare
  ram.io.wmask := DontCare
  val inst = Mux(pc(2),ram.io.rdata(63,32),ram.io.rdata(31,0))

  val predecode = Module(new PreDecode)
  predecode.io.instr := inst

  val predict_pc = predecode.io.offset + pc
  val ifu_redirect = predecode.io.is_br && ((predecode.io.br_type === BrType.J) || (predecode.io.br_type === BrType.B && predecode.io.offset.asSInt < 0.S))

  pc := Mux(mispred, io.redirect.bits.new_pc, pcReg)
  pcReg := Mux(ifu_redirect, predict_pc, pc + 4.U)


  io.out.bits.inst   := inst
  io.out.bits.pc     := pc
  io.out.bits.pre_pc := Mux(ifu_redirect, predict_pc, pc + 4.U)
  io.out.valid       := true.B//!mispred && !predecode.io.is_br  // TODO io.can_enq && io.bus.fire()

}