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
//    val to_mem = Decoupled(new IBus)
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
//  io.to_mem.bits := pc

  val predecode = Module(new PreDecode)
  predecode.io.instr := inst

  val predict_pc = predecode.io.offset + pc
  val ifu_redirect = predecode.io.is_br && ((predecode.io.br_type === BrType.J) || (predecode.io.br_type === BrType.B && predecode.io.offset.asSInt < 0.S))

  pc := Mux(mispred, io.redirect.bits.new_pc, pcReg)
  pcReg := Mux(ifu_redirect, predict_pc, pc + 4.U)
  //pc := Mux(io.redirect.bits.mispred && io.redirect.valid, io.redirect.bits.new_pc, Mux(ifu_redirect, predict_pc, pc + 4.U))
  //pc := Mux(io.redirect.valid,io.redirect.bits.new_pc,pc + 4.U)
//  pc := pc_wire
  //  printf("instr %x\n",inst)
  //  printf("predict_pc %x\n",predict_pc)
  //  printf("mux_pc %x\n",Mux(ifu_redirect, predict_pc, pc + 4.U))
  //  printf("mispred %x\n",io.redirect.bits.mispred && io.redirect.valid)
  //  printf("io.redirect new_pc %x\n",io.redirect.bits.new_pc)
  //  printf("new_pc %x\n", io.redirect.bits.new_pc)
  //  printf("real pc %x\n",Mux(io.redirect.bits.mispred && io.redirect.valid, io.redirect.bits.new_pc, Mux(ifu_redirect, predict_pc, pc + 4.U)))

  //printf("pc %x, inst %x\n",pc,inst)

  

  io.out.bits.inst   := inst
  io.out.bits.pc     := pc
  io.out.bits.is_br  := predecode.io.is_br
  io.out.bits.pre_pc := Mux(ifu_redirect, predict_pc, pc + 4.U)
  io.out.valid       := true.B//!mispred && !predecode.io.is_br  // TODO io.can_enq && io.bus.fire()

}