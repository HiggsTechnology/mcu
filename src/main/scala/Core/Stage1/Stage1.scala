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
  val pc_reset = PcStart.U(AddrWidth)
  val pc = RegInit(pc_reset)
  val n_pc = RegInit(pc)

  val pc_wire = Wire(UInt(AddrWidth))

  val ram = Module(new RAMHelper)
  ram.io.clk := clock
  ram.io.en  := !reset.asBool()
  ram.io.rIdx := (pc - pc_reset) >> 3
  ram.io.wIdx := DontCare
  ram.io.wen  := false.B
  ram.io.wdata := DontCare
  ram.io.wmask := DontCare
  val inst = ram.io.rdata
//  io.to_mem.bits := pc

  val predecode = Module(new PreDecode)
  predecode.io.instr := inst

  val predict_pc = predecode.io.offset + pc
  val ifu_redirect = predecode.io.is_br && ((predecode.io.br_type === BrType.J) || (predecode.io.br_type === BrType.B && predecode.io.offset < 0.U))
  pc_wire := Mux(io.redirect.bits.mispred, io.redirect.bits.new_pc, Mux(ifu_redirect, predict_pc, n_pc + 4.U))
  pc := pc_wire


  val flush = io.redirect.bits.mispred && io.redirect.valid

  io.out.bits.inst   := inst
  io.out.bits.pc     := pc
  io.out.bits.is_br  := predecode.io.is_br
  io.out.bits.pre_pc := predict_pc
  io.out.valid       := !flush && !predecode.io.is_br  // TODO io.can_enq && io.bus.fire()

}