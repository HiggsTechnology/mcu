package Core

import Core.Stage1.Stage1
import Core.Stage2.Stage2
import Core.Stage3.Stage3
import chisel3._

class EleCoreIO extends Bundle {
  val valid    = Output(Bool())
  val pc_instr = Output(new FetchInfo)
  val regout   = Flipped(new RegWriteIO)
}
class EleCore extends Module with Config{
  val io = IO(new EleCoreIO)
  val stage1 = Module(new Stage1)
  val stage2 = Module(new Stage2)
  val stage3 = Module(new Stage3)

  stage1.io.out      <> stage2.io.in
  stage2.io.out      <> stage3.io.in
  stage1.io.redirect <> stage3.io.redirect
  stage2.io.wb       <> stage3.io.wb

  io.valid     := withClock(clock){
    ~reset.asBool()
  }
  io.pc_instr.pc   := stage3.io.in.bits.fetch_info.pc
  io.pc_instr.inst := stage3.io.in.bits.fetch_info.inst
  io.regout.ena    := stage3.io.wb.bits.ena
  io.regout.addr   := stage3.io.wb.bits.addr
  io.regout.data   := stage3.io.wb.bits.data
}