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

  val flush = stage3.io.redirect.valid && stage3.io.redirect.bits.mispred

  val piplineReg1_valid = RegInit(false.B)
  val piplineReg1_data  = RegInit(0.U.asTypeOf(stage1.io.out.bits))
  when(stage2.io.in.fire || !piplineReg1_valid || flush){
    piplineReg1_valid := stage1.io.out.valid
    piplineReg1_data  := stage1.io.out.bits
  }
  stage1.io.out.ready := stage2.io.in.fire || !piplineReg1_valid || flush
  stage2.io.in.valid := piplineReg1_valid && !flush
  stage2.io.in.bits  := piplineReg1_data

  val piplineReg2_valid = RegInit(false.B)
  val piplineReg2_data  = RegInit(0.U.asTypeOf(stage2.io.out.bits))
  when(stage3.io.in.fire || !piplineReg2_valid || flush){
    piplineReg2_valid := stage2.io.out.valid && !flush
    piplineReg2_data  := stage2.io.out.bits
  }
  stage2.io.out.ready := stage3.io.in.fire || !piplineReg2_valid || flush
  stage3.io.in.valid := piplineReg2_valid
  stage3.io.in.bits  := piplineReg2_data

  
  stage1.io.redirect <> stage3.io.redirect
  stage2.io.wb       <> stage3.io.wb


  //difftest
  io.valid     := stage3.io.wb.valid
  io.pc_instr.pc   := stage3.io.in.bits.fetch_info.pc
  io.pc_instr.inst := stage3.io.in.bits.fetch_info.inst
  io.regout.ena    := stage3.io.wb.bits.ena
  io.regout.addr   := stage3.io.wb.bits.addr
  io.regout.data   := stage3.io.wb.bits.data
}