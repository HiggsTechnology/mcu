package Core.Stage3

import Core.Stage2.{ALUOpType, BRUOpType, FuncType}
import Core.{Config, CoreBundle, MicroOp, Redirect, WriteBack}
import chisel3._
import chisel3.util._


class ExuIO extends CoreBundle {
  val in = Flipped(DecoupledIO(new MicroOp))
  val redirect = ValidIO(new Redirect)
  val wb = ValidIO(new WriteBack)
}

class Stage3 extends Module with Config {
  val io = IO(new ExuIO)
  val alu = Module(new Alu)
  val bru = Module(new Bru)
  val lsu = Module(new Lsu)

  io.in.ready := true.B
// alu
  val func_type = io.in.bits.ctrl.func_type
  val func_optype = io.in.bits.ctrl.func_optype
  for(i<-0 until 2){
    alu.io.in.src(i) := io.in.bits.data.src_data(i)
  }
  when(func_type === FuncType.alu) {
    alu.io.in.optype := io.in.bits.ctrl.func_optype
    for(i<-0 until 2){
      alu.io.in.src(i) := io.in.bits.data.src_data(i)
    }
  }.elsewhen(func_type === FuncType.bru) {
    alu.io.in.optype := ALUOpType.add
    alu.io.in.src(0) := Mux(func_optype===BRUOpType.jalr, Cat(io.in.bits.data.src_data(0)(XLEN - 1,1), 0.U(1.W)), io.in.bits.fetch_info.pc)
    alu.io.in.src(1) := io.in.bits.data.imm
  }.otherwise {
    alu.io.in.optype := ALUOpType.add
    alu.io.in.src(0) := io.in.bits.data.src_data(0)
    alu.io.in.src(1) := io.in.bits.data.imm
  }

  val alu_res = alu.io.res
  alu.io.in.functype       := io.in.bits.ctrl.func_type
  // bru
  bru.io.in.optype         := io.in.bits.ctrl.func_optype
  bru.io.in.functype       := io.in.bits.ctrl.func_type
  bru.io.in.src(0)         := io.in.bits.data.src_data(0)
  bru.io.in.src(1)         := io.in.bits.data.src_data(1)
  val bru_pc_res = Mux(bru.io.bru_taken, alu_res, io.in.bits.fetch_info.pc + 4.U)
  io.redirect.valid        := io.in.valid && func_type === FuncType.bru
  io.redirect.bits.new_pc  := bru_pc_res
  io.redirect.bits.mispred := bru_pc_res =/= io.in.bits.fetch_info.pre_pc
  //  printf("func_type %x\n", io.in.bits.ctrl.func_type)
  //  printf("bru_taken %d\n", bru.io.bru_taken)
  //  printf("alu_res %d\n", alu_res)
  //  printf("fech_pre_pc %x\n",io.in.bits.fetch_info.pre_pc)
  //  printf("#################################################\n")


  // lsu
  lsu.io.valid       := func_type === FuncType.lsu && io.in.valid
  lsu.io.in.optype   := io.in.bits.ctrl.func_optype
  lsu.io.in.functype := io.in.bits.ctrl.func_type
  lsu.io.in.src(0)   := alu_res
  lsu.io.in.src(1)   := io.in.bits.data.src_data(1)

  //printf("wb ena %d, addr %d, data %x, func_type %d\n",io.in.bits.ctrl.rfWen,io.in.bits.ctrl.rfrd,io.wb.bits.data,func_type)
  //printf("lsu res %x\n",lsu.io.res.bits)

  io.wb.valid     := io.in.valid
  io.wb.bits.ena  := io.in.bits.ctrl.rfWen
  io.wb.bits.addr := io.in.bits.ctrl.rfrd

  when(func_type === FuncType.alu) {
    io.wb.bits.data := alu_res
  }.elsewhen(func_type === FuncType.lsu){
    io.wb.bits.data := lsu.io.res.bits
  }.elsewhen(func_type === FuncType.bru){
    io.wb.bits.data := io.in.bits.fetch_info.pc + 4.U
  }.otherwise {
    io.wb.bits.data := DontCare
  }
}
