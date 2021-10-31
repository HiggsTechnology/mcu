package Core.Stage3

import Core.Stage2.{ALUOpType, BRUOpType, FuncType}
import Core.{CoreBundle, CoreModule, MicroOp, Redirect}
import chisel3._
import chisel3.util._


class ExuIO extends CoreBundle {
  val in = Flipped(Decoupled(new MicroOp))
  val redirect = new Redirect
  val jmp_pc = ValidIO(UInt(AddrWidth))
  val wb_data = ValidIO(UInt(DataWidth))
}

class Exu extends CoreModule{
  val io = IO(new ExuIO)
  val alu = new Alu
  val bru = new Bru
  val lsu = new Lsu
// alu
  val func_optype = io.in.bits.ctrl.func_optype
  for(i<-0 until 2){
    alu.io.in.src(i) := io.in.bits.data.src_data(i)
  }
  alu.io.in.optype := Mux(func_optype === FuncType.alu, io.in.bits.ctrl.func_optype, ALUOpType.add)
  switch(func_optype){
    is(FuncType.alu){
      alu.io.in.optype := io.in.bits.ctrl.func_optype
      for(i<-0 until 2){
        alu.io.in.src(i) := io.in.bits.data.src_data(i)
      }
    }
    is(FuncType.bru){
      alu.io.in.optype := ALUOpType.add
      alu.io.in.src(0) := Mux(func_optype===BRUOpType.jalr, Cat(io.in.bits.data.src_data(0)(XLEN - 1,1), 0.U(1.W)), io.in.bits.fetch_info.pc)
      alu.io.in.src(1) := io.in.bits.data.imm
    }
    is(FuncType.lsu){
      alu.io.in.optype := ALUOpType.add
      alu.io.in.src(0) := io.in.bits.data.src_data(0)
      alu.io.in.src(1) := io.in.bits.data.imm
    }
  }
  val alu_res = alu.io.res
  // bru
  bru.io.in.optype := io.in.bits.ctrl.func_optype
  bru.io.in.functype := io.in.bits.ctrl.func_type
  bru.io.in.src(0) := io.in.bits.data.src_data(0)
  bru.io.in.src(1) := io.in.bits.data.src_data(1)
  io.jmp_pc.valid := bru.io.bru_taken
  io.jmp_pc.bits  := alu_res

  // lsu
  lsu.io.in.optype := io.in.bits.ctrl.func_optype
  lsu.io.in.functype := io.in.bits.ctrl.func_type
  lsu.io.in.src(0) := alu_res
  lsu.io.in.src(1) := io.in.bits.data.src_data(1)
  io.wb_data.valid := true.B//TODO
  io.wb_data.bits  := lsu.io.res.bits
}
