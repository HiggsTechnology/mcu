package Core.Stage3

import Core.Stage2.{BRUOpType, FuncType}
import utils.LookupTree
import Core.{Config, Fu}
import chisel3._
import chisel3.util._


class Bru extends Module with Config {
  val io = IO(new Bundle{
    val in = Input(new Fu)
    val bru_taken = Output(Bool())
  })
  val bru_functype = io.in.functype
  val bru_optype = io.in.optype
  val src1 = Wire(UInt(XLEN.W))
  val src2 = Wire(UInt(XLEN.W))
  src1 := io.in.src(0)
  src2 := io.in.src(1)

  io.bru_taken := (bru_functype === FuncType.bru) && LookupTree(bru_optype, List(
    BRUOpType.jal   ->  (true.B),
    BRUOpType.jalr  ->  (true.B),
    BRUOpType.beq   ->  (src1 === src2),
    BRUOpType.bne   ->  (src1 =/= src2),
    BRUOpType.blt   ->  (src1.asSInt() < src2.asSInt()),
    BRUOpType.bge   ->  (src1.asSInt() >= src2.asSInt()),
    BRUOpType.bltu  ->  (src1 < src2),
    BRUOpType.bgeu  ->  (src1 >= src2)
  ))
}