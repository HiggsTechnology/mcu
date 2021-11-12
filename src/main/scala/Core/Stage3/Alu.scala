package Core.Stage3
import Core.{Config, Fu}
import Core.Stage2.ALUOpType
import utils.{LookupTree, SignExt}
import chisel3._
import chisel3.util._

class Alu extends Module with Config {
  val io = IO(new Bundle{
    val in = Input(new Fu)
    val res = Output(UInt(AddrWidth))
  })
  val alu_optype = io.in.optype
  val src1 = Wire(UInt(XLEN.W))
  val src2 = Wire(UInt(XLEN.W))
  src1 := io.in.src(0)
  src2 := io.in.src(1)
  val shamt = Mux(ALUOpType.isWordOp(alu_optype), src2(4, 0), src2(5, 0))
  val res = LookupTree(alu_optype, List(
    ALUOpType.add   ->   (src1 + src2),
    ALUOpType.sll   ->   (src1 << shamt),
    ALUOpType.slt   ->   (Cat(0.U((XLEN - 1).W), src1.asSInt() < src2.asSInt())),
    ALUOpType.sltu  ->   (src1 < src2),
    ALUOpType.xor   ->   (src1 ^ src2),
    ALUOpType.srl   ->   (src1 >> shamt),
    ALUOpType.or    ->   (src1 | src2),
    ALUOpType.and   ->   (src1 & src2),
    ALUOpType.sub   ->   (src1 - src2),
    ALUOpType.sra   ->   ((src1.asSInt >> shamt).asUInt),
    ALUOpType.addw  ->   (src1 + src2),
    ALUOpType.subw  ->   (src1 - src2),
    ALUOpType.sllw  ->   (src1 << shamt),
    ALUOpType.srlw  ->   (src1(31,0) >> shamt),
    ALUOpType.sraw  ->   ((src1(31,0).asSInt() >> shamt).asUInt()),
    ALUOpType.lui  ->    (src2)
  ))
  io.res := Mux(ALUOpType.isWordOp(alu_optype), SignExt(res(31,0), 64), res)
}
