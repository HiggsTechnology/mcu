package Core.Stage3
import Core.{CoreModule, Fu, LookupTree}
import Core.Stage2.{ALUOpType}
import chisel3._
import chisel3.util._

class Alu extends CoreModule{
  val io = IO(new Bundle{
    val in = Input(new Fu)
    val res = Output(UInt(AddrWidth))
  })
  io.in.functype := DontCare
  val alu_optype = io.in.optype
  val src1 = Wire(UInt(XLEN.W))
  val src2 = Wire(UInt(XLEN.W))
  src1 := io.in.src(0)
  src2 := io.in.src(1)
  val shamt = src2(4, 0)
  io.res := LookupTree(alu_optype, List(
    ALUOpType.add   ->   (src1 + src2),
    ALUOpType.sll   ->   (src1 << shamt),
    ALUOpType.slt   ->   Cat(0.U((XLEN - 1).W), src1.asSInt() < src2.asSInt()),
    ALUOpType.sltu  ->   (src1 < src2),
    ALUOpType.xor   ->   (src1 ^ src2),
    ALUOpType.srl   ->   (src1 >> shamt),
    ALUOpType.or    ->   (src1 | src2),
    ALUOpType.and   ->   (src1 & src2),
    ALUOpType.sub   ->   (src1 - src2),
    ALUOpType.sra   ->   (src1.asSInt >> shamt).asUInt,
    ALUOpType.lui  ->    src2
  ))
}