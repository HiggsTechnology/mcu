package Core.Stage2


import Core.{CoreBundle, CoreModule, FetchInfo, LookupTree, MicroOp, SignExt, ZeroExt}
import chisel3._
import chisel3.util._
import RVIInstr._

class IduIO extends CoreBundle{
  val in = Flipped(DecoupledIO(new FetchInfo))
  val out = Decoupled(new MicroOp)
}

class Idu extends CoreModule{
  val io = IO(new IduIO)
  private val inst = io.in.bits.inst
  val (src1Addr, src2Addr, rdAddr) = (inst(19, 15), inst(24, 20), inst(11, 7))
  val decodeList = ListLookup(inst, RVIInstr.defaultInst, RVIInstr.table)
  val instrType :: funcType :: funcOpType :: src1Type :: src2Type :: Nil = decodeList
  val uimm : UInt = inst(19, 15)
  private val uimm_ext = Mux((funcType === FuncType.csr) & CsrOpType.isCsri(funcOpType),
    ZeroExt(uimm, XLEN), 0.U
  )
  private val imm = LookupTree(instrType, List(
    InstrI  -> SignExt(inst(31, 20), XLEN),
    InstrS  -> SignExt(Cat(inst(31, 25), inst(11, 7)), XLEN),
    InstrB  -> SignExt(Cat(inst(31), inst(7), inst(30, 25), inst(11, 8), 0.U(1.W)), XLEN),
    InstrU  -> SignExt(Cat(inst(31, 12), 0.U(12.W)), XLEN),//fixed
    InstrJ  -> SignExt(Cat(inst(31), inst(19, 12), inst(20), inst(30, 21), 0.U(1.W)), XLEN)
  ))
  io.out.valid                   := io.in.valid //|| interruptValid      // 插入中断指令不依赖于IFU的输出
  io.out.bits.fetch_info         <> io.in.bits
  io.out.bits.ctrl.rfSrc(0)      := Mux(src1Type === SrcType.reg, src1Addr, 0.U)  //保证取到的地址均为有效寄存器地址，若无效则置0
  io.out.bits.ctrl.rfSrc(1)      := Mux(src2Type === SrcType.reg, src2Addr, 0.U)
  io.out.bits.ctrl.rfrd          := Mux(isrfWen(instrType), rdAddr, 0.U)
  io.out.bits.ctrl.func_type     := funcType
  io.out.bits.ctrl.func_optype   := funcOpType
  io.out.bits.ctrl.rfWen         := isrfWen(instrType) && (rdAddr =/= 0.U)
  io.out.bits.ctrl.src_type(0)   := src1Type
  io.out.bits.ctrl.src_type(1)   := src2Type
  io.out.bits.data.imm           := imm
  io.out.bits.data.uimm_ext      := uimm_ext
  io.out.bits.data.src_data      := DontCare
  io.in.ready := io.out.ready


}