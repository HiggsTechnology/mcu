package core


import chisel3.{Bool, Input, Output, UInt, Vec}
import core.stage2.{FuncOpType, FuncType, SrcType1, SrcType2}
import chisel3._
class RedirectIO extends CoreBundle {
  val new_pc = Output(UInt(AddrWidth))
  val mispred = Output(Bool())
}

class ICodeIO extends CoreBundle{
  val pc = Output(UInt(AddrWidth))
  val instr = Input(UInt(InstWidth))
}

class CtrlFlow extends CoreBundle{
  val pc = Output(UInt(AddrWidth))
  val instr = Output(UInt(InstWidth))
}

class CtrlSignalIO extends CoreBundle {
  val src1Type     = Output(SrcType1.uwidth)
  val src2Type     = Output(SrcType2.uwidth)
  val funcType     = Output(FuncType.uwidth)
  val funcOpType   = Output(FuncOpType.uwidth)
  val rfSrc        = Vec(2,Output(UInt(5.W))) //src regfile address//logic
  val rfrd         = Output(UInt(5.W))    //rd regfile address
  val rfWen        = Output(Bool())       //regfile write enable
  //  val interruptVec = Vec(TrapConfig.InterruptVecWidth, OutBool())
  //  val exceptionVec = Vec(TrapConfig.ExceptionVecWidth, OutBool())
}

class DataSrcIO extends Bundle with Config {
  val imm      = Output(UInt(XLEN.W))
  val uimm_ext = Output(UInt(XLEN.W))
}

class CfCtrl extends Bundle with Config {
  val cf   = new CtrlFlow
  val ctrl = new CtrlSignalIO
  val data = new DataSrcIO
}

