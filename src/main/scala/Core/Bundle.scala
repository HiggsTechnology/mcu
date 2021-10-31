package Core


import Core.Stage2.{FuncOpType, FuncType, SrcType}
import chisel3._
class Redirect extends CoreBundle {
  val new_pc  = UInt(AddrWidth)
  val mispred = Bool()
}

class FetchInfo extends CoreBundle{
  val pc    = UInt(AddrWidth)
  val inst = UInt(InstWidth)
}

class CtrlSignal extends CoreBundle {
  val src_type     = Vec(2, SrcType.uwidth)
  val func_type    = FuncType.uwidth
  val func_optype  = FuncOpType.uwidth
  val rfSrc        = Vec(2,UInt(5.W)) //src regfile address//logic
  val rfrd         = UInt(5.W)    //rd regfile address
  val rfWen        = Bool()       //regfile write enable
  //  val interruptVec = Vec(TrapConfig.InterruptVecWidth, OutBool())
  //  val exceptionVec = Vec(TrapConfig.ExceptionVecWidth, OutBool())
}

class DataSrc extends Bundle with Config {
  val imm      = UInt(XLEN.W)
  val src_data = Vec(2, UInt(XLEN.W))
  val uimm_ext = UInt(XLEN.W)
}

class MicroOp extends CoreBundle{
  val fetch_info   = new FetchInfo
  val ctrl = new CtrlSignal
  val data = new DataSrc
}

class Fu extends CoreBundle{
  val src = Vec(2,(UInt(XLEN.W)))
  val optype = (FuncOpType.uwidth)
  val functype = (FuncType.uwidth)
}
