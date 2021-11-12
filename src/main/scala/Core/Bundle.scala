package Core


import Core.Stage2.{FuncOpType, FuncType, SrcType}
import chisel3._
class Redirect extends CoreBundle {
  val new_pc  = UInt(AddrWidth)
  val mispred = Bool()
}

class FetchInfo extends CoreBundle {
  val pc    = UInt(AddrWidth)
  val inst = UInt(InstWidth)
}

trait HasPred {
  val pre_pc = Output(UInt(Config.AddrWidth))
}

class FetchPreInfo extends FetchInfo with HasPred{}


class WriteBack extends CoreBundle {
  val addr = UInt(RegNumWidth.W)
  val data = UInt(XLEN.W)
  val ena  = Bool()
}

class CtrlSignal extends CoreBundle {
  val src_type     = Vec(2, SrcType.uwidth)
  val func_type    = FuncType.uwidth
  val func_optype  = FuncOpType.uwidth
  val rfSrc        = Vec(2,UInt(RegNumWidth.W)) //src regfile address//logic
  val rfrd         = UInt(RegNumWidth.W)    //rd regfile address
  val rfWen        = Bool()       //regfile write enable
}

class DataSrc extends CoreBundle {
  val imm      = UInt(XLEN.W)
  val src_data = Vec(2, UInt(XLEN.W))
  val uimm_ext = UInt(XLEN.W)
}

class MicroOp extends CoreBundle{
  val fetch_info   = new FetchPreInfo
  val ctrl = new CtrlSignal
  val data = new DataSrc
}

class Fu extends CoreBundle{
  val src = Vec(2,(UInt(XLEN.W)))
  val optype = (FuncOpType.uwidth)
  val functype = (FuncType.uwidth)
}

class LSU2MemIO extends  Bundle with Config{
  /** r:0, w:1 */
  val valid     = Output(Bool())
  val rready    = Input(Bool())
  val wready    = Input(Bool())
  val is_write  = Output(Bool())
  val addr      = Output(UInt(XLEN.W))
  val rdata     = Input(UInt(XLEN.W))
  val wdata     = Output(UInt(XLEN.W))
  val strb      = Output(UInt(8.W))
}

class IFU2MemIO extends  Bundle with Config{
  val valid     = Output(Bool())
  val ready     = Input(Bool())
  val addr        = Output(UInt(XLEN.W))
  val rdata     = Input(UInt((XLEN * 4).W))
}