package Core.Stage3

import Core.Stage2.LSUOpType
import Core.{CoreModule, Fu, LookupTree, SignExt, ZeroExt}
import chisel3._
import chisel3.util._

class Lsu extends CoreModule {

  def genWmask(sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> 0x1.U, //0001 << addr(2:0) 1111 1111
      "b01".U -> 0x3.U, //0011              1111 1111 1111 1111
      "b10".U -> 0xf.U, //1111              1111 1111 1111 1111 1111 1111 1111 1111
      "b11".U -> 0xff.U //11111111
    )).asUInt()
  }

  def genWdata(data: UInt, sizeEncode: UInt): UInt = {
    LookupTree(sizeEncode, List(
      "b00".U -> ZeroExt(data(7, 0) , XLEN),
      "b01".U -> ZeroExt(data(15, 0), XLEN),
      "b10".U -> ZeroExt(data(31, 0), XLEN),
      "b11".U -> ZeroExt(data(63, 0), XLEN)
    ))
  }
  val io = IO(new Bundle{
    val in = Input(new Fu)
    val res = ValidIO(UInt(DataWidth))
    val memBus = Output//TODO
  })
  val lsu_functype = io.in.functype
  val lsu_optype = io.in.optype
  val src1 = Wire(UInt(XLEN.W))
  val src2 = Wire(UInt(XLEN.W))


  val addr = RegInit(src1)
  addr := src1
  val storedata = src1
  val isStore = LSUOpType.isStore(lsu_optype)
  val size = lsu_optype(1,0)
  val wdata_align = genWdata(storedata, size) << (addr(2, 0) << 3.U)
  val mask_align = genWmask(size) << (addr(2, 0))
//  TODO
//  mem_req_addr := src1
//  mem_req_data := src2
  val mem_resp = Wire(UInt(DataWidth))
  mem_resp := 0.U// TODO io.toMem.resp.bits.data
  val rdata_sel =  mem_resp >> (addr(2, 0) << 3.U)
  io.res.bits := LookupTree(lsu_optype, List(
    LSUOpType.lb   -> SignExt(rdata_sel(7, 0) , XLEN),
    LSUOpType.lh   -> SignExt(rdata_sel(15, 0), XLEN),
    LSUOpType.lw   -> SignExt(rdata_sel(31, 0), XLEN),
    LSUOpType.ld   -> SignExt(rdata_sel(63, 0), XLEN),
    LSUOpType.lbu  -> ZeroExt(rdata_sel(7, 0) , XLEN),
    LSUOpType.lhu  -> ZeroExt(rdata_sel(15, 0), XLEN),
    LSUOpType.lwu  -> ZeroExt(rdata_sel(31, 0), XLEN)
  ))


}
