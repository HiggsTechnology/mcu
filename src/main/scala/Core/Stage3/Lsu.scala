package Core.Stage3

import Core.Stage2.LSUOpType
import utils.{LookupTree, SignExt, ZeroExt}
import Core.{Config, Fu, RAMHelper}
import chisel3._
import chisel3.util._

class Lsu extends Module with Config {
  // TODO : mask should be reconsidered when bits = 32
  def genWmask(addr: UInt, sizeEncode: UInt): UInt = {
    (LookupTree(sizeEncode, List(
      "b00".U -> BigInt(0xff).U(XLEN.W), //0001 << addr(2:0)
      "b01".U -> BigInt(0xffff).U(XLEN.W), //0011
      "b10".U -> BigInt(0xffffffffL).U(XLEN.W), //1111
      "b11".U -> (BigInt(Long.MaxValue) * 2 + 1).U(XLEN.W) //11111111
    )) ).asUInt()
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
    val valid = Input(Bool())
    val res = ValidIO(UInt(DataWidth))
    val memBus = Output//TODO
  })
  val lsu_functype = io.in.functype
  val lsu_optype = io.in.optype


  val addr      = io.in.src(0)
  val storedata = io.in.src(1)
  val isStore = LSUOpType.isStore(lsu_optype)
  val size = lsu_optype(1,0)
  val wdata_align = genWdata(storedata, size) << (addr(2, 0) * 8.U)
  val mask_align = genWmask(addr, size) << (addr(2, 0) * 8.U)

  val mem_resp = Wire(UInt(DataWidth))
  mem_resp := 0.U// TODO io.toMem.resp.bits.data
  val ram = Module(new RAMHelper)
  ram.io.clk  := clock
  ram.io.en   := io.valid
  ram.io.rIdx := (addr - PcStart.U) >> 3
  ram.io.wIdx := (addr - PcStart.U) >> 3
  ram.io.wen  := io.valid & isStore
  ram.io.wdata := wdata_align
  ram.io.wmask := mask_align
  val rdata_sel =  ram.io.rdata >> (addr(2, 0) * 8.U)
  io.res.bits := LookupTree(lsu_optype, List(
    LSUOpType.lb   -> SignExt(rdata_sel(7, 0) , XLEN),
    LSUOpType.lh   -> SignExt(rdata_sel(15, 0), XLEN),
    LSUOpType.lw   -> SignExt(rdata_sel(31, 0), XLEN),
    LSUOpType.ld   -> SignExt(rdata_sel(63, 0), XLEN),
    LSUOpType.lbu  -> ZeroExt(rdata_sel(7, 0) , XLEN),
    LSUOpType.lhu  -> ZeroExt(rdata_sel(15, 0), XLEN),
    LSUOpType.lwu  -> ZeroExt(rdata_sel(31, 0), XLEN)
  ))
  io.res.valid := io.valid   //ramhelper 是否需要延一拍

}
