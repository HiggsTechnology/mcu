package Core.Stage2

import utils.LookupTree
import Core.{Config, CoreBundle, FetchInfo, FetchPreInfo, MicroOp, Regfile, WriteBack}
import chisel3.util._
import chisel3._



class Stage2IO extends CoreBundle {
  val in = Flipped(DecoupledIO(new FetchPreInfo))
  val out = DecoupledIO(new MicroOp)
  val wb = Flipped(ValidIO(new WriteBack))
}

class Stage2 extends Module with Config {
  val io = IO(new Stage2IO)
  val idu = Module(new Idu)
  val reg = Module(new Regfile(numReadPorts = 2,numWritePorts = 1,numReg = RegNum))
  idu.io.in <> io.in
  idu.io.out.ready := io.out.ready
  private val reg_data = Wire(Vec(2,UInt(XLEN.W)))
  private val src_in = Wire(Vec(2,UInt(XLEN.W)))
  reg.io.read(0).addr := idu.io.out.bits.ctrl.rfSrc(0)
  reg.io.read(1).addr := idu.io.out.bits.ctrl.rfSrc(1)
  reg_data(0) := reg.io.read(0).data
  reg_data(1) := reg.io.read(1).data

  src_in(0) := LookupTree(idu.io.out.bits.ctrl.src_type(0), List(
    SrcType.reg  -> reg_data(0),
    SrcType.pc   -> idu.io.out.bits.fetch_info.pc,
    SrcType.uimm -> idu.io.out.bits.data.uimm_ext
  ))
  src_in(1) := LookupTree(idu.io.out.bits.ctrl.src_type(1), List(
    SrcType.reg  -> reg_data(1),
    SrcType.imm  -> idu.io.out.bits.data.imm
  ))

  val need_forward = Wire(Vec(2,Bool()))
  for(i <- 0 until 2){
    need_forward(i) := (io.out.bits.ctrl.rfSrc(i) === io.wb.bits.addr) && io.out.bits.ctrl.src_type(i) === SrcType.reg && io.wb.bits.ena && io.wb.valid
  }


  io.out.bits.fetch_info <> io.in.bits
  io.out.bits.ctrl       <> idu.io.out.bits.ctrl
  io.out.bits.data       <> idu.io.out.bits.data
  for(i <- 0 until 2){
    io.out.bits.data.src_data(i) := Mux(need_forward(i),io.wb.bits.data,src_in(i))
  }
  io.out.valid := io.in.valid


  reg.io.write(0).data := io.wb.bits.data
  reg.io.write(0).ena  := io.wb.bits.ena && io.wb.valid
  reg.io.write(0).addr := io.wb.bits.addr


}