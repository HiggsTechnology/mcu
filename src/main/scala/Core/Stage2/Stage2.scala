package Core.Stage2

import Core.{CoreBundle, CoreModule, FetchInfo, LookupTree, MicroOp, Regfile}
import chisel3.util._
import chisel3._



class Stage2IO extends CoreBundle{
  val in = Flipped(DecoupledIO(new FetchInfo))
  val out = Decoupled(new MicroOp)
  val wb_data = Flipped(ValidIO(UInt(DataWidth)))
}

class Stage2 extends CoreModule{
  val io = IO(new Stage2IO)
  val idu = Module(new Idu)
  val reg = Module(new Regfile(numReadPorts = 1,numWritePorts = 1,numReg = RegNum))
  idu.io.in <> io.in
  private val reg_data = Wire(Vec(2,Vec(2,UInt(XLEN.W))))
  private val src_in = Wire(Vec(2,Vec(2,UInt(XLEN.W))))
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

  io.out.bits.fetch_info <> io.in.bits
  io.out.bits.ctrl       <> idu.io.out.bits.ctrl
  io.out.bits.data       <> idu.io.out.bits.data
  io.out.bits.data.src_data(0) <> src_in(0)
  io.out.bits.data.src_data(1) <> src_in(1)



}