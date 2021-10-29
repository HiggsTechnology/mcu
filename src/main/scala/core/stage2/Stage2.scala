package core.stage2


import core.{CfCtrl, CoreBundle, CoreModule, CtrlFlow, LookupTree, Regfile}
import chisel3.util.DecoupledIO
import chisel3._


class Stage2IO extends CoreBundle{

  val in = Flipped(DecoupledIO(new CtrlFlow))
  val out =

}

class Stage2 extends CoreModule{
  val io = IO(new Stage2IO)
  val idu = Module(new IDU)
  val reg = Module(new Regfile(numReadPorts = 1,numWritePorts = 1,numReg = RegNum))
  idu.io.in <> io.in
  private val reg_data = Wire(Vec(2,Vec(2,UInt(XLEN.W))))
  private val src_in = Wire(Vec(2,Vec(2,UInt(XLEN.W))))
  reg.io.read(0).addr := idu.io.out.bits.ctrl.rfSrc(0)
  reg.io.read(1).addr := idu.io.out.bits.ctrl.rfSrc(1)
  reg_data(0) := reg.io.read(0).data
  reg_data(1) := reg.io.read(1).data

  src_in(0) := LookupTree(idu.io.out.bits.ctrl.src1Type, List(
    SrcType1.reg  -> reg_data(0),
    SrcType1.pc   -> idu.io.out.bits.cf.pc,
    SrcType1.uimm -> idu.io.out.bits.data.uimm_ext
  ))
  src_in(1) := LookupTree(idu.io.out.bits.ctrl.src2Type, List(
    SrcType2.reg  -> reg_data(1),
    SrcType2.imm  -> idu.io.out.bits.data.imm
  ))

}