package Core

import chisel3._


class RegfileFunc(numReg: Int) extends Config {
  val regs = RegInit(VecInit(Seq.fill(numReg)(0.U(XLEN.W))))

  def write(addr: UInt, data: UInt): Unit = {
    when(addr =/= 0.U) {
      regs(addr) := data
    }
    ()
  }

  def read(addr: UInt): UInt = {
    regs(addr)
  }
}

class RegReadIO extends Bundle with Config {
  val addr = Input(UInt(RegNumWidth.W))
  val data = Output(UInt(XLEN.W))
}

class RegWriteIO extends Bundle with Config {
  val addr = Input(UInt(RegNumWidth.W))
  val data = Input(UInt(XLEN.W))
  val ena  = Input(Bool())
}

class RegfileIO(numReadPorts: Int, numWritePorts: Int) extends Bundle {
  val read  = Vec(numReadPorts, new RegReadIO)
  val write = Vec(numWritePorts, new RegWriteIO)
}

class Regfile(numReadPorts: Int, numWritePorts: Int, numReg: Int) extends Module {
  val io = IO(new RegfileIO(numReadPorts, numWritePorts))
  val regfile = new RegfileFunc(32)

  for(i <- 0 until numWritePorts){
    when(io.write(i).ena) {
      regfile.write(io.write(i).addr, io.write(i).data)
    }
  }

  for(i <- 0 until numReadPorts){
    io.read(i).data := Mux(io.read(i).addr === 0.U, 0.U, regfile.read(io.read(i).addr))
  }
  val mod = Module(new difftest.DifftestArchIntRegState)
  mod.io.clock := clock
  mod.io.coreid := 0.U
  mod.io.gpr := RegNext(regfile.regs)

}