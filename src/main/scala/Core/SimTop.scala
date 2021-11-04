package Core
import chisel3._
import chisel3.util._
import difftest._

class SimTopIO extends Bundle {
  val logCtrl = new LogCtrlIO
  val perfInfo = new PerfInfoIO
  val uart = new UARTIO
}

class SimTop extends Module {
  val io : SimTopIO = IO(new SimTopIO)
  io.uart.in.valid  := false.B
  io.uart.out.valid := false.B
  io.uart.out.ch    := 0.U
  val EleCore       = Module(new EleCore)


  val instrCommit = Module(new DifftestInstrCommit)
  instrCommit.io.clock := clock
  instrCommit.io.coreid := 0.U
  instrCommit.io.index := 0.U
  instrCommit.io.skip := false.B
  instrCommit.io.isRVC := false.B
  instrCommit.io.scFailed := false.B

  instrCommit.io.valid := RegNext(RegNext(EleCore.io.valid))
  instrCommit.io.pc    := RegNext(RegNext(EleCore.io.pc_instr.pc))

  instrCommit.io.instr := RegNext(RegNext(EleCore.io.pc_instr.inst))

  instrCommit.io.wen   := RegNext(RegNext(EleCore.io.regout.ena))
  instrCommit.io.wdata := RegNext(RegNext(EleCore.io.regout.data))
  instrCommit.io.wdest := RegNext(RegNext(EleCore.io.regout.addr))


  val csrCommit = Module(new DifftestCSRState)
  csrCommit.io.clock          := clock
  csrCommit.io.priviledgeMode := 0.U
  csrCommit.io.mstatus        := 0.U
  csrCommit.io.sstatus        := 0.U
  csrCommit.io.mepc           := 0.U
  csrCommit.io.sepc           := 0.U
  csrCommit.io.mtval          := 0.U
  csrCommit.io.stval          := 0.U
  csrCommit.io.mtvec          := 0.U
  csrCommit.io.stvec          := 0.U
  csrCommit.io.mcause         := 0.U
  csrCommit.io.scause         := 0.U
  csrCommit.io.satp           := 0.U
  csrCommit.io.mip            := 0.U
  csrCommit.io.mie            := 0.U
  csrCommit.io.mscratch       := 0.U
  csrCommit.io.sscratch       := 0.U
  csrCommit.io.mideleg        := 0.U
  csrCommit.io.medeleg        := 0.U

  val trap = Module(new DifftestTrapEvent)
  trap.io.clock    := clock
  trap.io.coreid   := 0.U
  trap.io.valid    := RegNext(RegNext(EleCore.io.pc_instr.inst)) === BigInt("0000006b", 16).U
  trap.io.code     := 0.U // GoodTrap
  trap.io.pc       := RegNext(RegNext(EleCore.io.pc_instr.pc))
  trap.io.cycleCnt := 0.U
  trap.io.instrCnt := 0.U
}
