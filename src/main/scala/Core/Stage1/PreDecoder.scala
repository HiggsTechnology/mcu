package Core.Stage1

import Core.{SignExt,CoreModule}
import chisel3._
import chisel3.util._

object BrType {
  def B = "b00".U  // branch
  def R = "b01".U  // jalr
  def N = "b10".U  // err instr
  def J = "b11".U  // jal
  def apply() = UInt(2.W)
}

class PreDecode extends CoreModule{
  val io = IO(new Bundle{
    val instr = Input(UInt(InstWidth))
    val is_br = Output(Bool())
    val offset = Output(UInt(XLEN.W))
    val br_type = Output(BrType())
    val is_ret = Output(Bool())
    val iscall = Output(Bool())
  })

  val instr = io.instr
  io.is_ret := false.B
  io.iscall := false.B

  when(instr(6,4)==="b110".U){
    val brtype = instr(3,2)
    io.br_type := brtype
    io.offset:= 4.U
    io.is_br    := false.B

    switch(brtype){
      is(BrType.J){
        io.offset   := SignExt(Cat(instr(31), instr(19,12), instr(20), instr(30,21), 0.U(1.W)), XLEN)
        io.is_br    := true.B
        io.iscall   := instr(11,7) === 1.U || instr(11,7) === 5.U
      }
      is(BrType.R){
        io.is_br    := true.B
        io.offset   := 4.U
        io.is_ret   := instr(11,7) === 0.U && (instr(19,15) === 1.U || instr(19,15) === 5.U)
        io.iscall   := instr(11,7) === 1.U || instr(11,7) === 5.U
      }
      is(BrType.B){
        io.is_br   := true.B
        io.offset   := SignExt(Cat(instr(31), instr(7), instr(30,25), instr(11,8), 0.U(1.W)), XLEN)

      }
      is(BrType.N){
        io.offset:= 4.U
        io.is_br    := false.B
      }
    }
  }.otherwise{
    io.is_br    := false.B
    io.offset   := 4.U
    io.br_type := DontCare
  }
}