package core.stage2

import chisel3._
import chisel3.util._
object ALUOpType {
  def add  = "b1000000".U
  def sll  = "b0000001".U
  def slt  = "b0000010".U
  def sltu = "b0000011".U
  def xor  = "b0000100".U
  def srl  = "b0000101".U
  def or   = "b0000110".U
  def and  = "b0000111".U
  def sub  = "b0001000".U
  def sra  = "b0001101".U
  def lui  = "b0001111".U

  def addw = "b1100000".U
  def subw = "b0101000".U
  def sllw = "b0100001".U
  def srlw = "b0100101".U
  def sraw = "b0101101".U

  def isWordOp(func: UInt) = func(5)  //if 32bit
}
object BRUOpType {
  def jal  = "b1011000".U
  def jalr = "b1011010".U
  def beq  = "b0010000".U
  def bne  = "b0010001".U
  def blt  = "b0010100".U
  def bge  = "b0010101".U
  def bltu = "b0010110".U
  def bgeu = "b0010111".U
  def isJalr(func: UInt): Bool = func(6)
}
object LSUOpType {
  def lb   = "b0000000".U
  def lh   = "b0000001".U
  def lw   = "b0000010".U
  def ld   = "b0000011".U
  def lbu  = "b0000100".U
  def lhu  = "b0000101".U
  def lwu  = "b0000110".U
  def sb   = "b0001000".U
  def sh   = "b0001001".U
  def sw   = "b0001010".U
  def sd   = "b0001011".U

  def lr   = "b0100000".U
  def sc   = "b0100001".U

  def isStore(func: UInt): Bool = func(3)
  def isLoad(func: UInt): Bool = !isStore(func)
  def isLR(func: UInt): Bool = func === lr
  def isSC(func: UInt): Bool = func === sc
  def needMemRead(func: UInt): Bool  = isLoad(func) || isLR(func)
  def needMemWrite(func: UInt): Bool = isStore(func) || isSC(func)
}
object MDUOpType {
  def mul    = "b0000".U
  def mulh   = "b0001".U
  def mulhsu = "b0010".U
  def mulhu  = "b0011".U
  def div    = "b0100".U
  def divu   = "b0101".U
  def rem    = "b0110".U
  def remu   = "b0111".U

  def mulw   = "b1000".U
  def divw   = "b1100".U
  def divuw  = "b1101".U
  def remw   = "b1110".U
  def remuw  = "b1111".U

  def isDiv(op: UInt) = op(2)
  def isDivSign(op: UInt) = isDiv(op) && !op(0)
  def isW(op: UInt) = op(3)
  def isRem(op: UInt) = op(2) && op(1)
}
object CsrOpType {
  def RW        : UInt = "b000001".U(FuncOpType.width)
  def RS        : UInt = "b000010".U(FuncOpType.width)
  def RC        : UInt = "b000011".U(FuncOpType.width)
  def RWI       : UInt = "b000101".U(FuncOpType.width)
  def RSI       : UInt = "b000110".U(FuncOpType.width)
  def RCI       : UInt = "b000111".U(FuncOpType.width)
  def ECALL     : UInt = "b010000".U(FuncOpType.width)
  def EBREAK    : UInt = "b010001".U(FuncOpType.width)
  def MRET      : UInt = "b011000".U(FuncOpType.width)
  def SRET      : UInt = "b011001".U(FuncOpType.width)
  def URET      : UInt = "b011011".U(FuncOpType.width)
  def INTERRUPT : UInt = "b100000".U(FuncOpType.width)
  def isJmp(op: UInt)       : Bool = op(4).asBool()
  def isRet(op: UInt)       : Bool = op(3).asBool()
  def isCsri(op: UInt)      : Bool = op(2).asBool()
  def isCall(op: UInt)      : Bool = op === ECALL
  def isInterrupt(op :UInt) : Bool = op(5).asBool()
}

object MOUOpType {
  def fence  = "b00".U
  def fencei = "b01".U
  def sfence_vma = "b10".U
}


object RV32I_ALUInstr extends InstrType {
  def ADDI    = BitPat("b????????????_?????_000_?????_0010011")
  def SLLI    = BitPat("b000000??????_?????_001_?????_0010011")
  def SLTI    = BitPat("b????????????_?????_010_?????_0010011")
  def SLTIU   = BitPat("b????????????_?????_011_?????_0010011")
  def XORI    = BitPat("b????????????_?????_100_?????_0010011")
  def SRLI    = BitPat("b000000??????_?????_101_?????_0010011")
  def ORI     = BitPat("b????????????_?????_110_?????_0010011")
  def ANDI    = BitPat("b????????????_?????_111_?????_0010011")
  def SRAI    = BitPat("b010000??????_?????_101_?????_0010011")

  def ADD     = BitPat("b0000000_?????_?????_000_?????_0110011")
  def SLL     = BitPat("b0000000_?????_?????_001_?????_0110011")
  def SLT     = BitPat("b0000000_?????_?????_010_?????_0110011")
  def SLTU    = BitPat("b0000000_?????_?????_011_?????_0110011")
  def XOR     = BitPat("b0000000_?????_?????_100_?????_0110011")
  def SRL     = BitPat("b0000000_?????_?????_101_?????_0110011")
  def OR      = BitPat("b0000000_?????_?????_110_?????_0110011")
  def AND     = BitPat("b0000000_?????_?????_111_?????_0110011")
  def SUB     = BitPat("b0100000_?????_?????_000_?????_0110011")
  def SRA     = BitPat("b0100000_?????_?????_101_?????_0110011")

  def AUIPC   = BitPat("b????????????????????_?????_0010111")
  def LUI     = BitPat("b????????????????????_?????_0110111")

  val table = Array(
    ADDI           -> List(InstrI, FuncType.alu, ALUOpType.add  , SrcType1.reg, SrcType2.imm),
    SLLI           -> List(InstrI, FuncType.alu, ALUOpType.sll  , SrcType1.reg, SrcType2.imm),
    SLTI           -> List(InstrI, FuncType.alu, ALUOpType.slt  , SrcType1.reg, SrcType2.imm),
    SLTIU          -> List(InstrI, FuncType.alu, ALUOpType.sltu , SrcType1.reg, SrcType2.imm),
    XORI           -> List(InstrI, FuncType.alu, ALUOpType.xor  , SrcType1.reg, SrcType2.imm),
    SRLI           -> List(InstrI, FuncType.alu, ALUOpType.srl  , SrcType1.reg, SrcType2.imm),
    ORI            -> List(InstrI, FuncType.alu, ALUOpType.or   , SrcType1.reg, SrcType2.imm),
    ANDI           -> List(InstrI, FuncType.alu, ALUOpType.and  , SrcType1.reg, SrcType2.imm),
    SRAI           -> List(InstrI, FuncType.alu, ALUOpType.sra  , SrcType1.reg, SrcType2.imm),

    ADD            -> List(InstrR, FuncType.alu, ALUOpType.add  , SrcType1.reg, SrcType2.reg),
    SLL            -> List(InstrR, FuncType.alu, ALUOpType.sll  , SrcType1.reg, SrcType2.reg),
    SLT            -> List(InstrR, FuncType.alu, ALUOpType.slt  , SrcType1.reg, SrcType2.reg),
    SLTU           -> List(InstrR, FuncType.alu, ALUOpType.sltu , SrcType1.reg, SrcType2.reg),
    XOR            -> List(InstrR, FuncType.alu, ALUOpType.xor  , SrcType1.reg, SrcType2.reg),
    SRL            -> List(InstrR, FuncType.alu, ALUOpType.srl  , SrcType1.reg, SrcType2.reg),
    OR             -> List(InstrR, FuncType.alu, ALUOpType.or   , SrcType1.reg, SrcType2.reg),
    AND            -> List(InstrR, FuncType.alu, ALUOpType.and  , SrcType1.reg, SrcType2.reg),
    SUB            -> List(InstrR, FuncType.alu, ALUOpType.sub  , SrcType1.reg, SrcType2.reg),
    SRA            -> List(InstrR, FuncType.alu, ALUOpType.sra  , SrcType1.reg, SrcType2.reg),

    AUIPC          -> List(InstrU, FuncType.alu, ALUOpType.add  , SrcType1.pc , SrcType2.imm),
    LUI            -> List(InstrU, FuncType.alu, ALUOpType.lui  , SrcType1.pc , SrcType2.imm)
  )
}

object RV32I_BRUInstr extends InstrType {
  def JAL     = BitPat("b????????????????????_?????_1101111")
  def JALR    = BitPat("b????????????_?????_000_?????_1100111")

  def BNE     = BitPat("b???????_?????_?????_001_?????_1100011")
  def BEQ     = BitPat("b???????_?????_?????_000_?????_1100011")
  def BLT     = BitPat("b???????_?????_?????_100_?????_1100011")
  def BGE     = BitPat("b???????_?????_?????_101_?????_1100011")
  def BLTU    = BitPat("b???????_?????_?????_110_?????_1100011")
  def BGEU    = BitPat("b???????_?????_?????_111_?????_1100011")

  val table = Array(
    JAL            -> List(InstrJ, FuncType.bru, BRUOpType.jal  , SrcType1.pc , SrcType2.imm),
    JALR           -> List(InstrI, FuncType.bru, BRUOpType.jalr , SrcType1.reg, SrcType2.imm),
    BEQ            -> List(InstrB, FuncType.bru, BRUOpType.beq  , SrcType1.reg, SrcType2.reg),
    BNE            -> List(InstrB, FuncType.bru, BRUOpType.bne  , SrcType1.reg, SrcType2.reg),
    BLT            -> List(InstrB, FuncType.bru, BRUOpType.blt  , SrcType1.reg, SrcType2.reg),
    BGE            -> List(InstrB, FuncType.bru, BRUOpType.bge  , SrcType1.reg, SrcType2.reg),
    BLTU           -> List(InstrB, FuncType.bru, BRUOpType.bltu , SrcType1.reg, SrcType2.reg),
    BGEU           -> List(InstrB, FuncType.bru, BRUOpType.bgeu , SrcType1.reg, SrcType2.reg)
  )
}

object RV32I_LSUInstr extends InstrType {
  def LB      = BitPat("b????????????_?????_000_?????_0000011")
  def LH      = BitPat("b????????????_?????_001_?????_0000011")
  def LW      = BitPat("b????????????_?????_010_?????_0000011")
  def LBU     = BitPat("b????????????_?????_100_?????_0000011")
  def LHU     = BitPat("b????????????_?????_101_?????_0000011")
  def SB      = BitPat("b???????_?????_?????_000_?????_0100011")
  def SH      = BitPat("b???????_?????_?????_001_?????_0100011")
  def SW      = BitPat("b???????_?????_?????_010_?????_0100011")

  val table = Array(
    LB             -> List(InstrI, FuncType.lsu, LSUOpType.lb , SrcType1.reg, SrcType2.imm),
    LH             -> List(InstrI, FuncType.lsu, LSUOpType.lh , SrcType1.reg, SrcType2.imm),
    LW             -> List(InstrI, FuncType.lsu, LSUOpType.lw , SrcType1.reg, SrcType2.imm),
    LBU            -> List(InstrI, FuncType.lsu, LSUOpType.lbu, SrcType1.reg, SrcType2.imm),
    LHU            -> List(InstrI, FuncType.lsu, LSUOpType.lhu, SrcType1.reg, SrcType2.imm),
    SB             -> List(InstrS, FuncType.lsu, LSUOpType.sb , SrcType1.reg, SrcType2.reg),
    SH             -> List(InstrS, FuncType.lsu, LSUOpType.sh , SrcType1.reg, SrcType2.reg),
    SW             -> List(InstrS, FuncType.lsu, LSUOpType.sw , SrcType1.reg, SrcType2.reg)
  )
}

object RV64IInstr extends InstrType {
  def ADDIW   = BitPat("b???????_?????_?????_000_?????_0011011")
  def SLLIW   = BitPat("b0000000_?????_?????_001_?????_0011011")
  def SRLIW   = BitPat("b0000000_?????_?????_101_?????_0011011")
  def SRAIW   = BitPat("b0100000_?????_?????_101_?????_0011011")
  def SLLW    = BitPat("b0000000_?????_?????_001_?????_0111011")
  def SRLW    = BitPat("b0000000_?????_?????_101_?????_0111011")
  def SRAW    = BitPat("b0100000_?????_?????_101_?????_0111011")
  def ADDW    = BitPat("b0000000_?????_?????_000_?????_0111011")
  def SUBW    = BitPat("b0100000_?????_?????_000_?????_0111011")

  def LWU     = BitPat("b???????_?????_?????_110_?????_0000011")
  def LD      = BitPat("b???????_?????_?????_011_?????_0000011")
  def SD      = BitPat("b???????_?????_?????_011_?????_0100011")

  val table = Array(
    ADDIW          -> List(InstrI, FuncType.alu, ALUOpType.addw , SrcType1.reg, SrcType2.imm),
    SLLIW          -> List(InstrI, FuncType.alu, ALUOpType.sllw , SrcType1.reg, SrcType2.imm),
    SRLIW          -> List(InstrI, FuncType.alu, ALUOpType.srlw , SrcType1.reg, SrcType2.imm),
    SRAIW          -> List(InstrI, FuncType.alu, ALUOpType.sraw , SrcType1.reg, SrcType2.imm),
    SLLW           -> List(InstrR, FuncType.alu, ALUOpType.sllw , SrcType1.reg, SrcType2.reg),
    SRLW           -> List(InstrR, FuncType.alu, ALUOpType.srlw , SrcType1.reg, SrcType2.reg),
    SRAW           -> List(InstrR, FuncType.alu, ALUOpType.sraw , SrcType1.reg, SrcType2.reg),
    ADDW           -> List(InstrR, FuncType.alu, ALUOpType.addw , SrcType1.reg, SrcType2.reg),
    SUBW           -> List(InstrR, FuncType.alu, ALUOpType.subw , SrcType1.reg, SrcType2.reg),

    LWU            -> List(InstrI, FuncType.lsu, LSUOpType.lwu  , SrcType1.reg, SrcType2.imm),
    LD             -> List(InstrI, FuncType.lsu, LSUOpType.ld   , SrcType1.reg, SrcType2.imm),
    SD             -> List(InstrS, FuncType.lsu, LSUOpType.sd   , SrcType1.reg, SrcType2.reg)
  )
}

object RV64I_CSRInstr extends InstrType {
  //                              |------------|-----|func3|--rd-|-opcode-|
  val ECALL   : BitPat  = BitPat("b000000000000_00000__000__00000_1110011")
  val EBREAK  : BitPat  = BitPat("b000000000001_00000__000__00000_1110011")
  val MRET    : BitPat  = BitPat("b001100000010_00000__000__00000_1110011")
  val SRET    : BitPat  = BitPat("b000100000010_00000__000__00000_1110011")
  val WFI     : BitPat  = BitPat("b000100000101_00000__000__00000_1110011")
  //                              |----CSR-----|-rs1-|func3|--rd-|-opcode-|
  val CSRRW   : BitPat  = BitPat("b????????????_?????__001__?????_1110011")
  val CSRRS   : BitPat  = BitPat("b????????????_?????__010__?????_1110011")
  val CSRRC   : BitPat  = BitPat("b????????????_?????__011__?????_1110011")
  //                              |----CSR-----|-zimm|func3|--rd-|-opcode-|
  val CSRRWI  : BitPat  = BitPat("b????????????_?????__101__?????_1110011")
  val CSRRSI  : BitPat  = BitPat("b????????????_?????__110__?????_1110011")
  val CSRRCI  : BitPat  = BitPat("b????????????_?????__111__?????_1110011")

  val table : Array[(BitPat, List[UInt])] = Array (
    ECALL         ->  List( InstrI, FuncType.csr, CsrOpType.ECALL , SrcType1.reg  , SrcType2.imm ),
    EBREAK        ->  List( InstrI, FuncType.csr, CsrOpType.EBREAK, SrcType1.reg  , SrcType2.imm ),
    CSRRW         ->  List( InstrI, FuncType.csr, CsrOpType.RW    , SrcType1.reg  , SrcType2.imm ),
    CSRRS         ->  List( InstrI, FuncType.csr, CsrOpType.RS    , SrcType1.reg  , SrcType2.imm ),
    CSRRC         ->  List( InstrI, FuncType.csr, CsrOpType.RC    , SrcType1.reg  , SrcType2.imm ),
    CSRRWI        ->  List( InstrI, FuncType.csr, CsrOpType.RWI   , SrcType1.uimm , SrcType2.imm ),
    CSRRSI        ->  List( InstrI, FuncType.csr, CsrOpType.RSI   , SrcType1.uimm , SrcType2.imm ),
    CSRRCI        ->  List( InstrI, FuncType.csr, CsrOpType.RCI   , SrcType1.uimm , SrcType2.imm ),
    MRET          ->  List( InstrI, FuncType.csr, CsrOpType.MRET  , SrcType1.reg  , SrcType2.imm )
  )
}

object RV32M_Instr extends InstrType {
  def MUL     = BitPat("b0000001_?????_?????_000_?????_0110011")
  def MULH    = BitPat("b0000001_?????_?????_001_?????_0110011")
  def MULHSU  = BitPat("b0000001_?????_?????_010_?????_0110011")
  def MULHU   = BitPat("b0000001_?????_?????_011_?????_0110011")
  def DIV     = BitPat("b0000001_?????_?????_100_?????_0110011")
  def DIVU    = BitPat("b0000001_?????_?????_101_?????_0110011")
  def REM     = BitPat("b0000001_?????_?????_110_?????_0110011")
  def REMU    = BitPat("b0000001_?????_?????_111_?????_0110011")

  val table = Array(
    MUL            -> List(InstrR, FuncType.mdu, MDUOpType.mul    ,SrcType1.reg, SrcType2.reg),
    MULH           -> List(InstrR, FuncType.mdu, MDUOpType.mulh   ,SrcType1.reg, SrcType2.reg),
    MULHSU         -> List(InstrR, FuncType.mdu, MDUOpType.mulhsu ,SrcType1.reg, SrcType2.reg),
    MULHU          -> List(InstrR, FuncType.mdu, MDUOpType.mulhu  ,SrcType1.reg, SrcType2.reg),
    DIV            -> List(InstrR, FuncType.mdu, MDUOpType.div    ,SrcType1.reg, SrcType2.reg),
    DIVU           -> List(InstrR, FuncType.mdu, MDUOpType.divu   ,SrcType1.reg, SrcType2.reg),
    REM            -> List(InstrR, FuncType.mdu, MDUOpType.rem    ,SrcType1.reg, SrcType2.reg),
    REMU           -> List(InstrR, FuncType.mdu, MDUOpType.remu   ,SrcType1.reg, SrcType2.reg)
  )
}

object RV64M_Instr extends InstrType {
  def MULW    = BitPat("b0000001_?????_?????_000_?????_0111011")
  def DIVW    = BitPat("b0000001_?????_?????_100_?????_0111011")
  def DIVUW   = BitPat("b0000001_?????_?????_101_?????_0111011")
  def REMW    = BitPat("b0000001_?????_?????_110_?????_0111011")
  def REMUW   = BitPat("b0000001_?????_?????_111_?????_0111011")

  val table = Array(
    MULW           -> List(InstrR, FuncType.mdu, MDUOpType.mulw  ,SrcType1.reg, SrcType2.reg),
    DIVW           -> List(InstrR, FuncType.mdu, MDUOpType.divw  ,SrcType1.reg, SrcType2.reg),
    DIVUW          -> List(InstrR, FuncType.mdu, MDUOpType.divuw ,SrcType1.reg, SrcType2.reg),
    REMW           -> List(InstrR, FuncType.mdu, MDUOpType.remw  ,SrcType1.reg, SrcType2.reg),
    REMUW          -> List(InstrR, FuncType.mdu, MDUOpType.remuw ,SrcType1.reg, SrcType2.reg)
  )
}


object RVZifenceiInstr extends InstrType {
  def FENCEI = BitPat("b000000000000_00000_001_00000_0001111")

  val table = Array(
    FENCEI -> List(InstrB, FuncType.mou, MOUOpType.fencei ,SrcType1.pc, SrcType2.imm)
  )
}


object RVIInstr extends InstrType {
  val table : Array[(BitPat, List[UInt])] =
    RV32I_ALUInstr.table ++ RV32I_BRUInstr.table ++
      RV32I_LSUInstr.table ++ RV64IInstr.table ++ RV64I_CSRInstr.table ++
      RV64M_Instr.table ++ RV32M_Instr.table ++ RVZifenceiInstr.table
  val defaultInst = List(InstrN, FuncType.csr, CsrOpType.INTERRUPT, SrcType1.uimm, SrcType2.imm)
}