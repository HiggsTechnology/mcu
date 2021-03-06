package Core.Stage3

//import Core.{Config, CoreModule, Fu, LookupTree, SignExt, ZeroExt}
import chisel3._
import chisel3.util._


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

//class MUIO extends Bundle {
//  val in  = Flipped(ValidIO(new FuInPut))
//  val out = ValidIO(new FuOutPut)
//  val flush = Input(Bool())
//}

class MulIO(val len: Int) extends Bundle {
  val in = Flipped(ValidIO(Vec(2, Output(SInt(len.W)))))
  val flush  = Input(Bool())
  val out = ValidIO(Output(SInt((len * 2).W)))
}

class FullAdder(n: Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(SInt(n.W))
    val b = Input(SInt(n.W))
    val cin = Input(SInt(n.W))
    val sum = Output(SInt(n.W))
    val cout = Output(SInt(n.W))
  })

  // Generate the sum
  val a_xor_b = io.a ^ io.b
  io.sum := a_xor_b ^ io.cin
  // Generate the carry
  val a_and_b = io.a & io.b
  val b_and_cin = io.b & io.cin
  val a_and_cin = io.a & io.cin
  io.cout := a_and_b | b_and_cin | a_and_cin
}


class Booth4WTMultiplier(slen: Int = 33) extends Module {
  val io = IO(new MulIO(len = slen))
  val len = slen - 1
  val fa  = Seq.fill(len/4 - 1)(Module(new FullAdder(2*slen)))

  val (x, y) = (io.in.bits(0), io.in.bits(1))

  val px = WireInit(VecInit(Seq.fill(len/4)(0.S((2*slen).W))))
  val prod = WireInit(VecInit(Seq.fill(len/4 + 1)(0.S((2*slen).W))))
  val y_ext = Cat(y.asSInt(),0.S(1.W))
  val y_comp = WireInit(VecInit(Seq.fill(len/4)(0.U((len/4).W))))

  val m8condi = WireInit(VecInit(Seq.fill(len/4)(false.B)))

  val mx0 = WireInit(VecInit(Seq.fill(len/4)(0.S((slen+3).W))))
  val mx1 = WireInit(VecInit(Seq.fill(len/4)(0.S((slen+3).W))))

  val x1condi = WireInit(VecInit(Seq.fill(len/4)(false.B)))
  val x2condi = WireInit(VecInit(Seq.fill(len/4)(false.B)))
  val x3condi = WireInit(VecInit(Seq.fill(len/4)(false.B)))
  val x2 = (x << 1.U).asSInt()
  val x3 = x + x2
  val xminus = WireInit(VecInit(Seq.fill(len/4)(0.S((slen+2).W))))
  for(j <- 0 until (len/4)) {
    val i = 4 * j + 1

    y_comp(j) := Mux(y_ext(i+3), (~y_ext(i+2, i-1)).asUInt(), y_ext(i+2, i-1).asUInt())

    m8condi(j) := (y_comp(j)(0) || y_comp(j)(1) || y_comp(j)(2)) && y_comp(j)(3)
    mx0(j) := x << (2.U(2.W) + m8condi(j).asUInt())

    x1condi(j) := y_comp(j)(0) ^ y_comp(j)(1)
    x2condi(j) := (y_comp(j)(0) || y_comp(j)(1)) ^ y_comp(j)(2)

    when(x1condi(j) && x2condi(j)) {
      xminus(j) := x3
    }.elsewhen(!x1condi(j) && x2condi(j)) {
      xminus(j) := x2
    }.elsewhen(x1condi(j) && !x2condi(j)){
      xminus(j) := x
    }.otherwise {
      xminus(j) := 0.S((slen+2).W)
    }

    mx1(j) := Mux(y_comp(j).orR, mx0(j) - xminus(j), 0.S((slen+3).W))

    printf("mx0 %d\n", mx0(j))
    printf("mx1 %d\n", mx1(j))

    px(j) := Mux(y_ext(i+3), -mx1(j), mx1(j))
    printf("px %d\n", px(j))
  }
  val mux1 = Mux(y_ext(32)^y_ext(33),x,0.S((slen).W))

  prod(0) := (px(0)).asSInt()
  prod(1) := (px(1)<<4).asSInt()
  prod(2) := (px(2)<<8).asSInt()
  prod(3) := (px(3)<<12).asSInt()
  prod(4) := (px(4)<<16).asSInt()
  prod(5) := (px(5)<<20).asSInt()
  prod(6) := (px(6)<<24).asSInt()
  prod(7) := (px(7)<<28).asSInt()
  prod(8) := (Mux(y_ext(32), mux1, -mux1)<<32).asSInt()

  //----------------------------WT Structure---------------------------
  //branch0
  fa(0).io.a   := prod(0)
  fa(0).io.b   := prod(1)
  fa(0).io.cin := prod(2)
  fa(1).io.a   := prod(3)
  fa(1).io.b   := prod(4)
  fa(1).io.cin := prod(5)
  fa(2).io.a   := prod(6)
  fa(2).io.b   := prod(7)
  fa(2).io.cin := prod(8)

  //branch1
  fa(3).io.a   := fa(0).io.sum
  fa(3).io.b   := fa(1).io.sum
  fa(3).io.cin := fa(2).io.sum
  fa(4).io.a   := fa(0).io.cout<<1
  fa(4).io.b   := fa(1).io.cout<<1
  fa(4).io.cin := fa(2).io.cout<<1

  //branch2
  fa(5).io.a   := fa(3).io.sum
  fa(5).io.b   := fa(4).io.sum
  fa(5).io.cin := fa(3).io.cout<<1

  //branch3
  fa(6).io.a   := fa(5).io.sum
  fa(6).io.b   := fa(4).io.cout<<1
  fa(6).io.cin := fa(5).io.cout<<1

  printf("fa(6).io.sum %d\n",fa(6).io.sum)
  printf("fa(6).io.cout %d\n",fa(6).io.cout)
  printf("fa(6).io.cout<<1 %d\n",fa(6).io.cout<<1)
  val sumpx = fa(6).io.sum + (fa(6).io.cout<<1)

  //   val sumpx = prod(0) + prod(1) + prod(2) + prod(3) +
  //     prod(4) + prod(5) + prod(6) + prod(7) + prod(8)

  printf("sumpx %d", sumpx)
  io.out.bits :=  sumpx
  io.out.valid := io.in.valid
}


//class MU extends Module with Config {
//  val io   = IO(new MUIO)
//  val mul  = Module(new Booth4WTMultiplier(slen=33))
//  //todo: extend UInt(32.W), SInt(32.W) as SInt(33.W)
//  val src = new MDUbit(UInt(XLEN.W))
//  val funcOpType = io.in.bits.uop.ctrl.funcOpType
//  val lastOp = RegEnable(io.in.bits.uop.ctrl.funcOpType,io.in.valid)
//  //val isDiv = MDUOpType.isDiv(funcOpType)
//  //val isDivSign = MDUOpType.isDivSign(funcOpType)
//  val isW = RegEnable(MDUOpType.isW(funcOpType),io.in.valid)
//
//  val src1 = Wire(UInt(XLEN.W))
//  val src2 = Wire(UInt(XLEN.W))
//  src1 := io.in.bits.src(0)
//  src2 := io.in.bits.src(1)
//
//  def isMinus(x:UInt):Bool = x(XLEN-1)  //?????????????????????????????????
//
//  mul.io.in.valid   := io.in.valid    //????????????????????????
//  mul.io.flush := io.flush
//
//  val (resMinus:Bool) = RegEnable(LookupTree(funcOpType, List(
//    MDUOpType.mul     ->   (isMinus(src1) ^ isMinus(src2)),
//    MDUOpType.mulh    ->   (isMinus(src1) ^ isMinus(src2)),
//    MDUOpType.mulhsu  ->   isMinus(src1),
//    MDUOpType.mulhu   ->   false.B,
//    MDUOpType.mulw    ->   (isMinus(src1) ^ isMinus(src2))
//  )),io.in.valid)
//  //  val (mul.io.in.bits) = LookupTree(funcOpType, List(
//  //    MDUOpType.mul     ->   (src1, src2),
//  //    MDUOpType.mulh    ->   (src.single(src1), src.single(src2)),
//  //    MDUOpType.mulhsu  ->   (src.single(src1), src2),
//  //    MDUOpType.mulhu   ->   (src1, src2),
//  //    MDUOpType.mulw    ->   (src1, src2)
//  //  ))
//  mul.io.in.bits(0) := LookupTree(funcOpType, List(
//    MDUOpType.mul     ->   src.abs(src1,64),
//    MDUOpType.mulh    ->   src.abs(src1,64),
//    MDUOpType.mulhsu  ->   src.abs(src1,64),
//    MDUOpType.mulhu   ->   src1,
//    MDUOpType.mulw    ->   src.abs(src1,64)
//  ))
//  mul.io.in.bits(1) := LookupTree(funcOpType, List(
//    MDUOpType.mul     ->   src.abs(src2,64),
//    MDUOpType.mulh    ->   src.abs(src2,64),
//    MDUOpType.mulhsu  ->   src2,
//    MDUOpType.mulhu   ->   src2,
//    MDUOpType.mulw    ->   src.abs(src2,64)
//  ))
//  val res1 = Mux(resMinus, -mul.io.out.bits, mul.io.out.bits)
//  val res = LookupTree(lastOp, List(
//    MDUOpType.mul     ->   res1(63,0),
//    MDUOpType.mulh    ->   res1(127,64),
//    MDUOpType.mulhsu  ->   res1(127,64),
//    MDUOpType.mulhu   ->   res1(127,64),
//    MDUOpType.mulw    ->   res1(31,0)
//  ))
//
//  io.out.bits.res := Mux(isW, SignExt(res(31,0), 64), res)//all val about out need RegEnable
//  io.out.bits.uop := RegEnable(io.in.bits.uop,io.in.valid)
//  io.out.valid := mul.io.out.valid//RegNext(io.in.valid && !io.flush) && mul.io.out.valid //????????????
//
//  //  printf("MU0v in.valid %d pc %x instr %x, io.out %d %x %x\n",io.in.valid,io.in.bits.uop.cf.pc,io.in.bits.uop.cf.instr,io.out.valid,io.out.bits.uop.cf.pc,io.out.bits.uop.cf.instr)
//  //  printf("MU1in src1 %d src2 %d, funcOpType %d,lastOp %d\n",src1,src2,funcOpType,lastOp)
//  //  printf("MU2S out.valid %d resU %d resS %d outres %d\n",mul.io.out.valid,mul.io.out.bits,res1,io.out.bits.res)
//  //  printf("=================================================================\n")
//}
//