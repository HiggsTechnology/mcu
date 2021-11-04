package Core.TOP

import Core.SimTop
import chisel3.stage._

object TopMain extends App {
  (new ChiselStage).execute(
    args,
    Seq(
      ChiselGeneratorAnnotation(() => new SimTop())
    )
  )
}
