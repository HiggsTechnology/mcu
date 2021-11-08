package Core
import chisel3._

abstract class CoreBundle extends Bundle with Config {}

trait Config {
  val XLEN = 64
  val PcStart = "h80000000"
  val InstWidth = 32.W
  val AddrWidth = XLEN.W
  val DataWidth = XLEN.W
  val FetchWidth = 2
  val RegNum = 32
  val RegNumWidth = 5
}

object Config extends Config{}