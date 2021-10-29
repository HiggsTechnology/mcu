package core
import chisel3._

abstract class CoreBundle extends Bundle with Config
abstract class CoreModule extends Module with Config

trait Config {
  val XLEN = 32
  val PcStart = "h80000000"
  val InstWidth = 32.W
  val AddrWidth = XLEN.W
  val DataWidth = XLEN.W
  val FetchWidth = 2
  val RegNum = 32

  def PhyRegIdxWidth : Int = 7

}

object Config extends Config{}