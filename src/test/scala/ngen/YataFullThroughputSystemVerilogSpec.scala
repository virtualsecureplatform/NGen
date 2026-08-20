package ngen

import ngen.backend.YataFullThroughputSystemVerilog
import ngen.rtl.{ProfileName,StreamProtocol}
import org.scalatest.funsuite.AnyFunSuite

class YataFullThroughputSystemVerilogSpec extends AnyFunSuite:
  test("YATA full-throughput emits one native recursive stage pipeline"):
    val rtl=YataFullThroughputSystemVerilog.emit(6,3,ProfileName.Baseline,"YataFull")
    assert(rtl.contains("module YataFullRecursiveStage0"))
    assert(rtl.contains("PIPELINE_DEPTH=3"))
    assert(!rtl.contains("io_in_ready"))
    assert(!rtl.contains("io_out_ready"))
    assert(!rtl.contains("if(1'b1)"))

  test("elastic control is emitted only for ready-valid"):
    val rtl=YataFullThroughputSystemVerilog.emit(6,3,ProfileName.Baseline,"YataElastic",StreamProtocol.ReadyValid)
    assert(rtl.contains("output io_in_ready"))
    assert(rtl.contains("input io_out_ready"))
    assert(rtl.contains("assign io_in_ready="))
