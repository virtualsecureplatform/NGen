package ngen

import ngen.backend.YataFullThroughputSystemVerilog
import ngen.rtl.ProfileName
import org.scalatest.funsuite.AnyFunSuite

class YataFullThroughputSystemVerilogSpec extends AnyFunSuite:
  test("YATA full-throughput shell emits three round-robin engines"):
    val rtl=YataFullThroughputSystemVerilog.emit(6,3,ProfileName.Baseline,"YataFull")
    assert(rtl.contains("module YataFullEngine("))
    assert(rtl.contains("YataFullEngine engine_2"))
    assert(rtl.contains("localparam integer STREAM_CYCLES=8,ENGINE_COUNT=3"))
