package ngen

import ngen.backend.HogeFullThroughputSystemVerilog
import ngen.rtl.{ProfileName, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class HogeFullThroughputSystemVerilogSpec extends AnyFunSuite:
  test("full-throughput HOGE is a recursive two-pass switch pipeline"):
    val rtl = HogeFullThroughputSystemVerilog.emit("HogeFT", false, ProfileName.Baseline, TransposeKind.Switch)
    assert(HogeFullThroughputSystemVerilog.StreamCycles == 32)
    assert(HogeFullThroughputSystemVerilog.RadixPipelineDepth == 5)
    assert(rtl.contains("module HogeForwardRadix32Pipeline"))
    assert(rtl.contains("HogeFTNGenSwitchTransposeNetwork_5 transpose1"))
    assert(rtl.contains("HogeFTNGenSwitchTransposeNetwork_5 transpose2"))
    assert(rtl.contains("assign io_ready=1'b1"))

  test("full-throughput HOGE rejects a non-streaming transpose"):
    assertThrows[IllegalArgumentException](
      HogeFullThroughputSystemVerilog.emit("HogeFT", false, ProfileName.Baseline, TransposeKind.Indexed)
    )

  test("inverse full-throughput HOGE uses the golden former radix split"):
    val rtl = HogeFullThroughputSystemVerilog.emit("HogeIFT", true, ProfileName.Baseline, TransposeKind.Switch)
    assert(rtl.contains("module HogeFormerInverseRadix32Pipeline"))
    assert(rtl.contains("module HogeInverseRadix32Pipeline"))
    assert(rtl.contains("input [1023:0] io_in"))
