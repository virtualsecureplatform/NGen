package ngen

import ngen.backend.YataPipelinedSystemVerilog
import ngen.rtl.{ProfileName, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class YataPipelinedSystemVerilogSpec extends AnyFunSuite:
  test("stage-parallel YATA groups each radix layer into a registered stage"):
    assert(YataPipelinedSystemVerilog.stageCounts(6) == (3, 3))
    assert(YataPipelinedSystemVerilog.stageCounts(9) == (4, 4))

  test("stage-parallel YATA emits the dual streaming interface"):
    val rtl = YataPipelinedSystemVerilog.emit(6, 3, ProfileName.Baseline, "YataPipe", TransposeKind.Indexed)
    assert(rtl.contains("stage-parallel YATA"))
    assert(rtl.contains("localparam integer I_LENGTH=3"))
    assert(rtl.contains("module YataPipelinedModSwitch"))

  test("stage-parallel YATA keeps switch transpose explicit until its boundary is pipelined"):
    assertThrows[IllegalArgumentException](YataPipelinedSystemVerilog.emit(6, 3, ProfileName.Baseline, "YataPipe", TransposeKind.Switch))
