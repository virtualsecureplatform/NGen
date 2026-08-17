package ngen

import ngen.backend.YataMicrocodedSystemVerilog
import ngen.rtl.ProfileName
import ngen.rtl.TransposeKind
import org.scalatest.funsuite.AnyFunSuite

class YataSystemVerilogSpec extends AnyFunSuite:
  test("radix-8 backend emits the requested benchmark top"):
    val rtl = YataMicrocodedSystemVerilog.emit(3, 3, ProfileName.Baseline, "CandidateTop")
    assert(rtl.contains("module CandidateTop("))
    assert(rtl.contains("input [31:0] io_intt_in_7"))
    assert(rtl.contains("output reg [31:0] io_ntt_out_7"))
    assert(rtl.contains("27'sd11337725"))
    assert(rtl.contains("27'sd16777216"))
    assert(!rtl.contains(" % "))

  test("backend rejects an invalid module identifier"):
    assertThrows[IllegalArgumentException](YataMicrocodedSystemVerilog.emit(3, 3, ProfileName.Baseline, "bad-name"))

  test("streaming backend emits all characterized YATA points"):
    val small = YataMicrocodedSystemVerilog.emit(3, 3, ProfileName.Baseline, "SmallYata8RainttP27Rtl")
    val medium = YataMicrocodedSystemVerilog.emit(6, 3, ProfileName.Baseline, "SmallYata8x8RainttP27Rtl")
    val large = YataMicrocodedSystemVerilog.emit(9, 6, ProfileName.F300, "YataRainttTop")
    assert(small.contains("I_LENGTH="))
    assert(medium.contains("localparam integer STEP_GAP=0"))
    assert(large.contains("localparam integer STEP_GAP=1"))
    assert(large.contains("io_intt_in_63"))

  test("switch transpose backend wraps the natural-order YATA core"):
    val rtl = YataMicrocodedSystemVerilog.emit(6,3,ProfileName.Baseline,"YataSwitch",TransposeKind.Switch)
    assert(rtl.contains("module NGenSwitchTransposeUnit_3"))
    assert(rtl.contains("module YataSwitchCore"))
    assert(rtl.contains("module YataSwitch("))
