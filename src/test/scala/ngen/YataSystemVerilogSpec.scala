package ngen

import ngen.backend.YataStreamingSystemVerilog
import org.scalatest.funsuite.AnyFunSuite

class YataSystemVerilogSpec extends AnyFunSuite:
  test("radix-8 backend emits the requested benchmark top"):
    val rtl = YataStreamingSystemVerilog.emit(3, 3, "CandidateTop")
    assert(rtl.contains("module CandidateTop("))
    assert(rtl.contains("input [31:0] io_intt_in_7"))
    assert(rtl.contains("output reg [31:0] io_ntt_out_7"))
    assert(rtl.contains("27'sd11337725"))
    assert(rtl.contains("27'sd16777216"))
    assert(!rtl.contains(" % "))

  test("backend rejects an invalid module identifier"):
    assertThrows[IllegalArgumentException](YataStreamingSystemVerilog.emit(3, 3, "bad-name"))

  test("streaming backend emits all characterized YATA points"):
    val small = YataStreamingSystemVerilog.emit(3, 3, "SmallYata8RainttP27Rtl")
    val medium = YataStreamingSystemVerilog.emit(6, 3, "SmallYata8x8RainttP27Rtl")
    val large = YataStreamingSystemVerilog.emit(9, 6, "YataRainttTop")
    assert(small.contains("localparam integer YATA_N = 8"))
    assert(medium.contains("localparam integer YATA_CYCLES = 8"))
    assert(large.contains("localparam integer YATA_N = 512"))
    assert(large.contains("io_intt_in_63"))
