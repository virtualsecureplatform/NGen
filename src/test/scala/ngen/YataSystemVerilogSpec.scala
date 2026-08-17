package ngen

import ngen.backend.YataSystemVerilog
import org.scalatest.funsuite.AnyFunSuite

class YataSystemVerilogSpec extends AnyFunSuite:
  test("radix-8 backend emits the requested benchmark top"):
    val rtl = YataSystemVerilog.emitRadix8("CandidateTop")
    assert(rtl.contains("module CandidateTop("))
    assert(rtl.contains("input [31:0] io_intt_in_7"))
    assert(rtl.contains("output reg [31:0] io_ntt_out_7"))
    assert(rtl.contains("27'sd11337725"))
    assert(rtl.contains("27'sd16777216"))
    assert(!rtl.contains(" % "))

  test("backend rejects an invalid module identifier"):
    assertThrows[IllegalArgumentException](YataSystemVerilog.emitRadix8("bad-name"))
