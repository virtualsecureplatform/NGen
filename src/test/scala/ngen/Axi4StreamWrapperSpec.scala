package ngen

import ngen.backend.Axi4StreamWrapper
import org.scalatest.funsuite.AnyFunSuite

class Axi4StreamWrapperSpec extends AnyFunSuite:
  test("AXI4-Stream wrapper packs lanes and generates transaction TLAST"):
    val core="module Core(input clock,input reset,input in_valid,output in_ready,output out_valid,input out_ready,input[7:0]i0,input[7:0]i1,output[7:0]o0,output[7:0]o1);endmodule"
    val rtl=Axi4StreamWrapper.emit(core,"AxisTop","Core",2,8,4)
    assert(rtl.contains("s_axis_tvalid"))
    assert(rtl.contains("m_axis_tlast"))
    assert(!rtl.contains("axi4lite"))
