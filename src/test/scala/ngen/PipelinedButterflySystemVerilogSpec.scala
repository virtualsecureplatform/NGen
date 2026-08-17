package ngen

import ngen.algebra.Modulus
import ngen.backend.PipelinedButterflySystemVerilog
import ngen.rtl.ReductionKind
import org.scalatest.funsuite.AnyFunSuite

class PipelinedButterflySystemVerilogSpec extends AnyFunSuite:
  test("all generic reductions emit a three-stage tagged butterfly pipeline"):
    Vector(ReductionKind.Barrett, ReductionKind.Montgomery, ReductionKind.Shoup).foreach { reduction =>
      val rtl = PipelinedButterflySystemVerilog.emit(Modulus(12289), reduction)
      assert(rtl.contains("parameter TAG_WIDTH=1"))
      assert(rtl.contains("valid_0"))
      assert(rtl.contains("valid_1"))
      assert(rtl.contains("tag_out<=tag_1"))
    }
    assert(PipelinedButterflySystemVerilog.Latency == 3)

  test("runtime Barrett pipeline exposes modulus and reciprocal inputs"):
    val rtl=PipelinedButterflySystemVerilog.emit(Modulus(12289),ReductionKind.Barrett,runtimeField=true)
    assert(rtl.contains("input [13:0] modulus_in"))
    assert(rtl.contains("input [27:0] reduction_constant_in"))
