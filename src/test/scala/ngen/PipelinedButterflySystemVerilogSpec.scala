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
