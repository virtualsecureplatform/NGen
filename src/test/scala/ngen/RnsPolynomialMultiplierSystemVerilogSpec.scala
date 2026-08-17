package ngen

import ngen.algebra.{Modulus,NttDomain,TransformShape}
import ngen.backend.RnsPolynomialMultiplierSystemVerilog
import ngen.transform.RnsBasis
import org.scalatest.funsuite.AnyFunSuite

class RnsPolynomialMultiplierSystemVerilogSpec extends AnyFunSuite:
  test("backend composes forward, pointwise, and inverse cores per RNS prime"):
    val basis=RnsBasis(Vector(
      NttDomain("q17",8,Modulus(17),9,TransformShape.Negacyclic,Some(3)),
      NttDomain("q97",8,Modulus(97),64,TransformShape.Negacyclic,Some(8))))
    val rtl=RnsPolynomialMultiplierSystemVerilog.emit(basis)
    assert(rtl.contains("module RnsPolynomialMultiplier("))
    assert(rtl.contains("RnsPolynomialMultiplierForwardA0"))
    assert(rtl.contains("rns_mul_1"))
    assert(rtl.contains("assign next_out=inv_valid_0&inv_valid_1"))
    val crt=RnsPolynomialMultiplierSystemVerilog.emit(basis,emitCrt=true)
    assert(crt.contains("output [10:0] crt_7"))
