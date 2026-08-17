package ngen

import ngen.algebra.Modulus
import ngen.arithmetic.ShoupField
import org.scalatest.funsuite.AnyFunSuite

class ShoupFieldSpec extends AnyFunSuite:
  test("preconditioned multiplication agrees with exact field arithmetic"):
    Vector(Modulus(17), Modulus(97), Modulus(12289)).foreach { modulus =>
      val shoup = ShoupField(modulus)
      val samples = if modulus.q < 200 then 0 until modulus.q.toInt else 0 until 257
      for constant <- samples by math.max(1, samples.size / 31); value <- samples do
        assert(shoup.multiply(value, shoup.prepare(constant)) == modulus.multiply(value, constant))
    }
