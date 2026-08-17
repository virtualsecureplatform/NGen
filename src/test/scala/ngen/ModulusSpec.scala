package ngen

import ngen.algebra.Modulus
import org.scalatest.funsuite.AnyFunSuite

class ModulusSpec extends AnyFunSuite:
  private val field = Modulus(17)

  test("normalization and arithmetic stay in the field"):
    assert(field.normalize(-1) == 16)
    assert(field.add(16, 2) == 1)
    assert(field.subtract(1, 2) == 16)
    assert(field.multiply(8, 8) == 13)

  test("inversion is exact"):
    for value <- 1 until 17 do
      assert(field.multiply(value, field.inverse(value)) == 1)

  test("exact power-of-two order distinguishes proper roots"):
    assert(field.hasExactPowerOfTwoOrder(9, 8))
    assert(!field.hasExactPowerOfTwoOrder(16, 8))

  test("automatic root discovery returns the requested order"):
    val larger = Modulus(97)
    val root = larger.findPowerOfTwoRoot(16)
    assert(larger.hasExactPowerOfTwoOrder(root, 16))
