package ngen

import ngen.arithmetic.GeneralizedFermatField
import ngen.transform.ReferenceNtt
import org.scalatest.funsuite.AnyFunSuite

class GeneralizedFermatFieldSpec extends AnyFunSuite:
  test("generalized Fermat prime fields derive exact roots"):
    val field = GeneralizedFermatField(10, 1) // 10^2 + 1 = 101
    val domain = field.domain(2)
    domain.validate()
    val input = Vector.tabulate(4)(BigInt(_))
    assert(ReferenceNtt.inverse(domain, ReferenceNtt.forward(domain, input)) == input)

  test("base-power multiplication agrees with exact modular arithmetic"):
    val field = GeneralizedFermatField(6, 1) // 37
    for value <- 0 until 37; power <- 0 until field.rootOrder do
      assert(field.powerMultiply(value, power) == field.modulus.multiply(value, field.modulus.pow(6, power)))
