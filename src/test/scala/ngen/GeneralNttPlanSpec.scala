package ngen

import ngen.algebra.Modulus
import ngen.transform.{GeneralNttAlgorithm, GeneralNttDomain, GeneralNttPlan}
import org.scalatest.funsuite.AnyFunSuite

class GeneralNttPlanSpec extends AnyFunSuite:
  test("mixed-radix general plan round trips a six-point transform"):
    val domain = GeneralNttDomain("six", 6, Modulus(13), 4)
    val input = Vector.tabulate(6)(i => BigInt(i * i - 3))
    val forward = GeneralNttPlan(domain, inverse = false)
    assert(forward.algorithm == GeneralNttAlgorithm.MixedRadix)
    assert(forward.radixFactors == Vector(2, 3))
    assert(GeneralNttPlan(domain, inverse = true).evaluate(forward.evaluate(input)) == input.map(domain.modulus.normalize))

  test("prime transform sizes select Bluestein planning"):
    val domain = GeneralNttDomain("five", 5, Modulus(11), 3)
    assert(GeneralNttPlan(domain, inverse = false).algorithm == GeneralNttAlgorithm.Bluestein)

  test("four-step plans accept explicit factors"):
    val domain = GeneralNttDomain("eight", 8, Modulus(17), 9)
    val plan = GeneralNttPlan(domain, inverse = false, GeneralNttAlgorithm.FourStep, Some((2, 4)))
    val input = Vector.tabulate(8)(i => BigInt(i))
    assert(plan.algorithm == GeneralNttAlgorithm.FourStep)
    assert(plan.resolvedFourStepFactors.contains((2, 4)))
    assert(plan.evaluate(input) == GeneralNttPlan(domain, inverse = false).evaluate(input))
