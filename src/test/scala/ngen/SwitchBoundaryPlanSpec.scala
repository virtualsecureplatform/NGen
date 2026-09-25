package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.transform.{NttPlan, ReferenceNtt, SwitchBoundaryPlan}
import org.scalatest.funsuite.AnyFunSuite

class SwitchBoundaryPlanSpec extends AnyFunSuite:
  test("physical square transposes preserve natural external stream semantics"):
    val domain = NttDomain("q17", 16, Modulus(17), 3, TransformShape.Cyclic)
    val input = Vector.tabulate(16)(i => BigInt(i * 7 - 4))
    def transpose(values: Vector[BigInt]): Vector[BigInt] = Vector.tabulate(16)(index => values((index % 4) * 4 + index / 4))
    val corePlan = SwitchBoundaryPlan(NttPlan.radix2(domain, inverse = false), 4)
    assert(transpose(corePlan.evaluate(transpose(input))) == ReferenceNtt.forward(domain, input))

  test("rectangular rate-preserving transposes use inverse boundary permutations"):
    val domain = NttDomain("q17", 16, Modulus(17), 3, TransformShape.Cyclic)
    val lanes = 2
    val cycles = domain.size / lanes
    val input = Vector.tabulate(domain.size)(i => BigInt(i * 7 - 4))
    def transpose(values: Vector[BigInt]): Vector[BigInt] =
      Vector.tabulate(domain.size)(index => values((index % cycles) * lanes + index / cycles))
    val forward = SwitchBoundaryPlan(NttPlan.radix2(domain, inverse = false), lanes)
    val inverse = SwitchBoundaryPlan(NttPlan.radix2(domain, inverse = true), lanes)
    val transformed = transpose(forward.evaluate(transpose(input)))
    assert(transformed == ReferenceNtt.forward(domain, input))
    assert(transpose(inverse.evaluate(transpose(transformed))) == ReferenceNtt.inverse(domain, transformed))
