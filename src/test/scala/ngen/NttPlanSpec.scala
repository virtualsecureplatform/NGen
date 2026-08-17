package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.transform.{DataOrder, IncompleteNttPlan, NttPlan, RadixFusionPlan, ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class NttPlanSpec extends AnyFunSuite:
  private val cyclic = NttDomain("q17", 8, Modulus(17), 9, TransformShape.Cyclic)
  private val negacyclic = NttDomain("q97", 8, Modulus(97), 64, TransformShape.Negacyclic, Some(8))
  private val incomplete = NttDomain("incomplete-q17", 8, Modulus(17), 9, TransformShape.IncompleteNegacyclic(2))

  test("radix-2 plan agrees with complete-transform reference"):
    Vector(cyclic, negacyclic).foreach { domain =>
      val input = Vector.tabulate(domain.size)(i => BigInt(i * i - 5))
      assert(NttPlan.radix2(domain, inverse = false).evaluate(input) == ReferenceNtt.forward(domain, input))
      val transformed = ReferenceNtt.forward(domain, input)
      assert(NttPlan.radix2(domain, inverse = true).evaluate(transformed) == input.map(domain.modulus.normalize))
    }

  test("bit-reversed stream orders preserve mathematical values"):
    val input = Vector.tabulate(cyclic.size)(BigInt(_))
    val bitReversedInput = Vector.tabulate(cyclic.size)(i => input(ReferenceNtt.bitReverse(i, cyclic.logSize)))
    val expected = ReferenceNtt.forward(cyclic, input)
    assert(NttPlan.radix2(cyclic, inverse = false, inputOrder = DataOrder.BitReversed).evaluate(bitReversedInput) == expected)
    val bitReversedOutput = NttPlan.radix2(cyclic, inverse = false, outputOrder = DataOrder.BitReversed).evaluate(input)
    assert(bitReversedOutput == Vector.tabulate(cyclic.size)(i => expected(ReferenceNtt.bitReverse(i, cyclic.logSize))))

  test("plan exposes independent butterflies per stage"):
    val plan = NttPlan.radix2(cyclic, inverse = false)
    assert(plan.stages.size == cyclic.logSize)
    assert(plan.stages.forall(_.butterflies.size == cyclic.size / 2))
    assert(plan.stages.forall(stage => stage.butterflies.flatMap(op => Vector(op.left, op.right)).distinct.size == cyclic.size))

  test("incomplete plan generalizes the Kyber traversal to other sizes"):
    val input = Vector.tabulate(incomplete.size)(i => BigInt(i * i - 4))
    val transformed = IncompleteNttPlan(incomplete, inverse = false).evaluate(input)
    val recovered = IncompleteNttPlan(incomplete, inverse = true).evaluate(transformed)
    assert(recovered == input.map(incomplete.modulus.normalize))
    assert(IncompleteNttPlan.zetas(incomplete).size == 4)

  test("radix fusion preserves the complete NTT plan"):
    val input = Vector.tabulate(cyclic.size)(i => BigInt(i * 3 - 2))
    val base = NttPlan.radix2(cyclic, inverse = false)
    Vector(1, 2, 3).foreach(radixLog => assert(RadixFusionPlan(base, radixLog).evaluate(input) == base.evaluate(input)))
    val radix4 = RadixFusionPlan(base, 2)
    assert(radix4.stages.head.radixLog == 2)
    assert(radix4.stages.head.blocks.forall(_.indices.size == 4))
