package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.rtl.PeNttSchedule
import ngen.transform.{IncompleteNttPlan, NttPlan, ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class PeNttScheduleSpec extends AnyFunSuite:
  private val cyclic = NttDomain("q97", 16, Modulus(97), 8, TransformShape.Cyclic)
  private val incomplete = NttDomain("q17-incomplete", 8, Modulus(17), 9, TransformShape.IncompleteNegacyclic(2))

  test("bank mapping separates every operand of radix-2/4/8 operations"):
    Vector(1, 2, 3).foreach { radixLog =>
      val schedule = PeNttSchedule.build(NttPlan.radix2(cyclic, inverse = false), radixLog, requestedPeCount = 2)
      schedule.bundles.foreach(_.operations.foreach { operation =>
        assert(operation.inputs.map(schedule.mapping.bank).distinct.size == operation.inputs.size)
      })
    }

  test("scheduled reusable-PE operations preserve fused plans"):
    val input = Vector.tabulate(cyclic.size)(i => BigInt(i * i - 9))
    Vector(1, 2, 3).foreach { radixLog =>
      val schedule = PeNttSchedule.build(NttPlan.radix2(cyclic, inverse = false), radixLog, requestedPeCount = 2)
      assert(schedule.evaluate(input) == ReferenceNtt.forward(cyclic, input))
    }

  test("incomplete forward and inverse plans schedule on reusable radix-2 PEs"):
    val input = Vector.tabulate(incomplete.size)(i => BigInt(i * 5 - 7))
    val forward = PeNttSchedule.build(IncompleteNttPlan(incomplete, inverse = false), 1, 2)
    val transformed = forward.evaluate(input)
    assert(transformed == ReferenceNtt.forward(incomplete, input))
    val inverse = PeNttSchedule.build(IncompleteNttPlan(incomplete, inverse = true), 1, 2)
    assert(inverse.evaluate(transformed) == input.map(incomplete.modulus.normalize))
