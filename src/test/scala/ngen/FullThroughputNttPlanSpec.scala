package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.transform.{FullThroughputNttPlan, NttPlan, ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class FullThroughputNttPlanSpec extends AnyFunSuite:
  test("recursive full-throughput stages preserve the canonical NTT"):
    val domain = NttDomain("q17",8,Modulus(17),9,TransformShape.Cyclic)
    val input = Vector.tabulate(8)(i => BigInt(i * 3 + 1))
    val plan = FullThroughputNttPlan.build(NttPlan.radix2(domain,inverse=false),4)
    assert(plan.evaluate(input) == ReferenceNtt.forward(domain,input))
    assert(plan.latency == 3)
    assert(plan.initiationInterval == 2)
