package ngen

import ngen.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class FullThroughputSpec extends AnyFunSuite:
  test("composed streaming nodes accumulate latency and preserve full-throughput II"):
    val addOne = StreamingKernel("add-one", 4, 2, _.map(_ + 1))
    val reverse = StreamingPermutation("reverse", Vector(3,2,1,0), 1)
    val plan = FullThroughputPlan(StreamingShape(4,2), StreamingCompose(Vector(addOne,reverse)))
    assert(plan.evaluate(Vector(0,1,2,3)) == Vector(4,3,2,1))
    assert(plan.latency == 3)
    assert(plan.initiationInterval == 2)
    assert(plan.minimumGap == 0)

  test("streaming permutations reject non-bijections"):
    assertThrows[IllegalArgumentException](StreamingPermutation("bad",Vector(0,0),0))
