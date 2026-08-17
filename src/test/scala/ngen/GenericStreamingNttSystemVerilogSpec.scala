package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.backend.GenericStreamingNttSystemVerilog
import ngen.transform.NttPlan
import org.scalatest.funsuite.AnyFunSuite

class GenericStreamingNttSystemVerilogSpec extends AnyFunSuite:
  private val domain = NttDomain("q17", 8, Modulus(17), 9, TransformShape.Cyclic)

  test("scheduler maps every stage to width-bounded independent bundles"):
    val plan = NttPlan.radix2(domain, inverse = false)
    val schedule = GenericStreamingNttSystemVerilog.schedule(plan, streamingWidth = 2)
    assert(schedule.inputCycles == 4)
    assert(schedule.outputCycles == 4)
    assert(schedule.bundleGap == 0)
    assert(schedule.bundles.size == 6)
    assert(schedule.bundles.forall(_.size <= 2))
    assert(schedule.bundles.forall(bundle => bundle.flatMap(op => Vector(op.left, op.right)).distinct.size == 2 * bundle.size))
    assert(schedule.latency == 10)
    assert(schedule.initiationInterval == 13)

  test("f300 profile inserts an idle cycle between streamed bundles"):
    val schedule = GenericStreamingNttSystemVerilog.schedule(NttPlan.radix2(domain, inverse = false), 2, ngen.rtl.ProfileName.F300)
    assert(schedule.bundleGap == 1)
    assert(schedule.latency == 15)
    assert(schedule.initiationInterval == 18)

  test("backend emits streamed ports, static twiddles, storage, and ready"):
    val rtl = GenericStreamingNttSystemVerilog.emit(NttPlan.radix2(domain, inverse = false), 2, "StreamedNtt")
    assert(rtl.contains("module StreamedNtt("))
    assert(rtl.contains("input [4:0] i1"))
    assert(!rtl.contains("input [4:0] i2"))
    assert(rtl.contains("output ready"))
    assert(rtl.contains("reg [4:0] work [0:N-1]"))
    assert(rtl.contains("localparam integer BUNDLE_COUNT = 6"))
    assert(rtl.contains("BARRETT_MU"))
    assert(rtl.contains("function automatic [4:0] field_mul"))
    assert(!rtl.contains(" % "))

  test("backend specializes constant multiplication for Montgomery reduction"):
    val rtl = GenericStreamingNttSystemVerilog.emit(NttPlan.radix2(domain, inverse = false), 2, reduction = ngen.rtl.ReductionKind.Montgomery)
    assert(rtl.contains("MONTGOMERY_QINV"))
    assert(!rtl.contains("BARRETT_MU"))

  test("backend emits preconditioned Shoup constant multiplication"):
    val rtl = GenericStreamingNttSystemVerilog.emit(NttPlan.radix2(domain, inverse = false), 2, reduction = ngen.rtl.ReductionKind.Shoup)
    assert(rtl.contains("input [4:0] b_shoup"))
    assert(rtl.contains("approximate_product"))
    assert(!rtl.contains("BARRETT_MU"))
