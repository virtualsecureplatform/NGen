package ngen

import ngen.arithmetic.YataField
import ngen.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class TimedGraphSpec extends AnyFunSuite:
  test("builder inserts explicit delays on shorter paths"):
    val graph = YataTimedButterfly.build(YataField.R, PipelineProfile.Baseline)
    assert(graph.latency == 4)
    val delays = graph.nodes.collect { case Node(_, delay: Delay, _) => delay.cycles }
    assert(delays == Vector(3, 3))
    graph.outputs.foreach(output => assert(output.availableAt == 4))

  test("timed butterfly evaluation ignores delays but preserves arithmetic"):
    val twiddle = YataField.R
    val graph = YataTimedButterfly.build(twiddle, PipelineProfile.Baseline)
    val result = graph.evaluate(Map("left" -> BigInt(17), "right" -> BigInt(9)))
    val product = YataField.multiplySigned(9, twiddle)
    assert(result == Vector(BigInt(YataField.addMod(17, product)), BigInt(YataField.subtractMod(17, product))))

  test("DOT output includes operator timing and delay nodes"):
    val dot = YataTimedButterfly.build(31, PipelineProfile.Baseline).toDot
    assert(dot.startsWith("digraph timed_rtl"))
    assert(dot.contains("yata_mul_sredc"))
    assert(dot.contains("delay:3"))
    assert(dot.contains("t=4"))

  test("missing graph inputs are rejected"):
    val graph = YataTimedButterfly.build(1, PipelineProfile.Baseline)
    assertThrows[IllegalArgumentException](graph.evaluate(Map("left" -> BigInt(1))))

  test("f300 profile deepens multiplication and reduction without changing arithmetic"):
    val baseline = YataTimedButterfly.build(31, PipelineProfile.Baseline)
    val f300 = YataTimedButterfly.build(31, PipelineProfile.F300)
    assert(baseline.latency == 4)
    assert(f300.latency == 6)
    val inputs = Map("left" -> BigInt(123), "right" -> BigInt(-456))
    assert(baseline.evaluate(inputs) == f300.evaluate(inputs))
