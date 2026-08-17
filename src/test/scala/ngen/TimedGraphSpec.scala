package ngen

import ngen.arithmetic.YataField
import ngen.rtl.*
import org.scalatest.funsuite.AnyFunSuite

class TimedGraphSpec extends AnyFunSuite:
  test("builder inserts explicit delays on shorter paths"):
    val graph = YataTimedButterfly.build(YataField.R, PipelineProfile(addLatency = 1, multiplierLatency = 2, reductionLatency = 1))
    assert(graph.latency == 4)
    val delays = graph.nodes.collect { case Node(_, delay: Delay, _) => delay.cycles }
    assert(delays == Vector(3, 3))
    graph.outputs.foreach(output => assert(output.availableAt == 4))

  test("timed butterfly evaluation ignores delays but preserves arithmetic"):
    val twiddle = YataField.R
    val graph = YataTimedButterfly.build(twiddle, PipelineProfile())
    val result = graph.evaluate(Map("left" -> 17L, "right" -> 9L))
    val product = YataField.multiplySigned(9, twiddle)
    assert(result == Vector(YataField.addMod(17, product), YataField.subtractMod(17, product)))

  test("DOT output includes operator timing and delay nodes"):
    val dot = YataTimedButterfly.build(31, PipelineProfile()).toDot
    assert(dot.startsWith("digraph timed_rtl"))
    assert(dot.contains("yata_mul_sredc"))
    assert(dot.contains("delay:3"))
    assert(dot.contains("t=4"))

  test("missing graph inputs are rejected"):
    val graph = YataTimedButterfly.build(1, PipelineProfile())
    assertThrows[IllegalArgumentException](graph.evaluate(Map("left" -> 1L)))
