package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.arithmetic.BarrettField
import ngen.rtl.{GenericNttGraph, PipelineProfile}
import ngen.transform.ReferenceNtt
import org.scalatest.funsuite.AnyFunSuite

class BarrettGraphSpec extends AnyFunSuite:
  private val domain = NttDomain("q17", 8, Modulus(17), 9, TransformShape.Cyclic)

  test("Barrett model agrees with exact modular multiplication"):
    val barrett = BarrettField(domain.modulus)
    for lhs <- -20 to 40; rhs <- -20 to 40 do
      assert(barrett.multiply(lhs, rhs) == domain.modulus.multiply(lhs, rhs))

  test("generic timed graph agrees with cyclic reference NTT"):
    val input = Vector.tabulate(8)(i => BigInt(i * i - 3))
    val graph = GenericNttGraph.build(domain, inverse = false, PipelineProfile.Baseline)
    val values = input.indices.map(i => s"i$i" -> input(i)).toMap
    assert(graph.evaluate(values) == ReferenceNtt.forward(domain, input))

  test("baseline and f300 generic graphs are arithmetically identical"):
    val input = Vector.tabulate(8)(i => BigInt(2 * i + 1))
    val values = input.indices.map(i => s"i$i" -> input(i)).toMap
    val baseline = GenericNttGraph.build(domain, inverse = true, PipelineProfile.Baseline)
    val f300 = GenericNttGraph.build(domain, inverse = true, PipelineProfile.F300)
    assert(baseline.evaluate(values) == f300.evaluate(values))
    assert(f300.latency > baseline.latency)
