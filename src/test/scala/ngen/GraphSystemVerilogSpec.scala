package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.backend.GraphSystemVerilog
import ngen.rtl.{GenericNttGraph, PipelineProfile}
import org.scalatest.funsuite.AnyFunSuite

class GraphSystemVerilogSpec extends AnyFunSuite:
  private val domain = NttDomain("q17", 8, Modulus(17), 9, TransformShape.Cyclic)

  test("generic graph backend emits SGen-style streaming ports and registered valid"):
    val graph = GenericNttGraph.build(domain, inverse = false, PipelineProfile.Baseline)
    val rtl = GraphSystemVerilog.emit(graph, domain)
    assert(rtl.contains("module main("))
    assert(rtl.contains("input next"))
    assert(rtl.contains("input [4:0] i7"))
    assert(rtl.contains("output [4:0] o7"))
    assert(rtl.contains("assign next_out = next_pipe"))
    assert(rtl.contains("BARRETT_MU"))
    assert(!rtl.contains(" % "))
