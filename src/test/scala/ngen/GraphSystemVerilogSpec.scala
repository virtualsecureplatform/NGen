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

  test("split Barrett preserves direct DFT values, bubbles and reset"):
    import java.nio.file.Files
    import scala.sys.process.*
    for inverse <- Vector(false, true) do
      val graph = GenericNttGraph.build(domain, inverse, PipelineProfile.SplitBarrett)
      val pending = scala.collection.mutable.Queue.empty[Option[Vector[BigInt]]]
      val random = new scala.util.Random(73)
      val cycles = (0 until 180).map { cycle =>
        val reset = cycle == 0 || cycle == 71
        val valid = cycle % 5 != 0
        val values = Vector.fill(8)(BigInt(random.nextInt(17)))
        val root = if inverse then BigInt(9).modInverse(17) else BigInt(9)
        val expectedValues = Vector.tabulate(8)(k =>
          (values.zipWithIndex.map((v,j) => v * root.modPow(j*k,17)).sum *
            (if inverse then BigInt(8).modInverse(17) else BigInt(1))).mod(17))
        val expected = if reset then
          pending.clear()
          None
        else
          pending.enqueue(if valid then Some(expectedValues) else None)
          if pending.size >= graph.latency then pending.dequeue() else None
        val assignments = values.zipWithIndex.map((v,j) => s"i$j=5'd$v;").mkString
        val checks = expected.map(_.zipWithIndex.map((v,j) =>
          s"if(o$j!==5'd$v)$$fatal(1,\"output $j cycle $cycle\");").mkString).getOrElse("")
        s"@(negedge clock);reset=${if reset then 1 else 0};next=${if valid then 1 else 0};$assignments@(posedge clock);#1;if(next_out!==1'b${if expected.nonEmpty then 1 else 0})$$fatal(1,\"valid cycle $cycle\");$checks"
      }.mkString("\n")
      val directory = Files.createTempDirectory("ngen-split-barrett").toFile
      val ports = (0 until 8).map(j => s".i$j(i$j),.o$j(o$j)").mkString(",")
      val declarations = (0 until 8).map(j => s"reg[4:0] i$j=0;wire[4:0] o$j;").mkString
      Files.writeString(directory.toPath.resolve("dut.sv"), GraphSystemVerilog.emit(graph,domain,splitBarrett=true))
      Files.writeString(directory.toPath.resolve("tb.sv"), s"""module tb;
        reg clock=0;always #5 clock=~clock;reg reset=1,next=0;wire next_out;
        $declarations
        main dut(.clock(clock),.reset(reset),.next(next),.next_out(next_out),$ports);
        initial begin $cycles $$finish;end
        endmodule""")
      assert(Process(Seq("iverilog","-g2012","-s","tb","-o","sim","dut.sv","tb.sv"),directory).! == 0)
      assert(Process(Seq("vvp","sim"),directory).! == 0)
