package ngen

import ngen.algebra.Modulus
import ngen.backend.PipelinedButterflySystemVerilog
import ngen.rtl.ReductionKind
import org.scalatest.funsuite.AnyFunSuite

class PipelinedButterflySystemVerilogSpec extends AnyFunSuite:
  test("Barrett and Shoup retain three-stage tagged pipelines"):
    Vector(ReductionKind.Barrett, ReductionKind.Shoup).foreach { reduction =>
      val rtl = PipelinedButterflySystemVerilog.emit(Modulus(12289), reduction)
      assert(rtl.contains("parameter TAG_WIDTH=1"))
      assert(rtl.contains("valid_0"))
      assert(rtl.contains("valid_1"))
      assert(rtl.contains("tag_out<=tag_1"))
    }
    assert(PipelinedButterflySystemVerilog.Latency == 3)

  test("runtime Barrett pipeline exposes modulus and reciprocal inputs"):
    val rtl=PipelinedButterflySystemVerilog.emit(Modulus(12289),ReductionKind.Barrett,runtimeField=true)
    assert(rtl.contains("input [13:0] modulus_in"))
    assert(rtl.contains("input [27:0] reduction_constant_in"))

  test("wide Montgomery products preserve arithmetic, tags, bubbles and reset"):
    import java.nio.file.Files
    import scala.sys.process.*
    val random = new scala.util.Random(23)
    Vector(BigInt("4294967311"), BigInt("9007199255560193"), BigInt("18446744069414584321")).foreach { q =>
      val field = Modulus(q)
      val width = field.bitWidth
      val latency = PipelinedButterflySystemVerilog.latency(ReductionKind.Montgomery, width)
      assert(latency == 9)
      val pending = scala.collection.mutable.Queue.empty[Option[(BigInt, BigInt, Int)]]
      val cycles = (0 until 240).map { cycle =>
        val reset = cycle == 0 || cycle == 81 || cycle == 167
        val valid = cycle % 7 != 0
        val kind = cycle % 3 + 1
        val a = BigInt(width, random).mod(q)
        val b = BigInt(width, random).mod(q)
        val constant = if cycle % 5 == 0 then BigInt(1) else BigInt(width, random).mod(q)
        val encoded = (constant * (BigInt(1) << width)).mod(q)
        val result = kind match
          case 1 => ((a * constant).mod(q), BigInt(0), cycle)
          case 2 => ((a + b * constant).mod(q), (a - b * constant).mod(q), cycle)
          case 3 => ((a + b).mod(q), ((b - a) * constant).mod(q), cycle)
        val expected = if reset then
          pending.clear()
          None
        else
          pending.enqueue(if valid then Some(result) else None)
          if pending.size >= latency then pending.dequeue() else None
        val check = expected.map { case (x, y, tag) =>
          s"if(out0!==${width}'d$x||out1!==${width}'d$y||tag_out!==16'd$tag)$$fatal(1,\"arithmetic/tag cycle $cycle\");"
        }.getOrElse("")
        s"@(negedge clock);reset=${if reset then 1 else 0};valid_in=${if valid then 1 else 0};kind_in=2'd$kind;a_in=${width}'d$a;b_in=${width}'d$b;constant_in=${width}'d$encoded;tag_in=16'd$cycle;@(posedge clock);#1;if(valid_out!==1'b${if expected.nonEmpty then 1 else 0})$$fatal(1,\"valid cycle $cycle\");$check"
      }.mkString("\n")
      val directory = Files.createTempDirectory("ngen-wide-montgomery").toFile
      val rtl = PipelinedButterflySystemVerilog.emit(field, ReductionKind.Montgomery, "Dut")
      val tb = s"""module tb;
         |reg clock=0;always #5 clock=~clock;
         |reg reset=1,valid_in=0;reg [1:0] kind_in=0;
         |reg [${width-1}:0] a_in=0,b_in=0,constant_in=0;reg [15:0] tag_in=0;
         |wire valid_out;wire [${width-1}:0] out0,out1;wire [15:0] tag_out;
         |Dut #(.TAG_WIDTH(16)) dut(clock,reset,valid_in,kind_in,a_in,b_in,constant_in,${width}'d0,tag_in,valid_out,out0,out1,tag_out);
         |initial begin $cycles
         |$$finish;end
         |endmodule
         |""".stripMargin
      Files.writeString(directory.toPath.resolve("dut.sv"), rtl)
      Files.writeString(directory.toPath.resolve("tb.sv"), tb)
      assert(Process(Seq("iverilog", "-g2012", "-s", "tb", "-o", "sim", "dut.sv", "tb.sv"), directory).! == 0)
      assert(Process(Seq("vvp", "sim"), directory).! == 0)
    }
