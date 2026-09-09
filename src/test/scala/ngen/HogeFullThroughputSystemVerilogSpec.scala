package ngen

import ngen.backend.HogeFullThroughputSystemVerilog
import ngen.rtl.{ProfileName, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class HogeFullThroughputSystemVerilogSpec extends AnyFunSuite:
  test("full-throughput HOGE is a recursive two-pass switch pipeline"):
    val rtl = HogeFullThroughputSystemVerilog.emit("HogeFT", false, ProfileName.Baseline, TransposeKind.Switch)
    assert(HogeFullThroughputSystemVerilog.StreamCycles == 32)
    assert(HogeFullThroughputSystemVerilog.RadixPipelineDepth == 45)
    assert(rtl.contains("module HogeForwardRadix32Pipeline"))
    assert(rtl.contains("HogeFTNGenSwitchTransposeNetwork_5 transpose1"))
    assert(rtl.contains("HogeFTNGenSwitchTransposeNetwork_5 transpose2"))
    assert(rtl.contains("assign io_ready=1'b1"))

  test("full-throughput HOGE rejects a non-streaming transpose"):
    assertThrows[IllegalArgumentException](
      HogeFullThroughputSystemVerilog.emit("HogeFT", false, ProfileName.Baseline, TransposeKind.Indexed)
    )

  test("inverse full-throughput HOGE uses the golden former radix split"):
    val rtl = HogeFullThroughputSystemVerilog.emit("HogeIFT", true, ProfileName.Baseline, TransposeKind.Switch)
    assert(rtl.contains("module HogeFormerInverseRadix32Pipeline"))
    assert(rtl.contains("module HogeInverseRadix32Pipeline"))
    assert(rtl.contains("input [1023:0] io_in"))

  test("Goldilocks constant shifts match independent modular exponentiation"):
    import java.nio.file.Files
    import scala.sys.process.*
    val p = ngen.arithmetic.HogeField.Modulus
    val random = new scala.util.Random(0x484f4745)
    val boundary = Vector(BigInt(0),BigInt(1),p-1,p-2,(BigInt(1)<<32)-1,BigInt(1)<<32,BigInt(1)<<63)
    val checks = (0 until 192).flatMap { exponent =>
      val values = boundary ++ Vector.fill(32)(BigInt(64,random)%p)
      values.map { a =>
        val expected = a * BigInt(2).modPow(exponent,p) % p
        s"if(hoge_shift(64'h${a.toString(16)},$exponent)!==64'h${expected.toString(16)}) $$fatal(1,\"shift $exponent a=${a.toString(16)}\");"
      }
    }.mkString("\n")
    val directory = Files.createTempDirectory("ngen-hoge-shift")
    Files.writeString(directory.resolve("test.sv"),s"module test;${HogeFullThroughputSystemVerilog.arithmetic} initial begin $checks $$finish;end endmodule")
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)

  test("pipelined Goldilocks factors preserve products, bubbles and reset at II one"):
    import java.nio.file.Files
    import scala.sys.process.*
    val p = ngen.arithmetic.HogeField.Modulus
    val random = new scala.util.Random(0x50495045)
    val values = Vector(BigInt(0),BigInt(1),p-1,p-2,(BigInt(1)<<32)-1,BigInt(1)<<32,BigInt(1)<<63)
    var pending = Vector.fill[Option[BigInt]](8)(None)
    val checks = (0 until 600).map { cycle =>
      val reset = cycle < 2 || cycle == 77 || cycle == 78 || cycle == 213
      val valid = cycle % 7 != 2 && cycle % 11 != 5
      val a = if cycle < 49 then values(cycle / 7) else BigInt(64,random)%p
      val b = if cycle < 49 then values(cycle % 7) else BigInt(64,random)%p
      val expected = if reset then
        pending = Vector.fill(8)(None)
        None
      else
        val queue = pending :+ (if valid then Some(a*b%p) else None)
        pending = queue.tail
        queue.head
      val assertion = expected match
        case Some(value) => s"if(!valid_out || result!==64'h${value.toString(16)}) $$fatal(1,\"factor cycle $cycle\");"
        case None => s"if(valid_out) $$fatal(1,\"unexpected valid cycle $cycle\");"
      s"@(negedge clock);reset=${if reset then 1 else 0};valid_in=${if valid then 1 else 0};a=64'h${a.toString(16)};factor=64'h${b.toString(16)};@(posedge clock);#1;$assertion"
    }.mkString("\n")
    val directory = Files.createTempDirectory("ngen-hoge-factor-pipeline")
    Files.writeString(directory.resolve("test.sv"),s"${HogeFullThroughputSystemVerilog.factorPipeline}\nmodule test;reg clock=0,reset=1,valid_in=0;reg[63:0]a=0,factor=0;wire valid_out;wire[63:0]result;always #5 clock=~clock;HogeFactorPipeline dut(clock,reset,valid_in,a,factor,valid_out,result);initial begin $checks $$finish;end endmodule")
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)
