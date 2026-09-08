package ngen

import ngen.backend.HogeFullThroughputSystemVerilog
import ngen.rtl.{ProfileName, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class HogeFullThroughputSystemVerilogSpec extends AnyFunSuite:
  test("full-throughput HOGE is a recursive two-pass switch pipeline"):
    val rtl = HogeFullThroughputSystemVerilog.emit("HogeFT", false, ProfileName.Baseline, TransposeKind.Switch)
    assert(HogeFullThroughputSystemVerilog.StreamCycles == 32)
    assert(HogeFullThroughputSystemVerilog.RadixPipelineDepth == 5)
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
