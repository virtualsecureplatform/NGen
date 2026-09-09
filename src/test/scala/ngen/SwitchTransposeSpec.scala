package ngen

import ngen.backend.SwitchTransposeSystemVerilog
import ngen.rtl.{SwitchTranspose, SwitchTransposeSpec}
import org.scalatest.funsuite.AnyFunSuite

class SwitchTransposeSpecTest extends AnyFunSuite:
  test("unreset delay data preserves valid outputs across gaps and aborted frames"):
    import java.nio.file.Files
    import scala.sys.process.*
    val random = new scala.util.Random(0x52455345)
    for bits <- Vector(1,3,5) do
      val width = 1 << bits
      val spec = SwitchTransposeSpec(bits,16)
      val dir = Files.createTempDirectory("ngen-transpose-valid-reset")
      val cycles = (0 until 1200).map { cycle =>
        val reset = cycle < 2 || cycle % 113 == 41 || cycle % 197 == 91
        // Includes saturated streams, short gaps and mid-frame aborts.
        val valid = cycle % (width*3+3) < width*3
        val data = BigInt(width*16,random)
        val check = if cycle == 0 then "" else
          s"if(v_reset!==v_valid || (v_reset===1'b1 && d_reset!==d_valid)) $$fatal(1,\"reset equivalence width=$width cycle=$cycle\");"
        s"@(negedge clock);reset=${if reset then 1 else 0};valid_in=${if valid then 1 else 0};data_in=${width*16}'h${data.toString(16)};#1;$check"
      }.mkString("\n")
      val rtl = SwitchTransposeSystemVerilog.definitions(spec,"Reset") +
        SwitchTransposeSystemVerilog.definitions(spec,"Valid",resetData=false)
      val tb = s"""module test;reg clock=0,reset=1,valid_in=0;reg[${width*16-1}:0]data_in=0;
         |wire v_reset,v_valid;wire[${width*16-1}:0]d_reset,d_valid;always #5 clock=~clock;
         |ResetNGenSwitchTransposeNetwork_$bits a(clock,reset,valid_in,data_in,v_reset,d_reset);
         |ValidNGenSwitchTransposeNetwork_$bits b(clock,reset,valid_in,data_in,v_valid,d_valid);
         |initial begin $cycles $$finish;end endmodule
         |""".stripMargin
      Files.writeString(dir.resolve("test.sv"),rtl+tb)
      assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),dir.toFile).! == 0)
      assert(Process(Seq("vvp","sim"),dir.toFile).! == 0)

  test("reference transposes cycle and lane coordinates"):
    val input = Vector.tabulate(8)(cycle => Vector.tabulate(8)(lane => cycle * 8 + lane))
    val output = SwitchTranspose.reference(input)
    assert(output(3)(5) == input(5)(3))
    assert(output.flatten.sorted == input.flatten.sorted)

  test("reference supports rectangular cycle and lane dimensions"):
    val input=Vector.tabulate(4)(cycle=>Vector.tabulate(8)(lane=>cycle*8+lane))
    val output=SwitchTranspose.reference(input)
    assert(output.size==8 && output.forall(_.size==4))
    assert(output(6)(3)==input(3)(6))
    assert(output.flatten.sorted==input.flatten.sorted)

  test("switch transpose latency is the recursive delay sum"):
    assert(SwitchTransposeSpec(3, 16).latency == 7)
    assert(SwitchTransposeSpec(5, 64).latency == 31)

  test("backend emits recursive HOGE-style switch units"):
    val rtl = SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(3,16), "Transpose8")
    assert(rtl.contains("module NGenSwitchTransposeUnit_3"))
    assert(rtl.contains("module NGenSwitchTransposeNetwork_2"))
    assert(rtl.contains("module Transpose8"))

  test("backend emits a width-changing rectangular transpose adapter"):
    val rtl=SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(2,3,16),"Transpose4x8")
    assert(rtl.contains("4x8 -> 8x4"))
    assert(rtl.contains("input [128-1:0] data_in"))
    assert(rtl.contains("output reg [64-1:0] data_out"))
    assert(rtl.contains("output input_ready"))
    assert(rtl.contains("storage_0") && rtl.contains("storage_1"))

  test("fixed-rate rectangular adapter removes ready and documents its frame interval"):
    val rtl=SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(2,3,16),"Fixed4x8",fixedRate=true)
    assert(!rtl.contains("output input_ready"))
    assert(rtl.contains("FRAME_INTERVAL=8,MIN_FRAME_GAP=4"))

  test("rate-preserving rectangular adapter retains the input width and interval"):
    val rtl=SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(2,3,16),"Rate4x8",ratePreserving=true)
    assert(rtl.contains("output reg [128-1:0] data_out"))
    assert(rtl.contains("FRAME_INTERVAL=4,MIN_FRAME_GAP=0"))
    assert(rtl.contains("packed/split"))
    assert(rtl.contains("(output_count*8+1)%4)*8+((output_count*8+1)/4)"))

  test("switch definitions can be namespaced for distributed partitions"):
    val rtl = SwitchTransposeSystemVerilog.definitions(SwitchTransposeSpec(2, 32), "PartitionA")
    assert(rtl.contains("module PartitionANGenSwitchTransposeUnit_2"))
    assert(rtl.contains("module PartitionANGenSwitchTransposeNetwork_1"))
