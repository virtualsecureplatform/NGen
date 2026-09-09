package ngen

import ngen.backend.HogeRadixPipeline
import org.scalatest.funsuite.AnyFunSuite

class HogeRadixPipelineSpec extends AnyFunSuite:
  test("registered shifts match modular exponentiation across all exponents, bubbles and reset"):
    import java.nio.file.Files
    import scala.sys.process.*
    val p=ngen.arithmetic.HogeField.Modulus
    val random=new scala.util.Random(0x52414449)
    val values=Vector(BigInt(0),BigInt(1),p-1,p-2,(BigInt(1)<<32)-1,BigInt(1)<<32,BigInt(1)<<63) ++ Vector.fill(40)(BigInt(64,random)%p)
    var pending=Vector.fill[Option[BigInt]](6)(None)
    val checks=(0 until 70).map { cycle =>
      val a=values(cycle%values.size)
      val reset=cycle<2 || cycle==26 || cycle==45
      val valid=cycle%9!=3
      val expected=if reset then
        pending=Vector.fill(6)(None);None
      else
        val queue=pending :+ (if valid then Some(a) else None)
        pending=queue.tail;queue.head
      val assertions=(0 until 192).map { exponent =>
        expected match
          case Some(value) =>
            val result=value*BigInt(2).modPow(exponent,p)%p
            s"if(!v[$exponent] || r[$exponent]!==64'h${result.toString(16)}) $$fatal(1,\"shift $exponent cycle $cycle\");"
          case None => s"if(v[$exponent]) $$fatal(1,\"unexpected shift valid $exponent cycle $cycle\");"
      }.mkString("\n")
      s"@(negedge clock);reset=${if reset then 1 else 0};valid_in=${if valid then 1 else 0};a=64'h${a.toString(16)};@(posedge clock);#1;$assertions"
    }.mkString("\n")
    val directory=Files.createTempDirectory("ngen-hoge-registered-shifts")
    val tb=s"module test;reg clock=0,reset=1,valid_in=0;reg[63:0]a=0;wire[191:0]v;wire[63:0]r[0:191];always #5 clock=~clock;genvar g;generate for(g=0;g<192;g=g+1)begin HogeShiftPipeline #(.EXP(g)) dut(clock,reset,valid_in,a,v[g],r[g]);end endgenerate initial begin $checks $$finish;end endmodule"
    Files.writeString(directory.resolve("test.sv"),HogeRadixPipeline.primitives+tb)
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)
