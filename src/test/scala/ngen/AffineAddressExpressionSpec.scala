package ngen

import ngen.backend.AffineAddressExpression
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Files
import scala.sys.process.*

class AffineAddressExpressionSpec extends AnyFunSuite:
  test("emitted address logic matches all addresses including increment and constant maps"):
    val values = Vector.tabulate(32)(address => (Integer.reverse(address) >>> 27) ^ 19)
    val folded = Vector.tabulate(32)(address => (address ^ (address >>> 2) ^ (address >>> 4)) & 3)
    val directory = Files.createTempDirectory("ngen-affine-test")
    val expressions = Vector(
      AffineAddressExpression.emit(values,"index",5).get,
      AffineAddressExpression.emit(folded,"index+1",2).get,
      AffineAddressExpression.emit(Vector(3),"index",2).get)
    val checks = values.indices.map { address =>
      s"index=$address;#1;if(a!==5'd${values(address)} || b!==2'd${folded((address+1)%32)} || c!==2'd3) $$fatal(1,\"address $address\");"
    }.mkString
    val rtl = s"module test;integer index;wire [4:0] a=${expressions(0)};wire [1:0] b=${expressions(1)},c=${expressions(2)};initial begin $checks $$finish;end endmodule"
    Files.writeString(directory.resolve("test.sv"),rtl)
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)

  test("nonlinear address maps retain case-table fallback"):
    val nonlinear = Vector.tabulate(16)(address => if address==15 then 0 else address)
    assert(AffineAddressExpression.emit(nonlinear,"index",4).isEmpty)

  test("reject malformed tables and truncated output widths"):
    intercept[IllegalArgumentException](AffineAddressExpression.emit(Vector(0,1,2),"index",2))
    intercept[IllegalArgumentException](AffineAddressExpression.emit(Vector(0,4),"index",2))
