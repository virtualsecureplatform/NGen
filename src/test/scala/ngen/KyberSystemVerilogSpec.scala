package ngen

import ngen.backend.KyberSystemVerilog
import org.scalatest.funsuite.AnyFunSuite

class KyberSystemVerilogSpec extends AnyFunSuite:
  test("Kyber backend emits the PE1 protocol and incomplete transform"):
    val rtl = KyberSystemVerilog.emit()
    assert(rtl.contains("module KyberHPM1PE("))
    assert(rtl.contains("input start_fntt"))
    assert(rtl.contains("input start_intt"))
    assert(rtl.contains("output reg done"))
    assert(rtl.contains("KYBER_QINV = 16'd3327"))
    assert(!rtl.contains(" % "))

  test("compact Kyber validates its decoder and emits compilable physical RAM banks"):
    import java.nio.file.Files
    import scala.sys.process.*
    val rtl=KyberSystemVerilog.emit(banked=true)
    assert(!rtl.contains("reg [11:0] work ["))
    val directory=Files.createTempDirectory("ngen-kyber-banked")
    Files.writeString(directory.resolve("KyberHPM1PE.v"),rtl)
    assert(Process(Seq("iverilog","-g2012","-s","KyberHPM1PE","-o","simulation","KyberHPM1PE.v"),directory.toFile).! == 0)
