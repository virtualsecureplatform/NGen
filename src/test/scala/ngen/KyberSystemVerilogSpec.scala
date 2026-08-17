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
