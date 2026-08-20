package ngen

import ngen.backend.TwiddleStorageSystemVerilog
import ngen.rtl.TwiddleStorageKind
import org.scalatest.funsuite.AnyFunSuite

class TwiddleStorageSystemVerilogSpec extends AnyFunSuite:
  test("large twiddle tables lower to synchronous banked block ROM"):
    val(plan,rtl)=TwiddleStorageSystemVerilog.emit("Twiddles",Vector.tabulate(512)(BigInt(_)),64,32)
    assert(plan.kind==TwiddleStorageKind.BlockRom)
    assert(rtl.contains("rom_style=\"block\""))
    assert(rtl.contains("always @(posedge clock)"))

  test("small tables remain combinational constants"):
    val(plan,rtl)=TwiddleStorageSystemVerilog.emit("Tiny",Vector.tabulate(8)(BigInt(_)),27,2)
    assert(plan.kind==TwiddleStorageKind.Inline)
    assert(rtl.contains("always @(*)"))
