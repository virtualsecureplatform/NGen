package ngen

import ngen.backend.SwitchTransposeSystemVerilog
import ngen.rtl.{SwitchTranspose, SwitchTransposeSpec}
import org.scalatest.funsuite.AnyFunSuite

class SwitchTransposeSpecTest extends AnyFunSuite:
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

  test("switch definitions can be namespaced for distributed partitions"):
    val rtl = SwitchTransposeSystemVerilog.definitions(SwitchTransposeSpec(2, 32), "PartitionA")
    assert(rtl.contains("module PartitionANGenSwitchTransposeUnit_2"))
    assert(rtl.contains("module PartitionANGenSwitchTransposeNetwork_1"))
