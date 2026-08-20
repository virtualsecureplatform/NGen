package ngen

import ngen.backend.HogePipelinedSystemVerilog
import ngen.backend.HogeSystemVerilog
import ngen.rtl.{ProfileName, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class HogePipelinedSystemVerilogSpec extends AnyFunSuite:
  test("radix-32 HOGE has a bounded stage schedule"):
    assert(HogePipelinedSystemVerilog.stageCounts(10, 5) == (3, 3))

  test("stage-parallel HOGE emits the expected packed inverse interface"):
    val rtl = HogePipelinedSystemVerilog.emit("HogePipe", inverse = true, ProfileName.Baseline, TransposeKind.Indexed)
    assert(rtl.contains("stage-parallel HOGE"))
    assert(rtl.contains("module HogePipelinedStage_0"))
    assert(rtl.contains("localparam integer N=1024"))
    assert(rtl.contains("input [1023:0] io_in"))

  test("stage-parallel HOGE keeps the unsupported boundary mode explicit"):
    assertThrows[IllegalArgumentException](HogePipelinedSystemVerilog.emit("HogePipe", inverse = true, ProfileName.Baseline, TransposeKind.Switch))

  test("streaming HOGE supports forward boundary and distributed transpose modes"):
    val boundary = HogeSystemVerilog.emitStreamingNtt("HogeForwardSwitch", ProfileName.Baseline, TransposeKind.Switch)
    val distributed = HogeSystemVerilog.emitStreamingNtt("HogeForwardDistributed", ProfileName.Baseline, TransposeKind.Distributed)
    assert(boundary.contains("HogeInputNGenSwitchTransposeNetwork_5"))
    assert(distributed.contains("HogeDistributedSwitchTranspose_64"))
