package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.backend.PeStreamingNttSystemVerilog
import ngen.rtl.{PeNttSchedule, ProfileName, ReductionKind}
import ngen.transform.NttPlan
import org.scalatest.funsuite.AnyFunSuite

class PeStreamingNttSystemVerilogSpec extends AnyFunSuite:
  private val domain = NttDomain("q97", 16, Modulus(97), 8, TransformShape.Cyclic)

  test("backend emits reusable PEs, physical banks, and two buffers"):
    val schedule = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), radixLog = 1, requestedPeCount = 2, streamingWidth = 4)
    val rtl = PeStreamingNttSystemVerilog.emit(schedule, 4, "BankedNtt", ProfileName.Baseline, ReductionKind.Shoup)
    assert(rtl.contains("module BankedNtt("))
    assert(rtl.contains("PE_COUNT=2"))
    assert(rtl.contains("buffer_0_bank_0"))
    assert(rtl.contains("buffer_1_bank_0"))
    assert(rtl.contains("pe_kind_1"))
    assert(rtl.contains("b_shoup"))

  test("backend emits a shared-butterfly reusable radix-4 PE"):
    val schedule = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), radixLog = 2, requestedPeCount = 1, streamingWidth = 4)
    val rtl = PeStreamingNttSystemVerilog.emit(schedule, 4, "Radix4Ntt", ProfileName.F300, ReductionKind.Montgomery)
    assert(rtl.contains("RADIX=4"))
    assert(rtl.contains("pe_0_in_3"))
    assert(rtl.contains("pe_out_0_3"))
    assert(rtl.contains("BUNDLE_GAP=1"))

  test("metrics include synchronous reads and reusable bundle execution"):
    val radix2 = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), 1, 2, 4)
    val radix2Metrics = PeStreamingNttSystemVerilog.metrics(radix2, 4, ProfileName.Baseline)
    assert(radix2Metrics.bundleCount == 16)
    assert(radix2Metrics.executionCycles == 37)
    assert(radix2Metrics.latency == 43)
    val radix4 = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), 2, 1, 4)
    assert(PeStreamingNttSystemVerilog.metrics(radix4, 4, ProfileName.Baseline).latency == 47)

  test("backend emits per-chunk ready-valid ports"):
    val schedule = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), 1, 1, 4)
    val rtl = PeStreamingNttSystemVerilog.emit(schedule, 4, "ReadyValidNtt", ProfileName.Baseline, ReductionKind.Shoup, ngen.rtl.StreamProtocol.ReadyValid)
    assert(rtl.contains("input in_valid"))
    assert(rtl.contains("output in_ready"))
    assert(rtl.contains("output reg out_valid"))
    assert(rtl.contains("input out_ready"))
    assert(!rtl.contains("input next"))

  test("radix-2 backend emits multi-inflight issue and stage drain control"):
    val schedule = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), 1, 2, 4)
    val rtl = PeStreamingNttSystemVerilog.emit(schedule, 4, "MultiIssueNtt", ProfileName.Baseline, ReductionKind.Shoup)
    assert(rtl.contains("wire issue_fire=exec_active&&!draining"))
    assert(rtl.contains("integer inflight_count"))
    assert(rtl.contains("retire_fire&&draining"))

  test("fused backend registers each internal radix layer"):
    val schedule = PeNttSchedule.build(NttPlan.radix2(domain, inverse = false), 2, 1, 4)
    val rtl = PeStreamingNttSystemVerilog.emit(schedule, 4, "LayeredRadix4", ProfileName.Baseline, ReductionKind.Shoup)
    assert(rtl.contains("pe_0_layer_0_0"))
    assert(rtl.contains("pe_0_layer_1_0"))
    assert(rtl.contains("pe_0_valid_pipe[1]"))

  test("backend exposes writable packed control records"):
    val schedule=PeNttSchedule.build(NttPlan.radix2(domain,inverse=false),1,2,4)
    val rtl=PeStreamingNttSystemVerilog.emit(schedule,4,"RuntimeControl",ProfileName.Baseline,ReductionKind.Shoup,runtimeControl=true)
    assert(rtl.contains("input config_control_we"))
    assert(rtl.contains("config_control_address"))
    assert(rtl.contains("control_0_rom[config_control_address]<=config_control_data"))
