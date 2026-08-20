package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.backend.StageParallelNttSystemVerilog
import ngen.rtl.{ProfileName, ReductionKind}
import ngen.transform.NttPlan
import org.scalatest.funsuite.AnyFunSuite

class StageParallelNttSystemVerilogSpec extends AnyFunSuite:
  test("stage-parallel lowering accepts sparse prime folding"):
    val sparseDomain=ngen.algebra.NttDomain("gold",8,ngen.algebra.Modulus(BigInt("18446744069414584321")),
      ngen.algebra.Modulus(BigInt("18446744069414584321")).findPowerOfTwoRoot(8),ngen.algebra.TransformShape.Cyclic,None)
    val rtl=StageParallelNttSystemVerilog.emit(ngen.transform.NttPlan.radix2(sparseDomain,false),4,"SparseStage",ProfileName.Baseline,ReductionKind.SparseFold)
    assert(rtl.contains("Sparse signed folding"))
    assert(!rtl.contains("BARRETT_MU"))
  private val domain = NttDomain("stage", 8, Modulus(17), 9, TransformShape.Cyclic)

  test("generic stage-parallel lowering emits one registered boundary per NTT stage"):
    val plan = NttPlan.radix2(domain, inverse = false)
    assert(StageParallelNttSystemVerilog.stageCount(plan) == 3)
    val rtl = StageParallelNttSystemVerilog.emit(plan, 2, "StageNtt", ProfileName.Baseline, ReductionKind.Barrett)
    assert(rtl.contains("generic stage-parallel NTT"))
    assert(rtl.contains("localparam integer STAGE_COUNT=3"))
    assert(rtl.contains("module StageNtt("))

  test("generic stage-parallel lowering accepts Shoup constants"):
    val plan = NttPlan.radix2(domain, inverse = true)
    val rtl = StageParallelNttSystemVerilog.emit(plan, 2, "StageIntt", ProfileName.F300, ReductionKind.Shoup)
    assert(rtl.contains("Shoup reciprocals"))
    assert(rtl.contains("localparam integer STAGE_GAP=1"))
