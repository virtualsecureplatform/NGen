package ngen

import ngen.algebra.Modulus
import ngen.backend.{FusedTwiddleButterflySystemVerilog,PrimeReductionSystemVerilog}
import ngen.rtl.{ArithmeticLoweringPlan,DspMultiplyPlan,LazyReductionSchedule}
import org.scalatest.funsuite.AnyFunSuite

class ArithmeticLoweringSpec extends AnyFunSuite:
  test("lazy range analysis inserts only required corrections"):
    val schedule=LazyReductionSchedule.build(levelCount=8,modulusBits=27,storageBits=30)
    assert(schedule.correctionAfter==Vector(3,7))

  test("DSP48 tiling reports explicit partial product cost"):
    assert(DspMultiplyPlan(27,27).dspCount==2)
    assert(DspMultiplyPlan(64,64).dspCount==12)
    val plan=ArithmeticLoweringPlan.build(Modulus(40960001),3,30)
    assert(plan.reduction=="proth-sredc"&&plan.fusedTwiddleButterfly)

  test("special prime reducers and fused butterflies emit synthesizable structures"):
    val proth=PrimeReductionSystemVerilog.emit(Modulus(40960001),"ProthReducer")
    val gold=PrimeReductionSystemVerilog.emit(Modulus(BigInt("18446744069414584321")),"GoldReducer")
    val fused=FusedTwiddleButterflySystemVerilog.emit(Modulus(40960001),"FusedYata")
    val tiled=FusedTwiddleButterflySystemVerilog.emit(Modulus(40960001),"FusedYataTiled",dspDecompose=true)
    assert(proth.contains("RADIX_BITS=16,K=625"))
    assert(!proth.contains("%"))
    assert(gold.contains("C="))
    assert(fused.contains("DSP_DECOMPOSE=0"))
    assert(!fused.contains("use_dsp=\"yes\""))
    assert(tiled.contains("DSP_DECOMPOSE=1"))
    assert(tiled.contains("use_dsp=\"yes\""))
    assert(fused.contains("twiddle_montgomery"))
