package ngen

import ngen.algebra.{Modulus,NttDomain,TransformShape}
import ngen.rtl.{ConstantMultiplierKind,TwiddleStorageKind,TwiddleStoragePlan}
import ngen.transform.{DataOrder,NttPlan,RecursiveStreamingNttPlan,ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class LoweringStrategiesSpec extends AnyFunSuite:
  test("twiddle lowering moves from constants through distributed to block ROM"):
    assert(TwiddleStoragePlan.choose(16,27,8).kind==TwiddleStorageKind.Inline)
    assert(TwiddleStoragePlan.choose(64,27,8).kind==TwiddleStorageKind.DistributedRom)
    assert(TwiddleStoragePlan.choose(512,64,32).kind==TwiddleStorageKind.BlockRom)

  test("known prime shapes select their specialized multiplier"):
    assert(ConstantMultiplierKind.forModulus(BigInt("40960001"))==ConstantMultiplierKind.Sredc)
    assert(ConstantMultiplierKind.forModulus(BigInt("18446744069414584321"))==ConstantMultiplierKind.Goldilocks)
    assert(ConstantMultiplierKind.forModulus(BigInt(12289))==ConstantMultiplierKind.Shoup)

  test("general recursive radix grouping preserves the NTT"):
    val domain=NttDomain("r16",16,Modulus(97),8,TransformShape.Cyclic,None)
    val base=NttPlan.radix2(domain,false,DataOrder.Natural,DataOrder.Natural)
    val recursive=RecursiveStreamingNttPlan.build(base,4,radixLog=2)
    val input=Vector.tabulate(16)(BigInt(_))
    assert(recursive.radix==4)
    assert(recursive.levels==2)
    assert(recursive.plan.evaluate(input)==ReferenceNtt.forward(domain,input))
