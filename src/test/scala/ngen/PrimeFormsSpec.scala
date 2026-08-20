package ngen

import ngen.algebra.Modulus
import ngen.arithmetic.{PrimeAnalyzer,PrimeForm,ProthSredc,PseudoMersenneReduction,RangeBound}
import org.scalatest.funsuite.AnyFunSuite

class PrimeFormsSpec extends AnyFunSuite:
  test("YATA is recognized as a Proth prime with generalized SREDC"):
    val modulus=Modulus(40960001)
    val analysis=PrimeAnalyzer.analyze(modulus)
    assert(analysis.form==PrimeForm.Proth(625,16))
    assert(analysis.maximumNegacyclicLogSize==15)
    assert(analysis.recommendedMultiplier=="proth-sredc")
    val reducer=ProthSredc(modulus,625,16)
    val radix=BigInt(1)<<16
    for a <- 0 until 40; b <- 0 until 40 do
      assert(reducer.reduce(BigInt(a)*b)==modulus.multiply(BigInt(a)*b,radix.modInverse(modulus.q)))

  test("Goldilocks retains its dedicated sparse classification"):
    val analysis=PrimeAnalyzer.analyze(Modulus(BigInt("18446744069414584321")))
    assert(analysis.form==PrimeForm.Goldilocks)
    assert(analysis.twoAdicity==32)

  test("non-binary generalized Fermat primes retain their digit form"):
    assert(PrimeAnalyzer.analyze(Modulus(101)).form==PrimeForm.GeneralizedFermat(10,1))

  test("pseudo-Mersenne folding agrees with exact reduction"):
    val modulus=Modulus((BigInt(1)<<31)-1)
    val reducer=PseudoMersenneReduction(modulus,31,1)
    val values=Vector(BigInt(0),BigInt(1),(BigInt(1)<<62)-1,BigInt("12345678901234567890"))
    values.foreach(value=>assert(reducer.reduce(value)==modulus.normalize(value)))

  test("range bounds expose lazy butterfly growth"):
    assert(RangeBound(1).butterfly.butterfly.multipleOfQ==4)
    assert(RangeBound(8).reduced.multipleOfQ==1)
