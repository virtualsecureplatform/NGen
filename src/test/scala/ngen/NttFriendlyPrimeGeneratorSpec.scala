package ngen

import ngen.transform.{NttFriendlyPrimeGenerator,PrimeGenerationRequest,ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class NttFriendlyPrimeGeneratorSpec extends AnyFunSuite:
  test("prime generator returns validated negacyclic domains"):
    val domains=NttFriendlyPrimeGenerator.generate(PrimeGenerationRequest(transformLog=4,bitWidth=12,count=2,maxCoefficientWeight=6))
    assert(domains.size==2)
    domains.foreach{domain=>
      domain.validate()
      assert((domain.modulus.q-1)%(2*domain.size)==0)
      val input=Vector.tabulate(domain.size)(BigInt(_))
      assert(ReferenceNtt.inverse(domain,ReferenceNtt.forward(domain,input))==input)
    }

  test("RNS generation meets its requested dynamic range"):
    val basis=NttFriendlyPrimeGenerator.rns(PrimeGenerationRequest(3,10,count=1,maxCoefficientWeight=7),25)
    assert(basis.combinedModulus.bitLength>=25)
