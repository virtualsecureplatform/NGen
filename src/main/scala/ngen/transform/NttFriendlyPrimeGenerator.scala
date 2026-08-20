package ngen.transform

import ngen.algebra.{Modulus,NttDomain,TransformShape}
import ngen.arithmetic.PrimeAnalyzer

final case class PrimeGenerationRequest(transformLog: Int, bitWidth: Int, count: Int = 1, negacyclic: Boolean = true, maxCoefficientWeight: Int = 5):
  require(transformLog>0&&bitWidth>transformLog&&count>0&&maxCoefficientWeight>0)

object NttFriendlyPrimeGenerator:
  def generate(request: PrimeGenerationRequest): Vector[NttDomain] =
    val orderLog=request.transformLog+(if request.negacyclic then 1 else 0)
    val factor=BigInt(1)<<orderLog
    val minimum=BigInt(1)<<(request.bitWidth-1)
    val maximum=(BigInt(1)<<request.bitWidth)-1
    var k=((minimum-1+factor-1)/factor)|1
    val candidates=scala.collection.mutable.ArrayBuffer.empty[(Int,BigInt)]
    var attempts=0
    val maximumAttempts=2000000
    while k*factor+1<=maximum && candidates.size<request.count*16 && attempts<maximumAttempts do
      val q=k*factor+1
      if q.isProbablePrime(80) then
        val weight=PrimeAnalyzer.signedDigits(k).size
        if weight<=request.maxCoefficientWeight then candidates += weight->q
      k+=2
      attempts+=1
    val selected=candidates.sortBy((weight,q)=>(weight,q)).take(request.count).map(_._2).toVector
    require(selected.size==request.count,s"found ${selected.size} hardware-friendly primes, requested ${request.count}")
    selected.zipWithIndex.map{case(q,index)=>
      val field=Modulus(q)
      val psi=if request.negacyclic then Some(field.findPowerOfTwoRoot(1<<orderLog)) else None
      val root=psi.map(x=>field.multiply(x,x)).getOrElse(field.findPowerOfTwoRoot(1<<request.transformLog))
      NttDomain(s"generated-prime-$index",1<<request.transformLog,field,root,
        if request.negacyclic then TransformShape.Negacyclic else TransformShape.Cyclic,psi,
        s"hardware-friendly ${request.bitWidth}-bit NTT prime; ${PrimeAnalyzer.analyze(field).recommendedMultiplier}")
    }

  def rns(request: PrimeGenerationRequest, dynamicRangeBits: Int): RnsBasis =
    require(dynamicRangeBits>0)
    var count=math.max(request.count,(dynamicRangeBits+request.bitWidth-1)/request.bitWidth)
    var basis=RnsBasis(generate(request.copy(count=count)))
    while basis.combinedModulus.bitLength<dynamicRangeBits do
      count+=1;basis=RnsBasis(generate(request.copy(count=count)))
    basis
