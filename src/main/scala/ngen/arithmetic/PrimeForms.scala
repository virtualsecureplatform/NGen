package ngen.arithmetic

import ngen.algebra.Modulus

enum PrimeForm:
  case Goldilocks
  case Proth(k: BigInt, twoAdicity: Int)
  case PseudoMersenne(bits: Int, signedConstant: BigInt)
  case SparseSolinas(bits: Int, terms: Vector[(Int,Int)])
  case GeneralizedFermat(base: BigInt, index: Int)
  case Generic

final case class MontgomeryCost(wordBits: Int, qInverse: BigInt, nonzeroBits: Int, signedDigits: Int)
final case class PrimeAnalysis(
    modulus: Modulus,
    form: PrimeForm,
    twoAdicity: Int,
    maximumCyclicLogSize: Int,
    maximumNegacyclicLogSize: Int,
    montgomery: Vector[MontgomeryCost],
    recommendedMultiplier: String,
    lazyButterflyLevels: Int
)

object PrimeAnalyzer:
  private def valuation2(value: BigInt): Int =
    if value == 0 then 0 else value.bigInteger.getLowestSetBit

  /** Non-adjacent signed binary digits, least weight among local carry choices. */
  def signedDigits(value: BigInt): Vector[(Int,Int)] =
    var remaining=value
    var shift=0
    val result=scala.collection.mutable.ArrayBuffer.empty[(Int,Int)]
    while remaining != 0 do
      if remaining.testBit(0) then
        val digit=if (remaining & 3)==3 then -1 else 1
        result += shift -> digit
        remaining=(remaining-digit)/2
      else remaining/=2
      shift+=1
    result.toVector

  private def integerRoot(value: BigInt, degree: Int): BigInt =
    var low=BigInt(1);var high=BigInt(2)
    while high.pow(degree)<=value do high*=2
    while high-low>1 do
      val mid=(low+high)/2
      if mid.pow(degree)<=value then low=mid else high=mid
    low

  private def generalizedFermat(q: BigInt): Option[(BigInt,Int)] =
    (1 to 12).iterator.flatMap{index=>
      val degree=1<<index
      val base=integerRoot(q-1,degree)
      Option.when(base.pow(degree)==q-1)(base,index)
    }.toSeq.headOption

  def analyze(modulus: Modulus): PrimeAnalysis =
    val q=modulus.q
    val bits=modulus.bitWidth
    val adicity=valuation2(q-1)
    val high=BigInt(1)<<bits
    val lower=BigInt(1)<<(bits-1)
    val minusConstant=high-q
    val plusConstant=q-lower
    val prothK=(q-1)>>adicity
    val sparse=signedDigits(q)
    val form =
      if q==BigInt("18446744069414584321") then PrimeForm.Goldilocks
      else if adicity>=8 && prothK.testBit(0) && prothK < (BigInt(1)<<adicity) then PrimeForm.Proth(prothK,adicity)
      else generalizedFermat(q).map((base,index)=>PrimeForm.GeneralizedFermat(base,index)).getOrElse(
        if adicity>0 && prothK.testBit(0) && prothK < (BigInt(1)<<adicity) then PrimeForm.Proth(prothK,adicity)
        else if minusConstant>0 && signedDigits(minusConstant).size<=3 then PrimeForm.PseudoMersenne(bits,minusConstant)
        else if plusConstant>0 && signedDigits(plusConstant).size<=3 then PrimeForm.PseudoMersenne(bits-1,-plusConstant)
        else if sparse.size<=5 then PrimeForm.SparseSolinas(bits,sparse)
        else PrimeForm.Generic)
    val candidateWords=(Vector(16,18,27,32,54,64):+bits).distinct.sorted
    val montgomery=candidateWords.filter(_>=bits).map{wordBits=>
      val radix=BigInt(1)<<wordBits
      val inverse=if q.gcd(radix)==1 then (-q.modInverse(radix))&(radix-1) else BigInt(0)
      MontgomeryCost(wordBits,inverse,inverse.bitCount,signedDigits(inverse).size)
    }
    val recommendation=form match
      case PrimeForm.Goldilocks => "goldilocks-fold"
      case _:PrimeForm.GeneralizedFermat => "fermat-digit-shift"
      case _:PrimeForm.Proth => "proth-sredc"
      case _:PrimeForm.PseudoMersenne|_:PrimeForm.SparseSolinas => "sparse-fold"
      case PrimeForm.Generic => if montgomery.minBy(_.signedDigits).signedDigits<=3 then "montgomery" else "shoup-constant"
    // With inputs in [0,q), an unreduced radix-2 sum grows by one bit/level.
    val headroom=(2*bits)-bits
    PrimeAnalysis(modulus,form,adicity,adicity,math.max(0,adicity-1),montgomery,recommendation,headroom)

final case class ProthSredc(modulus: Modulus, k: BigInt, radixBits: Int):
  val radix: BigInt=BigInt(1)<<radixBits
  require(modulus.q==k*radix+1 && k.testBit(0))
  def reduce(product: BigInt): BigInt =
    val m=(-product)&(radix-1)
    modulus.normalize((product+m*modulus.q)/radix)
  def multiplyMontgomery(left: BigInt,right: BigInt): BigInt=reduce(left*right)

final case class PseudoMersenneReduction(modulus: Modulus, bits: Int, signedConstant: BigInt):
  require(modulus.q==(BigInt(1)<<bits)-signedConstant)
  private val mask=(BigInt(1)<<bits)-1
  def reduce(value: BigInt): BigInt =
    var current=value
    while current.bitLength>bits+2 do current=(current&mask)+(current>>bits)*signedConstant
    modulus.normalize(current)

final case class RangeBound(multipleOfQ: BigInt):
  require(multipleOfQ>0)
  def butterfly: RangeBound=RangeBound(multipleOfQ*2)
  def reduced: RangeBound=RangeBound(1)
