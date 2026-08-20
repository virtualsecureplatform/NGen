package ngen.rtl

import ngen.algebra.Modulus
import ngen.arithmetic.{PrimeAnalyzer,PrimeForm}

final case class ModularRange(multiple:Int):
  require(multiple>0)
  def butterfly:ModularRange=ModularRange(multiple*2)
  def multiply:ModularRange=ModularRange(1)
  def needsCorrection(storageBits:Int,modulusBits:Int):Boolean =
    val growthBits=if multiple<=1 then 0 else 32-Integer.numberOfLeadingZeros(multiple-1)
    modulusBits+growthBits>storageBits

final case class LazyReductionSchedule(levels:Vector[ModularRange],correctionAfter:Vector[Int])
object LazyReductionSchedule:
  def build(levelCount:Int,modulusBits:Int,storageBits:Int):LazyReductionSchedule=
    require(levelCount>0&&storageBits>=modulusBits)
    var range=ModularRange(1)
    val levels=scala.collection.mutable.ArrayBuffer.empty[ModularRange]
    val corrections=scala.collection.mutable.ArrayBuffer.empty[Int]
    for level<-0 until levelCount do
      val grown=range.butterfly
      if grown.needsCorrection(storageBits,modulusBits) then
        corrections+=level;range=ModularRange(1)
      else range=grown
      levels+=range
    LazyReductionSchedule(levels.toVector,corrections.toVector)

final case class DspMultiplyPlan(leftBits:Int,rightBits:Int,dspLeftBits:Int=27,dspRightBits:Int=18):
  require(Seq(leftBits,rightBits,dspLeftBits,dspRightBits).forall(_>0))
  val leftSlices:Int=(leftBits+dspLeftBits-1)/dspLeftBits
  val rightSlices:Int=(rightBits+dspRightBits-1)/dspRightBits
  val dspCount:Int=leftSlices*rightSlices
  val adderLevels:Int=if dspCount<=1 then 0 else 32-Integer.numberOfLeadingZeros(dspCount-1)

final case class ArithmeticLoweringPlan(reduction:String,lazySchedule:LazyReductionSchedule,multiplier:DspMultiplyPlan,fusedTwiddleButterfly:Boolean)
object ArithmeticLoweringPlan:
  def build(modulus:Modulus,levels:Int,storageBits:Int):ArithmeticLoweringPlan=
    val analysis=PrimeAnalyzer.analyze(modulus)
    ArithmeticLoweringPlan(analysis.recommendedMultiplier,LazyReductionSchedule.build(levels,modulus.bitWidth,storageBits),DspMultiplyPlan(modulus.bitWidth,modulus.bitWidth),fusedTwiddleButterfly=true)
