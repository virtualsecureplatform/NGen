package ngen.rtl

import ngen.arithmetic.YataField

final case class YataAddMod(latency: Int) extends Operator:
  override val name = "yata_add_mod"
  override val arity = 2
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[BigInt]) = BigInt(YataField.addMod(inputs(0).toLong, inputs(1).toLong))

final case class YataSubtractMod(latency: Int) extends Operator:
  override val name = "yata_sub_mod"
  override val arity = 2
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[BigInt]) = BigInt(YataField.subtractMod(inputs(0).toLong, inputs(1).toLong))

final case class YataMultiplySredc(constant: Long, latency: Int) extends Operator:
  override val name = s"yata_mul_sredc:$constant"
  override val arity = 1
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[BigInt]) = BigInt(YataField.multiplySigned(inputs.head.toLong, constant))

object YataTimedButterfly:
  /** Twiddle multiplication followed by a modular radix-2 butterfly. */
  def build(twiddle: Long, profile: PipelineProfile = PipelineProfile.Baseline): TimedGraph =
    val builder = TimedGraphBuilder()
    val left = builder.input("left", ValueFormat.SignedWord27)
    val right = builder.input("right", ValueFormat.SignedWord27)
    val product = builder(YataMultiplySredc(twiddle, profile.multiplierLatency + profile.reductionLatency), right)
    val sum = builder(YataAddMod(profile.addLatency), left, product)
    val difference = builder(YataSubtractMod(profile.addLatency), left, product)
    builder.result(sum, difference)
