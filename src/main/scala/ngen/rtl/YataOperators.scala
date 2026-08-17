package ngen.rtl

import ngen.arithmetic.YataField

final case class PipelineProfile(
    addLatency: Int = 1,
    multiplierLatency: Int = 2,
    reductionLatency: Int = 1
):
  require(addLatency >= 0 && multiplierLatency >= 0 && reductionLatency >= 0)

final case class YataAddMod(latency: Int) extends Operator:
  override val name = "yata_add_mod"
  override val arity = 2
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[Long]) = YataField.addMod(inputs(0), inputs(1))

final case class YataSubtractMod(latency: Int) extends Operator:
  override val name = "yata_sub_mod"
  override val arity = 2
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[Long]) = YataField.subtractMod(inputs(0), inputs(1))

final case class YataMultiplySredc(constant: Long, latency: Int) extends Operator:
  override val name = s"yata_mul_sredc:$constant"
  override val arity = 1
  override def outputFormat(inputs: Vector[Signal]) = ValueFormat.SignedWord27
  override def evaluate(inputs: Vector[Long]) = YataField.multiplySigned(inputs.head, constant)

object YataTimedButterfly:
  /** Twiddle multiplication followed by a modular radix-2 butterfly. */
  def build(twiddle: Long, profile: PipelineProfile): TimedGraph =
    val builder = TimedGraphBuilder()
    val left = builder.input("left", ValueFormat.SignedWord27)
    val right = builder.input("right", ValueFormat.SignedWord27)
    val product = builder(YataMultiplySredc(twiddle, profile.multiplierLatency + profile.reductionLatency), right)
    val sum = builder(YataAddMod(profile.addLatency), left, product)
    val difference = builder(YataSubtractMod(profile.addLatency), left, product)
    builder.result(sum, difference)
