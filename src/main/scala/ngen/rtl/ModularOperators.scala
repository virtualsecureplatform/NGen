package ngen.rtl

import ngen.algebra.Modulus
import ngen.arithmetic.BarrettField

sealed trait ModularOperator extends Operator:
  def modulus: Modulus
  protected final val format: ValueFormat = ValueFormat.unsigned(modulus.bitWidth)
  override def outputFormat(inputs: Vector[Signal]): ValueFormat = format

final case class ModularAdd(modulus: Modulus, latency: Int) extends ModularOperator:
  override val name = "mod_add"
  override val arity = 2
  override def evaluate(inputs: Vector[BigInt]) = modulus.add(inputs(0), inputs(1))

final case class ModularSubtract(modulus: Modulus, latency: Int) extends ModularOperator:
  override val name = "mod_sub"
  override val arity = 2
  override def evaluate(inputs: Vector[BigInt]) = modulus.subtract(inputs(0), inputs(1))

final case class BarrettMultiplyConstant(modulus: Modulus, constant: BigInt, latency: Int) extends ModularOperator:
  override val name = s"barrett_mul_const:${modulus.normalize(constant)}"
  override val arity = 1
  override def evaluate(inputs: Vector[BigInt]) = BarrettField(modulus).multiply(inputs.head, constant)
