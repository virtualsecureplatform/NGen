package ngen.arithmetic

import ngen.algebra.Modulus

final case class BarrettField(modulus: Modulus):
  val width: Int = modulus.bitWidth
  val mu: BigInt = (BigInt(1) << (2 * width)) / modulus.q

  def reduce(value: BigInt): BigInt =
    if value < 0 then modulus.normalize(value)
    else
      val estimate = (value * mu) >> (2 * width)
      var remainder = value - estimate * modulus.q
      while remainder < 0 do remainder += modulus.q
      while remainder >= modulus.q do remainder -= modulus.q
      remainder

  def add(lhs: BigInt, rhs: BigInt): BigInt = modulus.add(lhs, rhs)
  def subtract(lhs: BigInt, rhs: BigInt): BigInt = modulus.subtract(lhs, rhs)
  def multiply(lhs: BigInt, rhs: BigInt): BigInt = reduce(modulus.normalize(lhs) * modulus.normalize(rhs))
