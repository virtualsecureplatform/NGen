package ngen.arithmetic

import ngen.algebra.Modulus

final case class ShoupConstant(value: BigInt, precondition: BigInt)

/** Exact model of fixed-operand Shoup multiplication with one correction. */
final case class ShoupField(modulus: Modulus):
  val width: Int = modulus.bitWidth
  val radix: BigInt = BigInt(1) << width

  def prepare(value: BigInt): ShoupConstant =
    val normalized = modulus.normalize(value)
    ShoupConstant(normalized, normalized * radix / modulus.q)

  def multiply(value: BigInt, constant: ShoupConstant): BigInt =
    val normalized = modulus.normalize(value)
    val approximateQuotient = normalized * constant.precondition / radix
    val remainder = normalized * constant.value - approximateQuotient * modulus.q
    require(remainder >= 0 && remainder < 2 * modulus.q, s"Shoup remainder outside [0,2q): $remainder")
    if remainder >= modulus.q then remainder - modulus.q else remainder
