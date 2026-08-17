package ngen.arithmetic

import ngen.algebra.{Modulus, NttDomain, TransformShape}

/** Classical Fermat-prime field F_m = 2^(2^m) + 1, m <= 4. */
final case class FermatField(index: Int):
  require(index >= 0 && index <= 4, "the classical prime Fermat fields are F0 through F4")
  val exponentBits: Int = 1 << index
  val modulus: Modulus = Modulus((BigInt(1) << exponentBits) + 1)
  val rootOrder: Int = 2 * exponentBits
  private val generalized = GeneralizedFermatField(2, index)

  def domain(logSize: Int): NttDomain =
    val size = 1 << logSize
    require(size <= rootOrder, s"F$index supports transform size at most $rootOrder")
    generalized.domain(logSize).copy(name = s"fermat$index-$size",
      description = s"classical Fermat number transform modulo F$index=${modulus.q}")

  /** Multiplication by 2^shift; reduction uses 2^B == -1 (mod F_m). */
  def shiftMultiply(value: BigInt, shift: Int): BigInt =
    val period = 2 * exponentBits
    val normalizedShift = Math.floorMod(shift, period)
    val lowShift = normalizedShift % exponentBits
    val shifted = modulus.normalize(value) << lowShift
    val signed = if normalizedShift >= exponentBits then -shifted else shifted
    modulus.normalize(signed)
