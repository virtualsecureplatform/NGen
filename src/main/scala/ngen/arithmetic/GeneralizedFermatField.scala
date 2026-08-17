package ngen.arithmetic

import ngen.algebra.{Modulus, NttDomain, TransformShape}

/** Prime field q = base^(2^index) + 1 with base as a 2^(index+1)-order root. */
final case class GeneralizedFermatField(base: BigInt, index: Int):
  require(base >= 2)
  require(index >= 0 && index < 30)
  val exponent: Int = 1 << index
  val modulus: Modulus = Modulus(base.pow(exponent) + 1)
  require(modulus.q.isProbablePrime(80), s"${modulus.q} is not a generalized Fermat prime")
  val rootOrder: Int = 2 * exponent

  def domain(logSize: Int): NttDomain =
    val size = 1 << logSize
    require(size <= rootOrder, s"base=$base index=$index supports transform size at most $rootOrder")
    val root = modulus.pow(base, rootOrder / size)
    NttDomain(s"generalized-fermat-$base-$index-$size", size, modulus, root, TransformShape.Cyclic,
      description = s"generalized Fermat transform modulo $base^(2^$index)+1")

  def powerMultiply(value: BigInt, power: Int): BigInt =
    val normalizedPower = Math.floorMod(power, rootOrder)
    modulus.multiply(value, modulus.pow(base, normalizedPower))
