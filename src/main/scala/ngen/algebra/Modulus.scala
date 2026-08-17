package ngen.algebra

/** Exact arithmetic in Z/qZ.
  *
  * BigInt is intentional here: this layer is the executable specification used
  * to check transform decompositions and, later, finite-width RTL operators.
  */
final case class Modulus(q: BigInt):
  require(q > 2, s"modulus must be greater than two, got $q")

  val bitWidth: Int = q.bitLength

  def normalize(value: BigInt): BigInt =
    val reduced = value % q
    if reduced.signum < 0 then reduced + q else reduced

  def add(lhs: BigInt, rhs: BigInt): BigInt = normalize(lhs + rhs)
  def subtract(lhs: BigInt, rhs: BigInt): BigInt = normalize(lhs - rhs)
  def multiply(lhs: BigInt, rhs: BigInt): BigInt = normalize(lhs * rhs)

  def pow(base: BigInt, exponent: BigInt): BigInt =
    require(exponent >= 0, s"negative exponent $exponent")
    normalize(base).modPow(exponent, q)

  def inverse(value: BigInt): BigInt =
    val normalized = normalize(value)
    require(normalized != 0, "zero has no multiplicative inverse")
    normalized.modInverse(q)

  def divide(lhs: BigInt, rhs: BigInt): BigInt = multiply(lhs, inverse(rhs))

  def hasExactPowerOfTwoOrder(value: BigInt, order: Int): Boolean =
    require(Integer.bitCount(order) == 1, s"order must be a power of two, got $order")
    pow(value, order) == 1 && (order == 1 || pow(value, order / 2) != 1)
