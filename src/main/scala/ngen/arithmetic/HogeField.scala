package ngen.arithmetic

object HogeField:
  val Modulus: BigInt = BigInt("18446744069414584321")
  val Generator: BigInt = BigInt("12037493425763644479")

  def normalize(value: BigInt): BigInt =
    val reduced = value % Modulus
    if reduced < 0 then reduced + Modulus else reduced
  def add(left: BigInt, right: BigInt): BigInt = normalize(left + right)
  def subtract(left: BigInt, right: BigInt): BigInt = normalize(left - right)
  def multiply(left: BigInt, right: BigInt): BigInt = normalize(left * right)
  def shift(left: BigInt, amount: Int): BigInt = multiply(left, BigInt(2).modPow(amount, Modulus))
  def power(base: BigInt, exponent: Int): BigInt = normalize(base).modPow(exponent, Modulus)

  def inversePowerOfTwo(logSize: Int): BigInt = BigInt(1 << (32 - logSize)) + 1 + ((BigInt(1) << 32) - (BigInt(1 << (32 - logSize)) + 1) << 32)

  def reverse(value: Int, bits: Int): Int = Integer.reverse(value) >>> (32 - bits)

  final case class Tables(forward: Vector[BigInt], inverse: Vector[BigInt], forwardTwist: Vector[BigInt], inverseTwist: Vector[BigInt])

  def tables(logSize: Int): Tables =
    val size = 1 << logSize
    val tableRoot = power(Generator, 1 << (32 - logSize))
    val inverseTable = Vector.iterate(BigInt(1), size)(multiply(_, tableRoot))
    val forwardTable = BigInt(1) +: Vector.tabulate(size - 1)(i => inverseTable(size - i - 1))
    val twistRoot = power(Generator, 1 << (32 - logSize - 1))
    val inverseTwist = Vector.iterate(BigInt(1), size)(multiply(_, twistRoot))
    val forwardTwist = Vector.tabulate(size)(i => if i == 0 then BigInt(1) else inverseTwist(i).modInverse(Modulus))
    Tables(forwardTable, inverseTable, forwardTwist, inverseTwist)
