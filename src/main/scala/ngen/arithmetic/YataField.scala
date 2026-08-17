package ngen.arithmetic

final case class YataTables(
    inttTwist: Vector[Long],
    nttTwist: Vector[Long],
    inttTable0: Vector[Long],
    inttTable1: Vector[Long],
    nttTable0: Vector[Long],
    nttTable1: Vector[Long]
)

/** Software specification of YATA's signed 27-bit Montgomery-like reduction. */
object YataField:
  val Modulus: Long = 40960001L
  val K: Long = 625L
  val Shift: Int = 16
  val WordBits: Int = 27
  val WordMask: Long = (1L << WordBits) - 1
  val R: Long = (1L << WordBits) % Modulus
  val R2: Long = (R * R) % Modulus
  val RadixLog: Int = 3

  def signedWord(value: Long): Long =
    val truncated = value & WordMask
    if (truncated & (1L << (WordBits - 1))) != 0 then truncated - (1L << WordBits)
    else truncated

  def addMod(lhs: Long, rhs: Long): Long =
    val result = lhs + rhs
    if result >= Modulus then result - Modulus
    else if result <= -Modulus then result + Modulus
    else result

  def subtractMod(lhs: Long, rhs: Long): Long =
    val result = lhs - rhs
    if result >= Modulus then result - Modulus
    else if result <= -Modulus then result + Modulus
    else result

  def signedReduce(value: Long): Long =
    val low = value & WordMask
    val high = value >> WordBits
    val m = signedWord(-((low * K) << Shift) + low)
    val correction = (((m * K) << Shift) + m) >> WordBits
    signedWord(high - correction)

  def unsignedReduce(value: Long): Long =
    val low = value & WordMask
    val rawM = (((low * K) << Shift) - low) & WordMask
    val m = if (rawM & (1L << (WordBits - 1))) != 0 then rawM - (1L << WordBits) else rawM
    val reduced = (value + ((m * K) << Shift) + m) >> WordBits
    if reduced > Modulus then reduced - Modulus
    else if reduced < 0 then reduced + Modulus
    else reduced

  def multiplySigned(lhs: Long, rhs: Long): Long = signedReduce(lhs * rhs)
  def multiplyUnsigned(lhs: Long, rhs: Long): Long = unsignedReduce(lhs * rhs)

  def powRedc(base: Long, exponent: Int): Long =
    require(exponent >= 0)
    var result = 1L
    val baseR = multiplyUnsigned(R2, base)
    var index = 0
    while index < exponent do
      result = multiplyUnsigned(result, baseR)
      index += 1
    result

  def tables(logSize: Int): YataTables =
    require(logSize >= RadixLog && logSize < Shift - 1)
    val size = 1 << logSize
    val root = powRedc(31, K.toInt)
    val inverseSize = BigInt(size).modInverse(BigInt(Modulus)).toLong

    val twist = Array.fill(2, size)(0L)
    val twistWR = multiplyUnsigned(powRedc(root, 1 << (Shift - logSize - 1)), R2)
    twist(1)(0) = R
    for index <- 1 until size do twist(1)(index) = multiplyUnsigned(twist(1)(index - 1), twistWR)
    twist(0)(size - 1) = multiplyUnsigned(
      multiplyUnsigned(twist(1)(size - 1), twistWR),
      (inverseSize * twistWR) % Modulus
    )
    twist(0)(0) = (inverseSize * R) % Modulus
    for index <- 2 until size do twist(0)(size - index) = multiplyUnsigned(twist(0)(size - index + 1), twistWR)
    for index <- 0 until size do
      if ((index >> (logSize - RadixLog)) & ((1 << (RadixLog - 1)) - 1)) != 0 then
        twist(0)(index) = multiplyUnsigned(twist(0)(index), R2)

    val table = Array.fill(2, 2, size)(0L)
    val tableW = powRedc(root, 1 << (Shift - logSize))
    val tableWR = multiplyUnsigned(tableW, R2)
    table(0)(0)(0) = R
    table(1)(0)(0) = R
    table(0)(1)(0) = R2
    table(1)(1)(0) = R2
    for index <- 1 until size do table(1)(0)(index) = multiplyUnsigned(table(1)(0)(index - 1), tableWR)
    for index <- 1 until size do table(1)(1)(index) = multiplyUnsigned(table(1)(0)(index), R2)
    for lane <- 0 until 2; index <- 1 until size do table(0)(lane)(index) = table(1)(lane)(size - index)

    YataTables(
      twist(1).toVector.map(signedWord),
      twist(0).toVector.map(signedWord),
      table(1)(0).toVector.map(signedWord),
      table(1)(1).toVector.map(signedWord),
      table(0)(0).toVector.map(signedWord),
      table(0)(1).toVector.map(signedWord)
    )
