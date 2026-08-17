package ngen.transform

import ngen.arithmetic.YataField

/** Executable specification of YATA's compressed radix-8 RAINTT block. */
object YataRadix8:
  val Size = 8

  private def constantMultiply(value: Long, radixLog: Int, number: Int): Long =
    val repetitions = number * (4 >> (radixLog - 1))
    var factor = 1L
    for _ <- 0 until repetitions do factor *= 5
    (value * factor) << (number * (16 >> (radixLog - 1)))

  private def bothMod(values: Array[Long], offset: Int, size: Int): Unit =
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.addMod(values(left), values(right))
      values(right) = YataField.subtractMod(original, values(right))

  private def addAdd(values: Array[Long], offset: Int, size: Int): Unit =
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.addMod(values(left), values(right))
      values(right) = original - values(right)

  private def bothSredc(values: Array[Long], offset: Int, size: Int): Unit =
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.signedReduce(values(left) + values(right))
      values(right) = YataField.signedReduce(original - values(right))

  private def inverseRadix4(values: Array[Long], offset: Int, size: Int): Unit =
    addAdd(values, offset, size)
    bothMod(values, offset, size / 2)
    val block = size >> 2
    for index <- 0 until block do
      val position = offset + index + size / 2 + block
      values(position) = constantMultiply(values(position), 2, 1)
    bothSredc(values, offset + size / 2, size / 2)

  private def inverseRadix8(values: Array[Long], offset: Int): Unit =
    addAdd(values, offset, 8)
    inverseRadix4(values, offset, 4)
    values(offset + 6) = constantMultiply(values(offset + 6), 3, 2)
    val value4 = values(offset + 4)
    values(offset + 4) += values(offset + 6)
    values(offset + 6) = value4 - values(offset + 6)
    val temporary = constantMultiply(values(offset + 5), 3, 3)
    values(offset + 5) = constantMultiply(values(offset + 5), 3, 1) + constantMultiply(values(offset + 7), 3, 3)
    values(offset + 7) = temporary + constantMultiply(values(offset + 7), 3, 1)
    bothSredc(values, offset + 4, 2)
    bothSredc(values, offset + 6, 2)

  private def forwardRadix4(values: Array[Long], offset: Int, size: Int): Unit =
    bothMod(values, offset, size / 2)
    bothMod(values, offset + size / 2, size / 2)
    for index <- 0 until size / 4 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.addMod(values(left), values(right))
      values(right) = YataField.subtractMod(original, values(right))
    for index <- size / 4 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(right) = -constantMultiply(values(right), 2, 1)
      values(left) += values(right)
      values(right) = original - values(right)

  private def forwardRadix8(values: Array[Long], offset: Int): Unit =
    forwardRadix4(values, offset, 4)
    bothMod(values, offset + 4, 2)
    bothMod(values, offset + 6, 2)
    val original4 = values(offset + 4)
    values(offset + 4) = YataField.addMod(values(offset + 4), values(offset + 6))
    values(offset + 6) = -constantMultiply(original4 - values(offset + 6), 3, 2)
    val temporary = -constantMultiply(values(offset + 5), 3, 1)
    values(offset + 5) = -constantMultiply(values(offset + 5), 3, 3) - constantMultiply(values(offset + 7), 3, 1)
    values(offset + 7) = temporary - constantMultiply(values(offset + 7), 3, 3)
    val original0 = values(offset)
    values(offset) = YataField.addMod(values(offset), values(offset + 4))
    values(offset + 4) = YataField.subtractMod(original0, values(offset + 4))
    for index <- 1 until 4 do
      val left = offset + index
      val right = left + 4
      val original = values(left)
      values(left) = YataField.signedReduce(values(left) + values(right))
      values(right) = YataField.signedReduce(original - values(right))

  def inverse(input: Seq[Long]): Vector[Long] =
    require(input.size == Size)
    val twist = YataField.tables(3).inttTwist
    val values = input.zip(twist).map(YataField.multiplySigned).toArray
    inverseRadix8(values, 0)
    values.toVector.map(YataField.signedWord)

  def forwardResidues(input: Seq[Long]): Vector[Long] =
    require(input.size == Size)
    val values = input.map(YataField.signedWord).toArray
    forwardRadix8(values, 0)
    val twist = YataField.tables(3).nttTwist
    values.toVector.zip(twist).map(YataField.multiplySigned)

  def forwardTorus(input: Seq[Long]): Vector[Long] =
    val scale = (BigInt(1) << (32 + YataField.WordBits - 1)) / YataField.Modulus
    val rounding = BigInt(1) << (YataField.WordBits - 2)
    forwardResidues(input).map { residue =>
      val positive = if residue < 0 then residue + YataField.Modulus else residue
      (((BigInt(positive) * scale + rounding) >> (YataField.WordBits - 1)) & 0xffffffffL).toLong
    }
