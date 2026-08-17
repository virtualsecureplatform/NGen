package ngen.transform

import ngen.arithmetic.YataField

/** Parameterized executable specification of YATA's radix-8 RAINTT. */
object YataTransform:
  private def reverse3(value: Int): Int = ((value & 1) << 2) | (value & 2) | ((value & 4) >> 2)

  private def constantMultiply(value: Long, radixLog: Int, number: Int): Long =
    var factor = 1L
    for _ <- 0 until number * (4 >> (radixLog - 1)) do factor *= 5
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

  private def inverseRadix8(values: Array[Long], offset: Int, size: Int): Unit =
    addAdd(values, offset, size)
    inverseRadix4(values, offset, size / 2)
    val block = size >> 3
    for index <- 0 until block do
      val position = offset + size / 2 + 2 * block + index
      values(position) = constantMultiply(values(position), 3, 2)
      val left = offset + size / 2 + index
      val original = values(left)
      values(left) += values(position)
      values(position) = original - values(position)
    for index <- 0 until block do
      val left = offset + size / 2 + block + index
      val right = offset + size / 2 + 3 * block + index
      val temporary = constantMultiply(values(left), 3, 3)
      values(left) = constantMultiply(values(left), 3, 1) + constantMultiply(values(right), 3, 3)
      values(right) = temporary + constantMultiply(values(right), 3, 1)
    bothSredc(values, offset + size / 2, size / 4)
    bothSredc(values, offset + 3 * size / 4, size / 4)

  private def forwardRadix4(values: Array[Long], offset: Int, size: Int, reduce: Boolean): Unit =
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
      if reduce then
        values(left) = YataField.signedReduce(values(left) + values(right))
        values(right) = YataField.signedReduce(original - values(right))
      else
        values(left) += values(right)
        values(right) = original - values(right)

  private def forwardRadix8(values: Array[Long], offset: Int, size: Int): Unit =
    forwardRadix4(values, offset, size / 2, reduce = false)
    bothMod(values, offset + size / 2, size / 4)
    bothMod(values, offset + 3 * size / 4, size / 4)
    val block = size >> 3
    for relative <- size / 2 until size / 2 + block do
      val left = offset + relative
      val right = left + size / 4
      val original = values(left)
      values(left) = YataField.addMod(values(left), values(right))
      values(right) = -constantMultiply(original - values(right), 3, 2)
    for relative <- size / 2 + block until size / 2 + 2 * block do
      val left = offset + relative
      val right = left + size / 4
      val temporary = -constantMultiply(values(left), 3, 1)
      values(left) = -constantMultiply(values(left), 3, 3) - constantMultiply(values(right), 3, 1)
      values(right) = temporary - constantMultiply(values(right), 3, 3)
    for index <- 0 until block do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.addMod(values(left), values(right))
      values(right) = YataField.subtractMod(original, values(right))
    for group <- 1 until 4; index <- group * block until (group + 1) * block do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = YataField.signedReduce(values(left) + values(right))
      values(right) = YataField.signedReduce(original - values(right))

  def inverse(input: Seq[Long], logSize: Int): Vector[Long] =
    val size = 1 << logSize
    require(input.size == size && logSize >= 3 && logSize % 3 == 0)
    val tables = YataField.tables(logSize)
    val values = input.zip(tables.inttTwist).map(YataField.multiplySigned).toArray
    var sizeLog = logSize
    while sizeLog > 3 do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = blockSize * block
        inverseRadix8(values, offset, blockSize)
        val subblock = blockSize >> 3
        for lane <- 1 until 8 do
          val stride = reverse3(lane) * blockCount
          val table = if lane > 1 then tables.inttTable1 else tables.inttTable0
          for index <- 0 until subblock do
            val position = offset + lane * subblock + index
            values(position) = YataField.multiplySigned(values(position), table(stride * index))
      sizeLog -= 3
    for block <- 0 until 1 << (logSize - 3) do inverseRadix8(values, 8 * block, 8)
    values.toVector.map(YataField.signedWord)

  def forwardResidues(input: Seq[Long], logSize: Int): Vector[Long] =
    val size = 1 << logSize
    require(input.size == size && logSize >= 3 && logSize % 3 == 0)
    val tables = YataField.tables(logSize)
    val values = input.map(YataField.signedWord).toArray
    for block <- 0 until 1 << (logSize - 3) do forwardRadix8(values, 8 * block, 8)
    var sizeLog = 6
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> 3
      for block <- 0 until blockCount do
        val offset = blockSize * block
        for lane <- 0 until 8 do
          val stride = reverse3(lane) * blockCount
          for index <- 0 until subblock do
            val position = offset + lane * subblock + index
            val tableOne = ((index >> (sizeLog - 6)) & 3) != 0
            if stride == 0 then
              if tableOne then values(position) = YataField.multiplySigned(values(position), YataField.R2)
            else
              val table = if tableOne then tables.nttTable1 else tables.nttTable0
              values(position) = YataField.multiplySigned(values(position), table(stride * index))
        forwardRadix8(values, offset, blockSize)
      sizeLog += 3
    values.toVector.zip(tables.nttTwist).map(YataField.multiplySigned)

  def forwardTorus(input: Seq[Long], logSize: Int): Vector[Long] =
    val scale = (BigInt(1) << (32 + YataField.WordBits - 1)) / YataField.Modulus
    val rounding = BigInt(1) << (YataField.WordBits - 2)
    forwardResidues(input, logSize).map { residue =>
      val positive = if residue < 0 then residue + YataField.Modulus else residue
      (((BigInt(positive) * scale + rounding) >> (YataField.WordBits - 1)) & 0xffffffffL).toLong
    }
