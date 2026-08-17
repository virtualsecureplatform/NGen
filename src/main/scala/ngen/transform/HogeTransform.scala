package ngen.transform

import ngen.arithmetic.HogeField

object HogeTransform:
  private def butterfly(values: Array[BigInt], offset: Int, size: Int): Unit =
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      val original = values(left)
      values(left) = HogeField.add(values(left), values(right))
      values(right) = HogeField.subtract(original, values(right))

  private def inverseButterfly(values: Array[BigInt], offset: Int, size: Int, radixLog: Int): Unit =
    if radixLog > 0 then
      butterfly(values, offset, size)
      val block = size >> radixLog
      for lane <- 1 until 1 << (radixLog - 1); index <- 0 until block do
        val position = offset + size / 2 + lane * block + index
        values(position) = HogeField.shift(values(position), 3 * (lane << (6 - radixLog)))
      inverseButterfly(values, offset, size / 2, radixLog - 1)
      inverseButterfly(values, offset + size / 2, size / 2, radixLog - 1)

  private def forwardButterfly(values: Array[BigInt], offset: Int, size: Int, radixLog: Int): Unit =
    if radixLog > 0 then
      forwardButterfly(values, offset + size / 2, size / 2, radixLog - 1)
      forwardButterfly(values, offset, size / 2, radixLog - 1)
      val block = size >> radixLog
      if radixLog != 1 then
        for lane <- 1 until 1 << (radixLog - 1); index <- 0 until block do
          val position = offset + size / 2 + lane * block + index
          values(position) = HogeField.shift(values(position), 3 * (64 - (lane << (6 - radixLog))))
      butterfly(values, offset, size)

  def inverse(input: Seq[BigInt], logSize: Int, radixLog: Int): Vector[BigInt] =
    val size = 1 << logSize
    require(input.size == size && logSize % radixLog == 0)
    val tables = HogeField.tables(logSize)
    val values = input.zip(tables.inverseTwist).map(HogeField.multiply).toArray
    var sizeLog = logSize
    while sizeLog > radixLog do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = block * blockSize
        inverseButterfly(values, offset, blockSize, radixLog)
        val subblock = blockSize >> radixLog
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          values(position) = HogeField.multiply(values(position), tables.inverse(HogeField.reverse(lane, radixLog) * blockCount * index))
      sizeLog -= radixLog
    for block <- 0 until 1 << (logSize - radixLog) do inverseButterfly(values, block << radixLog, 1 << radixLog, radixLog)
    values.toVector

  def forwardResidues(input: Seq[BigInt], logSize: Int, radixLog: Int): Vector[BigInt] =
    val size = 1 << logSize
    require(input.size == size && logSize % radixLog == 0)
    val tables = HogeField.tables(logSize)
    val values = input.map(HogeField.normalize).toArray
    for block <- 0 until 1 << (logSize - radixLog) do forwardButterfly(values, block << radixLog, 1 << radixLog, radixLog)
    var sizeLog = 2 * radixLog
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> radixLog
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          values(position) = HogeField.multiply(values(position), tables.forward(HogeField.reverse(lane, radixLog) * blockCount * index))
        forwardButterfly(values, offset, blockSize, radixLog)
      sizeLog += radixLog
    val inverseSize = HogeField.inversePowerOfTwo(logSize)
    values.toVector.zip(tables.forwardTwist).map((value, twist) => HogeField.multiply(HogeField.multiply(value, twist), inverseSize))

  def inverseRadix32(input: Seq[BigInt]): Vector[BigInt] =
    val values = input.toArray
    inverseButterfly(values, 0, 32, 5)
    values.toVector

  def forwardRadix32(input: Seq[BigInt]): Vector[BigInt] =
    val values = input.toArray
    forwardButterfly(values, 0, 32, 5)
    values.toVector
