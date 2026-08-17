package ngen.transform

import ngen.algebra.{NttDomain, TransformShape}

/** Parameterized incomplete negacyclic transform ending in polynomial base cases. */
final case class IncompleteNttPlan(
    domain: NttDomain,
    inverse: Boolean,
    inputOrder: DataOrder,
    outputOrder: DataOrder,
    inputAddresses: Vector[Int],
    inputFactors: Vector[BigInt],
    stages: Vector[NttStage],
    outputAddresses: Vector[Int],
    outputFactors: Vector[BigInt]
) extends StreamingNttPlan:
  override def evaluate(input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == domain.size)
    val field = domain.modulus
    val work = input.map(field.normalize).toArray
    butterflies.foreach { butterfly =>
      val left = work(butterfly.left)
      val right = work(butterfly.right)
      butterfly.kind match
        case ButterflyKind.DecimationInTime =>
          val product = field.multiply(right, butterfly.twiddle)
          work(butterfly.left) = field.add(left, product)
          work(butterfly.right) = field.subtract(left, product)
        case ButterflyKind.GentlemanSande =>
          work(butterfly.left) = field.add(left, right)
          work(butterfly.right) = field.multiply(field.subtract(right, left), butterfly.twiddle)
    }
    Vector.tabulate(domain.size)(index => field.multiply(work(outputAddresses(index)), outputFactors(index)))

object IncompleteNttPlan:
  def zetas(domain: NttDomain): Vector[BigInt] =
    val baseCaseSize = domain.shape match
      case TransformShape.IncompleteNegacyclic(size) => size
      case _ => throw new IllegalArgumentException("incomplete NTT plan requires an incomplete negacyclic domain")
    domain.validate()
    val levels = domain.logSize - Integer.numberOfTrailingZeros(baseCaseSize)
    Vector.tabulate(domain.size / baseCaseSize)(index =>
      domain.modulus.pow(domain.normalizedRoot, ReferenceNtt.bitReverse(index, levels)))

  def apply(domain: NttDomain, inverse: Boolean): IncompleteNttPlan =
    val baseCaseSize = domain.shape match
      case TransformShape.IncompleteNegacyclic(size) => size
      case _ => throw new IllegalArgumentException("incomplete NTT plan requires an incomplete negacyclic domain")
    val constants = zetas(domain)
    val field = domain.modulus
    val stages = scala.collection.mutable.ArrayBuffer.empty[NttStage]
    if !inverse then
      var constantIndex = 1
      var length = domain.size / 2
      var stage = 0
      while length >= baseCaseSize do
        val operations = scala.collection.mutable.ArrayBuffer.empty[PlannedButterfly]
        var start = 0
        while start < domain.size do
          val zeta = constants(constantIndex)
          constantIndex += 1
          for index <- start until start + length do
            operations += PlannedButterfly(stage, index, index + length, zeta, ButterflyKind.DecimationInTime)
          start += 2 * length
        stages += NttStage(stage, 2 * length, operations.toVector)
        stage += 1
        length /= 2
      require(constantIndex == constants.size)
    else
      var constantIndex = constants.size - 1
      var length = baseCaseSize
      var stage = 0
      while length <= domain.size / 2 do
        val operations = scala.collection.mutable.ArrayBuffer.empty[PlannedButterfly]
        var start = 0
        while start < domain.size do
          val zeta = constants(constantIndex)
          constantIndex -= 1
          for index <- start until start + length do
            operations += PlannedButterfly(stage, index, index + length, zeta, ButterflyKind.GentlemanSande)
          start += 2 * length
        stages += NttStage(stage, 2 * length, operations.toVector)
        stage += 1
        length *= 2
      require(constantIndex == 0)
    val scale = if inverse then field.inverse(domain.size / baseCaseSize) else BigInt(1)
    IncompleteNttPlan(
      domain, inverse, DataOrder.Natural, DataOrder.Natural,
      Vector.tabulate(domain.size)(identity), Vector.fill(domain.size)(BigInt(1)), stages.toVector,
      Vector.tabulate(domain.size)(identity), Vector.fill(domain.size)(scale)
    )
