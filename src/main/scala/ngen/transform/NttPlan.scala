package ngen.transform

import ngen.algebra.{NttDomain, TransformShape}

enum DataOrder:
  case Natural, BitReversed

object DataOrder:
  def parse(value: String): DataOrder = value.toLowerCase.replace("-", "") match
    case "natural" => DataOrder.Natural
    case "bitreversed" | "bitreverse" => DataOrder.BitReversed
    case other => throw new IllegalArgumentException(s"unknown data order '$other'; expected natural or bitreversed")

final case class PlannedButterfly(
    stage: Int,
    left: Int,
    right: Int,
    twiddle: BigInt,
    kind: ButterflyKind = ButterflyKind.DecimationInTime
)

final case class NttStage(stage: Int, span: Int, butterflies: Vector[PlannedButterfly])

enum ButterflyKind:
  /** (a, b) -> (a + wb, a - wb). */
  case DecimationInTime
  /** (a, b) -> (a + b, w(b - a)). */
  case GentlemanSande

trait StreamingNttPlan:
  def domain: NttDomain
  def inverse: Boolean
  def inputOrder: DataOrder
  def outputOrder: DataOrder
  def inputAddresses: Vector[Int]
  def inputFactors: Vector[BigInt]
  def stages: Vector[NttStage]
  def outputAddresses: Vector[Int]
  def outputFactors: Vector[BigInt]
  final def butterflies: Vector[PlannedButterfly] = stages.flatMap(_.butterflies)
  def evaluate(input: Seq[BigInt]): Vector[BigInt]

/** Field-correct transform plan independent of a particular RTL datapath. */
final case class NttPlan(
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
    val work = Array.fill(domain.size)(BigInt(0))
    input.indices.foreach { index =>
      work(inputAddresses(index)) = field.multiply(input(index), inputFactors(index))
    }
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
    Vector.tabulate(domain.size) { index =>
      field.multiply(work(outputAddresses(index)), outputFactors(index))
    }

object NttPlan:
  def radix2(
      domain: NttDomain,
      inverse: Boolean,
      inputOrder: DataOrder = DataOrder.Natural,
      outputOrder: DataOrder = DataOrder.Natural
  ): NttPlan =
    require(domain.shape == TransformShape.Cyclic || domain.shape == TransformShape.Negacyclic,
      "radix-2 plan requires a complete cyclic or negacyclic domain")
    domain.validate()
    val field = domain.modulus
    val root = if inverse then field.inverse(domain.normalizedRoot) else domain.normalizedRoot
    val inputAddresses = Vector.tabulate(domain.size) { index =>
      if inputOrder == DataOrder.Natural then ReferenceNtt.bitReverse(index, domain.logSize) else index
    }
    val inputFactors = Vector.tabulate(domain.size) { index =>
      if !inverse && domain.shape == TransformShape.Negacyclic then
        val naturalIndex = if inputOrder == DataOrder.Natural then index else ReferenceNtt.bitReverse(index, domain.logSize)
        field.pow(domain.normalizedTwist.get, naturalIndex)
      else BigInt(1)
    }
    val stages = Vector.tabulate(domain.logSize) { stage =>
      val span = 1 << (stage + 1)
      val half = span / 2
      val stepRoot = field.pow(root, domain.size / span)
      val butterflies = (for
        block <- 0 until domain.size by span
        index <- 0 until half
      yield PlannedButterfly(stage, block + index, block + index + half, field.pow(stepRoot, index))).toVector
      NttStage(stage, span, butterflies)
    }
    val outputAddresses = Vector.tabulate(domain.size) { index =>
      if outputOrder == DataOrder.Natural then index else ReferenceNtt.bitReverse(index, domain.logSize)
    }
    val outputFactors = Vector.tabulate(domain.size) { index =>
      if inverse then
        val naturalIndex = if outputOrder == DataOrder.Natural then index else ReferenceNtt.bitReverse(index, domain.logSize)
        val scale = field.inverse(domain.size)
        domain.normalizedTwist match
          case Some(psi) => field.multiply(scale, field.pow(field.inverse(psi), naturalIndex))
          case None => scale
      else BigInt(1)
    }
    NttPlan(domain, inverse, inputOrder, outputOrder, inputAddresses, inputFactors, stages, outputAddresses, outputFactors)
