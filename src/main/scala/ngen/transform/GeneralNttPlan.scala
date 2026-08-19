package ngen.transform

import ngen.algebra.Modulus

enum GeneralNttAlgorithm:
  case MixedRadix, Bluestein, FourStep

final case class GeneralNttDomain(name: String, size: Int, modulus: Modulus, root: BigInt, convolutionRoot: Option[BigInt] = None):
  require(size >= 2)
  val normalizedRoot: BigInt = modulus.normalize(root)

  private def primeFactors(value: Int): Vector[Int] =
    var remaining = value
    var divisor = 2
    val result = scala.collection.mutable.ArrayBuffer.empty[Int]
    while divisor * divisor <= remaining do
      while remaining % divisor == 0 do
        result += divisor
        remaining /= divisor
      divisor += 1
    if remaining > 1 then result += remaining
    result.toVector

  val factors: Vector[Int] = primeFactors(size)
  val convolutionSize: Int = Integer.highestOneBit(2 * size - 2) << 1

  def validate(): Unit =
    require(modulus.pow(normalizedRoot, size) == 1, s"root does not have order dividing $size")
    factors.distinct.foreach(factor => require(modulus.pow(normalizedRoot, size / factor) != 1, s"root does not have exact order $size"))
    convolutionRoot.foreach(root => require(modulus.hasExactPowerOfTwoOrder(root,convolutionSize),s"convolution root must have exact order $convolutionSize"))

final case class GeneralNttPlan(domain: GeneralNttDomain, inverse: Boolean, algorithm: GeneralNttAlgorithm, fourStepFactors: Option[(Int, Int)] = None):
  domain.validate()
  val radixFactors: Vector[Int] = domain.factors
  val fourStepFactorsOrDefault: (Int, Int) =
    if algorithm != GeneralNttAlgorithm.FourStep then (1, domain.size)
    else fourStepFactors.getOrElse(GeneralNttPlan.defaultFourStepFactors(domain.size))
  val resolvedFourStepFactors: Option[(Int, Int)] =
    if algorithm == GeneralNttAlgorithm.FourStep then Some(fourStepFactorsOrDefault) else None
  require(
    algorithm != GeneralNttAlgorithm.FourStep || {
      val (first, second) = fourStepFactorsOrDefault
      first > 1 && second > 1 && first * second == domain.size
    },
    s"four-step requires a valid factorization of ${domain.size}"
  )
  fourStepFactorsOrDefault match
    case (first, second) =>
      if algorithm == GeneralNttAlgorithm.FourStep then
        require(first > 1 && first < domain.size, s"four-step first factor must be in (1, ${domain.size})")
        require(second > 1, "four-step factors must be positive and at least two")

  /** Exact executable oracle used before mixed-radix/Bluestein RTL lowering. */
  def evaluate(input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == domain.size)
    val field = domain.modulus
    val root = if inverse then field.inverse(domain.normalizedRoot) else domain.normalizedRoot
    val scale = if inverse then field.inverse(domain.size) else BigInt(1)
    Vector.tabulate(domain.size) { output =>
      val sum = input.indices.foldLeft(BigInt(0)) { (acc,index) =>
        field.add(acc, field.multiply(input(index), field.pow(root, index * output)))
      }
      field.multiply(sum, scale)
    }

object GeneralNttPlan:
  def apply(domain: GeneralNttDomain, inverse: Boolean): GeneralNttPlan =
    val algorithm = if domain.factors.size == 1 then GeneralNttAlgorithm.Bluestein else GeneralNttAlgorithm.MixedRadix
    new GeneralNttPlan(domain, inverse, algorithm)

  def apply(
      domain: GeneralNttDomain,
      inverse: Boolean,
      algorithm: GeneralNttAlgorithm,
      fourStepFactors: Option[(Int, Int)] = None
  ): GeneralNttPlan =
    new GeneralNttPlan(domain, inverse, algorithm, fourStepFactors)

  def defaultFourStepFactors(size: Int): (Int, Int) =
    require(size > 1, s"four-step requires size > 1, got $size")
    var first = Math.sqrt(size.toDouble).toInt
    while first > 1 && size % first != 0 do first -= 1
    require(first > 1, s"size $size has no two-dimensional four-step factorization")
    (first, size / first)
