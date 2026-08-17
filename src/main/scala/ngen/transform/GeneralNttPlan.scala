package ngen.transform

import ngen.algebra.Modulus

enum GeneralNttAlgorithm:
  case MixedRadix, Bluestein

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

final case class GeneralNttPlan(domain: GeneralNttDomain, inverse: Boolean, algorithm: GeneralNttAlgorithm):
  domain.validate()
  val radixFactors: Vector[Int] = domain.factors

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
