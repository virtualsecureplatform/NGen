package ngen.transform

import ngen.algebra.{NttDomain, TransformShape}

object ReferenceNtt:
  def bitReverse(value: Int, bits: Int): Int =
    Integer.reverse(value) >>> (Integer.SIZE - bits)

  def cyclicPlan(domain: NttDomain, inverse: Boolean = false): Transform =
    require(domain.shape == TransformShape.Cyclic || domain.shape == TransformShape.Negacyclic)
    domain.validate()
    val modulus = domain.modulus
    val root = if inverse then modulus.inverse(domain.normalizedRoot) else domain.normalizedRoot
    val permutation = Permutation(Vector.tabulate(domain.size)(bitReverse(_, domain.logSize)))
    val stages = Vector.iterate(2, domain.logSize)(_ * 2).map(Radix2Stage(domain.size, _, root))
    val scale =
      if inverse then Vector.fill(domain.size)(modulus.inverse(domain.size))
      else Vector.fill(domain.size)(BigInt(1))
    Compose(permutation +: stages :+ Diagonal(scale))

  def forward(domain: NttDomain, input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == domain.size)
    domain.shape match
      case TransformShape.Cyclic => cyclicPlan(domain).eval(input.toVector, domain.modulus)
      case TransformShape.Negacyclic =>
        val psi = domain.normalizedTwist.get
        val twist = Vector.tabulate(domain.size)(i => domain.modulus.pow(psi, i))
        cyclicPlan(domain).eval(Diagonal(twist).eval(input.toVector, domain.modulus), domain.modulus)
      case TransformShape.IncompleteNegacyclic(_) =>
        KyberNtt.forward(domain, input)

  def inverse(domain: NttDomain, input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == domain.size)
    domain.shape match
      case TransformShape.Cyclic => cyclicPlan(domain, inverse = true).eval(input.toVector, domain.modulus)
      case TransformShape.Negacyclic =>
        val untwisted = cyclicPlan(domain, inverse = true).eval(input.toVector, domain.modulus)
        val psiInverse = domain.modulus.inverse(domain.normalizedTwist.get)
        val untwist = Vector.tabulate(domain.size)(i => domain.modulus.pow(psiInverse, i))
        Diagonal(untwist).eval(untwisted, domain.modulus)
      case TransformShape.IncompleteNegacyclic(_) =>
        KyberNtt.inverse(domain, input)
