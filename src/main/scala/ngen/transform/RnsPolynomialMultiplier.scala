package ngen.transform

import ngen.algebra.{NttDomain, TransformShape}

final case class RnsBasis(domains: Vector[NttDomain]):
  require(domains.nonEmpty)
  require(domains.map(_.size).distinct.size == 1, "RNS domains must use the same transform size")
  require(domains.map(_.shape).distinct.size == 1, "RNS domains must use the same transform shape")
  require(domains.forall(domain => domain.shape == TransformShape.Cyclic || domain.shape == TransformShape.Negacyclic),
    "RNS pointwise multiplication requires complete transforms")
  domains.foreach(_.validate())
  for left <- domains.indices; right <- left + 1 until domains.size do
    require(domains(left).modulus.q.gcd(domains(right).modulus.q) == 1, "RNS moduli must be pairwise coprime")

  val size: Int = domains.head.size
  val combinedModulus: BigInt = domains.map(_.modulus.q).product

  def reconstruct(residues: Seq[BigInt]): BigInt =
    require(residues.size == domains.size)
    domains.indices.foldLeft(BigInt(0)) { (sum,index) =>
      val modulus = domains(index).modulus.q
      val partial = combinedModulus / modulus
      (sum + residues(index).mod(modulus) * partial * partial.modInverse(modulus)).mod(combinedModulus)
    }

object RnsPolynomialMultiplier:
  def multiply(basis: RnsBasis, lhs: Seq[BigInt], rhs: Seq[BigInt]): Vector[BigInt] =
    require(lhs.size == basis.size && rhs.size == basis.size)
    val residueResults = basis.domains.map { domain =>
      val leftTransform = ReferenceNtt.forward(domain, lhs)
      val rightTransform = ReferenceNtt.forward(domain, rhs)
      val products = leftTransform.zip(rightTransform).map(domain.modulus.multiply)
      ReferenceNtt.inverse(domain, products)
    }
    Vector.tabulate(basis.size)(index => basis.reconstruct(residueResults.map(_(index))))
