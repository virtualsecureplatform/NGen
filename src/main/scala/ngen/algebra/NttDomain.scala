package ngen.algebra

enum TransformShape:
  /** Ordinary evaluation at all powers of an N-th root. */
  case Cyclic

  /** Negacyclic transform using a primitive 2N-th root psi. */
  case Negacyclic

  /**
    * Kyber-style transform stopping at degree-one polynomial pairs.
    *
    * q = 3329 has no primitive 512-th root, so x^256 + 1 cannot be split
    * into 256 linear factors over the base field. The seven-layer Kyber NTT
    * instead produces 128 pairs and must not be treated as a cyclic NTT.
    */
  case IncompleteNegacyclic(baseCaseSize: Int)

final case class NttDomain(
    name: String,
    size: Int,
    modulus: Modulus,
    root: BigInt,
    shape: TransformShape,
    twist: Option[BigInt] = None,
    description: String = ""
):
  require(size >= 2 && Integer.bitCount(size) == 1, s"size must be a power of two, got $size")

  val logSize: Int = Integer.numberOfTrailingZeros(size)
  val normalizedRoot: BigInt = modulus.normalize(root)
  val normalizedTwist: Option[BigInt] = twist.map(modulus.normalize)

  def validate(): Unit =
    require(
      modulus.hasExactPowerOfTwoOrder(normalizedRoot, size),
      s"root $normalizedRoot does not have exact order $size modulo ${modulus.q}"
    )
    shape match
      case TransformShape.Cyclic =>
        require(normalizedTwist.isEmpty, "cyclic domains do not use a twist")
      case TransformShape.Negacyclic =>
        val psi = normalizedTwist.getOrElse(
          throw new IllegalArgumentException("negacyclic domains require a primitive 2N-th root")
        )
        require(modulus.pow(psi, size) == modulus.q - 1, s"twist^N must equal -1 modulo ${modulus.q}")
        require(modulus.multiply(psi, psi) == normalizedRoot, "twist^2 must equal the N-th root")
      case TransformShape.IncompleteNegacyclic(baseCaseSize) =>
        require(baseCaseSize >= 2 && Integer.bitCount(baseCaseSize) == 1)
        require(baseCaseSize <= size)
        require(normalizedTwist.isEmpty, "incomplete domains encode their root schedule directly")
