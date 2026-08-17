package ngen.transform

import ngen.algebra.{NttDomain, TransformShape}

/** Exact, normal-domain model of Kyber's seven-layer incomplete NTT.
  *
  * The production Kyber implementation stores the same constants in Montgomery
  * form. Keeping this executable specification in the normal field domain makes
  * it suitable as an oracle for either Montgomery or Barrett RTL operators.
  */
object KyberNtt:
  private def requireKyberShape(domain: NttDomain): Unit =
    require(domain.size == 256, s"Kyber schedule requires N=256, got ${domain.size}")
    require(
      domain.shape == TransformShape.IncompleteNegacyclic(2),
      s"Kyber schedule requires an incomplete negacyclic domain with base-case size two"
    )
    domain.validate()

  /** Constants root^bitReverse7(i), matching Kyber's stage traversal order. */
  def zetas(domain: NttDomain): Vector[BigInt] =
    requireKyberShape(domain)
    Vector.tabulate(128)(i => domain.modulus.pow(domain.normalizedRoot, ReferenceNtt.bitReverse(i, 7)))

  def forward(domain: NttDomain, input: Seq[BigInt]): Vector[BigInt] =
    requireKyberShape(domain)
    require(input.size == domain.size)
    val field = domain.modulus
    val constants = zetas(domain)
    val values = input.map(field.normalize).toArray
    var constantIndex = 1
    var length = 128
    while length >= 2 do
      var start = 0
      while start < domain.size do
        val zeta = constants(constantIndex)
        constantIndex += 1
        var index = start
        while index < start + length do
          val product = field.multiply(zeta, values(index + length))
          val even = values(index)
          values(index) = field.add(even, product)
          values(index + length) = field.subtract(even, product)
          index += 1
        start += 2 * length
      length /= 2
    assert(constantIndex == 128)
    values.toVector

  def inverse(domain: NttDomain, input: Seq[BigInt]): Vector[BigInt] =
    requireKyberShape(domain)
    require(input.size == domain.size)
    val field = domain.modulus
    val constants = zetas(domain)
    val values = input.map(field.normalize).toArray
    var constantIndex = 127
    var length = 2
    while length <= 128 do
      var start = 0
      while start < domain.size do
        val zeta = constants(constantIndex)
        constantIndex -= 1
        var index = start
        while index < start + length do
          val even = values(index)
          val odd = values(index + length)
          values(index) = field.add(even, odd)
          values(index + length) = field.multiply(zeta, field.subtract(odd, even))
          index += 1
        start += 2 * length
      length *= 2
    assert(constantIndex == 0)
    val scale = field.inverse(128)
    values.toVector.map(field.multiply(_, scale))
