package ngen.rtl

import ngen.algebra.{NttDomain, TransformShape}
import ngen.transform.ReferenceNtt

object GenericNttGraph:
  def build(domain: NttDomain, inverse: Boolean, profile: PipelineProfile): TimedGraph =
    require(domain.shape == TransformShape.Cyclic || domain.shape == TransformShape.Negacyclic)
    domain.validate()
    val builder = TimedGraphBuilder()
    val field = domain.modulus
    val multiplyLatency = profile.multiplierLatency + profile.reductionLatency
    var values = Vector.tabulate(domain.size)(i => builder.input(s"i$i", ValueFormat.unsigned(field.bitWidth)))

    if domain.shape == TransformShape.Negacyclic && !inverse then
      val psi = domain.normalizedTwist.get
      values = values.zipWithIndex.map((signal, index) =>
        builder(BarrettMultiplyConstant(field, field.pow(psi, index), multiplyLatency), signal)
      )

    values = Vector.tabulate(domain.size)(i => values(ReferenceNtt.bitReverse(i, domain.logSize)))
    val root = if inverse then field.inverse(domain.normalizedRoot) else domain.normalizedRoot
    var span = 2
    while span <= domain.size do
      val half = span / 2
      val stepRoot = field.pow(root, domain.size / span)
      val next = values.toArray
      var block = 0
      while block < domain.size do
        var twiddle = BigInt(1)
        var index = 0
        while index < half do
          val even = values(block + index)
          val odd = builder(BarrettMultiplyConstant(field, twiddle, multiplyLatency), values(block + index + half))
          next(block + index) = builder(ModularAdd(field, profile.addLatency), even, odd)
          next(block + index + half) = builder(ModularSubtract(field, profile.addLatency), even, odd)
          twiddle = field.multiply(twiddle, stepRoot)
          index += 1
        block += span
      values = next.toVector
      span *= 2

    if inverse then
      val inverseSize = field.inverse(domain.size)
      val psiInverse = domain.normalizedTwist.map(field.inverse)
      values = values.zipWithIndex.map((signal, index) =>
        val scale = psiInverse match
          case Some(psi) => field.multiply(inverseSize, field.pow(psi, index))
          case None => inverseSize
        builder(BarrettMultiplyConstant(field, scale, multiplyLatency), signal)
      )

    builder.result(values*)
