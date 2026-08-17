package ngen.transform

import ngen.algebra.Modulus

sealed trait Transform:
  def size: Int
  def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt]

final case class Identity(size: Int) extends Transform:
  override def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt] =
    require(input.size == size)
    input.map(modulus.normalize)

/** output(i) = input(indices(i)). */
final case class Permutation(indices: Vector[Int]) extends Transform:
  override val size: Int = indices.size
  require(indices.sorted == (0 until size).toVector, "indices must be a permutation")

  override def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt] =
    require(input.size == size)
    indices.map(input).map(modulus.normalize)

final case class Diagonal(factors: Vector[BigInt]) extends Transform:
  override val size: Int = factors.size

  override def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt] =
    require(input.size == size)
    input.zip(factors).map(modulus.multiply)

/** One iterative Cooley-Tukey radix-2 stage with the given butterfly span. */
final case class Radix2Stage(size: Int, span: Int, root: BigInt) extends Transform:
  require(span >= 2 && Integer.bitCount(span) == 1 && size % span == 0)

  override def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt] =
    require(input.size == size)
    val output = input.map(modulus.normalize).toArray
    val half = span / 2
    val stepRoot = modulus.pow(root, size / span)
    var block = 0
    while block < size do
      var twiddle = BigInt(1)
      var index = 0
      while index < half do
        val even = output(block + index)
        val odd = modulus.multiply(output(block + index + half), twiddle)
        output(block + index) = modulus.add(even, odd)
        output(block + index + half) = modulus.subtract(even, odd)
        twiddle = modulus.multiply(twiddle, stepRoot)
        index += 1
      block += span
    output.toVector

/** Composition is evaluated from first to last. */
final case class Compose(parts: Vector[Transform]) extends Transform:
  require(parts.nonEmpty, "a composition needs at least one transform")
  override val size: Int = parts.head.size
  require(parts.forall(_.size == size), "all composed transforms must have the same size")

  override def eval(input: Vector[BigInt], modulus: Modulus): Vector[BigInt] =
    parts.foldLeft(input)((values, transform) => transform.eval(values, modulus))
