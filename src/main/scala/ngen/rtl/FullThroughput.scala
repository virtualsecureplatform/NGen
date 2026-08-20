package ngen.rtl

/** SGen-style token-timed streaming shape. */
final case class StreamingShape(size: Int, width: Int):
  require(size > 0 && width > 0 && size % width == 0)
  require(Integer.bitCount(size) == 1 && Integer.bitCount(width) == 1)
  val cycles: Int = size / width
  val logSize: Int = Integer.numberOfTrailingZeros(size)
  val logWidth: Int = Integer.numberOfTrailingZeros(width)

/** Executable, latency-carrying node in a full-throughput dataflow. */
sealed trait FullThroughputNode:
  def size: Int
  def latency: Int
  def name: String
  def evaluate(input: Vector[BigInt]): Vector[BigInt]

final case class StreamingKernel(
    name: String,
    size: Int,
    latency: Int,
    operation: Vector[BigInt] => Vector[BigInt]
) extends FullThroughputNode:
  require(size > 0 && latency >= 0)
  override def evaluate(input: Vector[BigInt]): Vector[BigInt] =
    require(input.size == size)
    val output = operation(input)
    require(output.size == size)
    output

final case class StreamingPermutation(name: String, mapping: Vector[Int], latency: Int) extends FullThroughputNode:
  require(mapping.sorted == mapping.indices)
  require(latency >= 0)
  override val size: Int = mapping.size
  override def evaluate(input: Vector[BigInt]): Vector[BigInt] =
    require(input.size == size)
    mapping.map(input)

final case class StreamingCompose(nodes: Vector[FullThroughputNode]) extends FullThroughputNode:
  require(nodes.nonEmpty && nodes.map(_.size).distinct.size == 1)
  override val size: Int = nodes.head.size
  override val latency: Int = nodes.map(_.latency).sum
  override val name: String = nodes.map(_.name).mkString("compose(", ",", ")")
  override def evaluate(input: Vector[BigInt]): Vector[BigInt] = nodes.foldLeft(input)((values, node) => node.evaluate(values))

/** Complete token contract for an acyclic full-throughput pipeline. */
final case class FullThroughputPlan(shape: StreamingShape, root: FullThroughputNode):
  require(root.size == shape.size)
  val latency: Int = root.latency
  val initiationInterval: Int = shape.cycles
  val minimumGap: Int = 0
  def evaluate(input: Seq[BigInt]): Vector[BigInt] = root.evaluate(input.toVector)
