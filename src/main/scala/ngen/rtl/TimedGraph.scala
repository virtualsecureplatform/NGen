package ngen.rtl

import scala.collection.mutable.ArrayBuffer

enum ValueFormat(val width: Int, val signed: Boolean):
  case SignedWord27 extends ValueFormat(27, true)
  case SignedWide54 extends ValueFormat(54, true)
  case Torus32 extends ValueFormat(32, false)
  case Valid extends ValueFormat(1, false)

trait Operator:
  def name: String
  def arity: Int
  def latency: Int
  def outputFormat(inputs: Vector[Signal]): ValueFormat
  def evaluate(inputs: Vector[Long]): Long

final case class InputOperator(port: String, format: ValueFormat) extends Operator:
  override val name: String = s"input:$port"
  override val arity: Int = 0
  override val latency: Int = 0
  override def outputFormat(inputs: Vector[Signal]): ValueFormat = format
  override def evaluate(inputs: Vector[Long]): Long =
    throw new UnsupportedOperationException("input values are supplied by TimedGraph.evaluate")

final case class Delay(cycles: Int, format: ValueFormat) extends Operator:
  require(cycles > 0)
  override val name: String = s"delay:$cycles"
  override val arity: Int = 1
  override val latency: Int = cycles
  override def outputFormat(inputs: Vector[Signal]): ValueFormat = format
  override def evaluate(inputs: Vector[Long]): Long = inputs.head

final case class Signal(id: Int, format: ValueFormat, availableAt: Int)

final case class Node(signal: Signal, operator: Operator, inputs: Vector[Signal])

final case class TimedGraph(nodes: Vector[Node], outputs: Vector[Signal]):
  private val byId: Map[Int, Node] = nodes.map(node => node.signal.id -> node).toMap
  require(byId.size == nodes.size, "signal identifiers must be unique")
  require(nodes.indices.forall(index => nodes(index).signal.id == index), "nodes must be topologically numbered")

  val latency: Int = outputs.map(_.availableAt).maxOption.getOrElse(0)

  def evaluate(inputValues: Map[String, Long]): Vector[Long] =
    val values = Array.ofDim[Long](nodes.size)
    nodes.foreach { node =>
      values(node.signal.id) = node.operator match
        case InputOperator(port, _) => inputValues.getOrElse(port, throw new IllegalArgumentException(s"missing input $port"))
        case operator => operator.evaluate(node.inputs.map(signal => values(signal.id)))
    }
    outputs.map(signal => values(signal.id))

  def toDot: String =
    val body = nodes.flatMap { node =>
      val label = s"${node.signal.id}: ${node.operator.name}\\nt=${node.signal.availableAt}"
      Vector(s"  n${node.signal.id} [label=\"$label\"];") ++
        node.inputs.map(input => s"  n${input.id} -> n${node.signal.id};")
    }
    (Vector("digraph timed_rtl {") ++ body ++ Vector("}")).mkString("\n")

final class TimedGraphBuilder:
  private val nodes = ArrayBuffer.empty[Node]

  private def append(operator: Operator, inputs: Vector[Signal], availableAt: Int): Signal =
    val signal = Signal(nodes.size, operator.outputFormat(inputs), availableAt)
    nodes += Node(signal, operator, inputs)
    signal

  def input(port: String, format: ValueFormat): Signal =
    require(!nodes.exists(_.operator match
      case InputOperator(existing, _) => existing == port
      case _ => false
    ), s"duplicate input port $port")
    append(InputOperator(port, format), Vector.empty, 0)

  def delay(signal: Signal, cycles: Int): Signal =
    if cycles == 0 then signal
    else append(Delay(cycles, signal.format), Vector(signal), signal.availableAt + cycles)

  /**
    * Add an operator and align all operands to their latest arrival time.
    * Explicit Delay nodes make the schedule inspectable by RTL and DOT backends.
    */
  def apply(operator: Operator, operands: Signal*): Signal =
    require(operands.size == operator.arity, s"${operator.name} expects ${operator.arity} operands, got ${operands.size}")
    val targetTime = operands.map(_.availableAt).maxOption.getOrElse(0)
    val aligned = operands.toVector.map(signal => delay(signal, targetTime - signal.availableAt))
    append(operator, aligned, targetTime + operator.latency)

  def result(outputs: Signal*): TimedGraph = TimedGraph(nodes.toVector, outputs.toVector)
