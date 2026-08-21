package ngen.rtl

final case class SwitchTransposeSpec(inputCycleLog: Int, inputLaneLog: Int, dataWidth: Int):
  require(inputCycleLog >= 0, s"input cycle log must be nonnegative, got $inputCycleLog")
  require(inputLaneLog >= 0, s"input lane log must be nonnegative, got $inputLaneLog")
  require(inputCycleLog + inputLaneLog > 0, "rectangular transpose must contain at least two elements")
  require(dataWidth > 0, s"data width must be positive, got $dataWidth")
  val inputCycles: Int = 1 << inputCycleLog
  val inputLanes: Int = 1 << inputLaneLog
  val outputCycles: Int = inputLanes
  val outputLanes: Int = inputCycles
  val size: Int = inputLanes // Backward-compatible square-network lane count.
  val square: Boolean = inputCycleLog == inputLaneLog
  val logSize: Int = inputLaneLog
  val halfCycle: Int = inputCycles / 2
  val latency: Int = if square then inputLanes - 1 else inputCycles

object SwitchTransposeSpec:
  def apply(logSize: Int, dataWidth: Int): SwitchTransposeSpec = SwitchTransposeSpec(logSize, logSize, dataWidth)

object SwitchTranspose:
  /** Recursive switch-unit hierarchy swaps all temporal and lane address bits. */
  def reference[T](input: Vector[Vector[T]]): Vector[Vector[T]] =
    require(input.nonEmpty && input.forall(_.size == input.head.size), "switch transpose requires a rectangular stream with uniform lanes")
    Vector.tabulate(input.head.size)(cycle => Vector.tabulate(input.size)(lane => input(lane)(cycle)))
