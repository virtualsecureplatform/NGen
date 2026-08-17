package ngen.rtl

final case class SwitchTransposeSpec(logSize: Int, dataWidth: Int):
  require(logSize > 0, s"transpose log size must be positive, got $logSize")
  require(dataWidth > 0, s"data width must be positive, got $dataWidth")
  val size: Int = 1 << logSize
  val halfCycle: Int = size / 2
  val latency: Int = size - 1

object SwitchTranspose:
  /** Recursive switch-unit hierarchy swaps all temporal and lane address bits. */
  def reference[T](input: Vector[Vector[T]]): Vector[Vector[T]] =
    require(input.nonEmpty && input.forall(_.size == input.size), "switch transpose requires a square stream")
    Vector.tabulate(input.size)(cycle => Vector.tabulate(input.size)(lane => input(lane)(cycle)))
