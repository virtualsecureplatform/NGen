package ngen.transform

object SwitchBoundaryPlan:
  def apply(plan: StreamingNttPlan, dimension: Int): StreamingNttPlan =
    val size = plan.domain.size
    require(dimension > 1 && Integer.bitCount(dimension) == 1 && size % dimension == 0,
      "switch boundary requires a power-of-two lane width dividing the transform size")
    val cycles = size / dimension
    // The rate-preserving adapter transposes a cycles-by-lanes frame and then
    // repacks it into the original lane width. Its output word j came from
    // input word inputIndex(j). A rectangular transpose is not an involution.
    def inputIndex(index: Int): Int = (index % cycles) * dimension + index / cycles
    def outputIndex(index: Int): Int = (index % dimension) * cycles + index / dimension
    val inputAddresses = Vector.tabulate(size)(index => plan.inputAddresses(inputIndex(index)))
    val inputFactors = Vector.tabulate(size)(index => plan.inputFactors(inputIndex(index)))
    val outputAddresses = Vector.tabulate(size)(index => plan.outputAddresses(outputIndex(index)))
    val outputFactors = Vector.tabulate(size)(index => plan.outputFactors(outputIndex(index)))
    plan match
      case complete: NttPlan => complete.copy(inputAddresses = inputAddresses, inputFactors = inputFactors, outputAddresses = outputAddresses, outputFactors = outputFactors)
      case incomplete: IncompleteNttPlan => incomplete.copy(inputAddresses = inputAddresses, inputFactors = inputFactors, outputAddresses = outputAddresses, outputFactors = outputFactors)
      case other => throw new IllegalArgumentException(s"unsupported switch-boundary plan ${other.getClass.getSimpleName}")
