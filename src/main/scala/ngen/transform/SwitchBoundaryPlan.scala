package ngen.transform

object SwitchBoundaryPlan:
  def apply(plan: StreamingNttPlan, dimension: Int): StreamingNttPlan =
    require(dimension > 1 && dimension * dimension == plan.domain.size, "switch boundary requires a square lane/time stream")
    def transpose(index: Int): Int = (index % dimension) * dimension + index / dimension
    val inputAddresses = Vector.tabulate(plan.domain.size)(index => plan.inputAddresses(transpose(index)))
    val inputFactors = Vector.tabulate(plan.domain.size)(index => plan.inputFactors(transpose(index)))
    val outputAddresses = Vector.tabulate(plan.domain.size)(index => plan.outputAddresses(transpose(index)))
    val outputFactors = Vector.tabulate(plan.domain.size)(index => plan.outputFactors(transpose(index)))
    plan match
      case complete: NttPlan => complete.copy(inputAddresses = inputAddresses, inputFactors = inputFactors, outputAddresses = outputAddresses, outputFactors = outputFactors)
      case incomplete: IncompleteNttPlan => incomplete.copy(inputAddresses = inputAddresses, inputFactors = inputFactors, outputAddresses = outputAddresses, outputFactors = outputFactors)
      case other => throw new IllegalArgumentException(s"unsupported switch-boundary plan ${other.getClass.getSimpleName}")
