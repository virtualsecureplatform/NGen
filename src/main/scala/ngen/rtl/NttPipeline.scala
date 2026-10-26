package ngen.rtl

/** Concrete Montgomery arithmetic partitions, not arbitrary latency padding. */
final case class NttPipeline(
    multiplierStages: Option[Int] = None,
    reductionStages: Option[Int] = None,
    outputRegister: Boolean = false
):
  require(multiplierStages.forall(Set(1,3)), "multiplier pipeline stages must be 1 or 3")
  require(reductionStages.forall(Set(3,4,5)), "reduction pipeline stages must be 3, 4, or 5")
  def configured: Boolean = multiplierStages.nonEmpty || reductionStages.nonEmpty || outputRegister
  def multiplier(width: Int): Int = multiplierStages.getOrElse(if width > 32 then 3 else 1)
  def reduction: Int = reductionStages.getOrElse(4)
  def butterflyLatency(width: Int): Int = 2 + multiplier(width) + reduction
