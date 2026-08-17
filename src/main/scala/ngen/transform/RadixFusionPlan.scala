package ngen.transform

final case class LinearTerm(index: Int, coefficient: BigInt)
final case class FusedAssignment(output: Int, terms: Vector[LinearTerm])
final case class FusedButterflyBlock(indices: Vector[Int], assignments: Vector[FusedAssignment])
final case class FusedNttStage(firstRadix2Stage: Int, radixLog: Int, blocks: Vector[FusedButterflyBlock])

/** Algebraically fuses adjacent radix-2 stages into radix-2^r blocks. */
final case class RadixFusionPlan(base: NttPlan, radixLog: Int, stages: Vector[FusedNttStage]):
  require(radixLog > 0)
  val fusedBlocks: Int = stages.map(_.blocks.size).sum
  val constantMultiplications: Int = stages.flatMap(_.blocks).flatMap(_.assignments).map(_.terms.count(_.coefficient != 1)).sum

  def evaluate(input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == base.domain.size)
    val field = base.domain.modulus
    var work = Array.fill(base.domain.size)(BigInt(0))
    input.indices.foreach(index => work(base.inputAddresses(index)) = field.multiply(input(index), base.inputFactors(index)))
    stages.foreach { stage =>
      val previous = work
      val next = previous.clone()
      stage.blocks.foreach(_.assignments.foreach { assignment =>
        next(assignment.output) = assignment.terms.foldLeft(BigInt(0)) { (sum, term) =>
          field.add(sum, field.multiply(previous(term.index), term.coefficient))
        }
      })
      work = next
    }
    Vector.tabulate(base.domain.size)(index => field.multiply(work(base.outputAddresses(index)), base.outputFactors(index)))

object RadixFusionPlan:
  private type Expression = Map[Int, BigInt]

  def apply(base: NttPlan, radixLog: Int): RadixFusionPlan =
    require(radixLog > 0 && radixLog <= base.domain.logSize)
    val field = base.domain.modulus
    def combine(lhs: Expression, rhs: Expression, rhsFactor: BigInt): Expression =
      (lhs.keySet ++ rhs.keySet).flatMap { index =>
        val coefficient = field.add(lhs.getOrElse(index, BigInt(0)), field.multiply(rhs.getOrElse(index, BigInt(0)), rhsFactor))
        Option.when(coefficient != 0)(index -> coefficient)
      }.toMap

    val stages = (0 until base.domain.logSize by radixLog).map { firstStage =>
      val effectiveRadixLog = math.min(radixLog, base.domain.logSize - firstStage)
      val radix = 1 << effectiveRadixLog
      val stride = 1 << firstStage
      val span = radix * stride
      val selectedStages = base.stages.slice(firstStage, firstStage + effectiveRadixLog)
      val blocks = (for
        blockBase <- 0 until base.domain.size by span
        offset <- 0 until stride
      yield
        val indices = Vector.tabulate(radix)(lane => blockBase + offset + lane * stride)
        val indexSet = indices.toSet
        var expressions = indices.map(index => index -> Map(index -> BigInt(1))).toMap
        selectedStages.foreach(_.butterflies.foreach { butterfly =>
          if indexSet(butterfly.left) then
            require(indexSet(butterfly.right) && butterfly.kind == ButterflyKind.DecimationInTime)
            val left = expressions(butterfly.left)
            val right = expressions(butterfly.right)
            expressions = expressions.updated(butterfly.left, combine(left, right, butterfly.twiddle))
            expressions = expressions.updated(butterfly.right, combine(left, right, field.subtract(0, butterfly.twiddle)))
        })
        val assignments = indices.map { output =>
          FusedAssignment(output, expressions(output).toVector.sortBy(_._1).map(LinearTerm.apply))
        }
        FusedButterflyBlock(indices, assignments)
      ).toVector
      FusedNttStage(firstStage, effectiveRadixLog, blocks)
    }.toVector
    RadixFusionPlan(base, radixLog, stages)
