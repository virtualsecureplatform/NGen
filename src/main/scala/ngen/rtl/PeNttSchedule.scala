package ngen.rtl

import ngen.transform.{ButterflyKind, IncompleteNttPlan, LinearTerm, NttPlan, RadixFusionPlan, StreamingNttPlan}

final case class PeTerm(inputSlot: Int, coefficient: BigInt)
final case class PeAssignment(address: Int, terms: Vector[PeTerm])
final case class PeButterflyStep(leftSlot: Int, rightSlot: Int, twiddle: BigInt, kind: PeOperationKind)
enum PeOperationKind:
  case Scale, DecimationInTime, GentlemanSande, Dense
final case class PeOperation(kind: PeOperationKind, inputs: Vector[Int], outputs: Vector[PeAssignment], steps: Vector[PeButterflyStep] = Vector.empty, postFactors: Vector[BigInt] = Vector.empty)
final case class PeBundle(stage: Int, operations: Vector[PeOperation])

final case class BankMapping(bankCount: Int, addressToBank: Vector[Int], addressToRow: Vector[Int]):
  require(bankCount > 0 && Integer.bitCount(bankCount) == 1)
  require(addressToBank.size == addressToRow.size)
  val depth: Int = (addressToBank.size + bankCount - 1) / bankCount
  def bank(address: Int): Int = addressToBank(address)
  def row(address: Int): Int = addressToRow(address)

final case class PeNttSchedule(
    plan: StreamingNttPlan,
    radixLog: Int,
    peCount: Int,
    mapping: BankMapping,
    bundles: Vector[PeBundle]
):
  val radix: Int = 1 << radixLog

  def evaluate(input: Seq[BigInt]): Vector[BigInt] =
    require(input.size == plan.domain.size)
    val field = plan.domain.modulus
    var work = Array.fill(plan.domain.size)(BigInt(0))
    input.indices.foreach(index => work(plan.inputAddresses(index)) = field.normalize(input(index)))
    bundles.foreach { bundle =>
      val previous = work
      val next = previous.clone()
      bundle.operations.foreach { operation =>
        operation.outputs.foreach { assignment =>
          val value = assignment.terms.foldLeft(BigInt(0)) { (sum, term) =>
            field.add(sum, field.multiply(previous(operation.inputs(term.inputSlot)), term.coefficient))
          }
          next(assignment.address) = field.multiply(value,operation.postFactors.lift(operation.outputs.indexOf(assignment)).getOrElse(BigInt(1)))
        }
      }
      work = next
    }
    Vector.tabulate(plan.domain.size)(index => work(plan.outputAddresses(index)))

object PeNttSchedule:
  private def nextPowerOfTwo(value: Int): Int =
    if value <= 1 then 1 else Integer.highestOneBit(value - 1) << 1

  def bankMapping(size: Int, requestedBanks: Int): BankMapping =
    require(size > 1 && Integer.bitCount(size) == 1)
    val bankCount = math.min(size, nextPowerOfTwo(math.max(2, requestedBanks)))
    val bankBits = Integer.numberOfTrailingZeros(bankCount)
    def foldedBank(address: Int): Int =
      var result = 0
      var bit = 0
      while bit < Integer.numberOfTrailingZeros(size) do
        if ((address & (1 << bit)) != 0) result ^= 1 << (bit % bankBits)
        bit += 1
      result
    val banks = Vector.tabulate(size)(foldedBank)
    val nextRow = Array.fill(bankCount)(0)
    val rows = banks.map { bank =>
      val row = nextRow(bank)
      nextRow(bank) += 1
      row
    }
    BankMapping(bankCount, banks, rows)

  def build(plan: StreamingNttPlan, radixLog: Int, requestedPeCount: Int, streamingWidth: Int = 1): PeNttSchedule =
    require(radixLog > 0 && radixLog <= plan.domain.logSize)
    require(requestedPeCount > 0)
    require(streamingWidth > 0 && Integer.bitCount(streamingWidth) == 1 && streamingWidth <= plan.domain.size)
    require(radixLog == 1 || plan.isInstanceOf[NttPlan], "incomplete transforms currently support radix 2 only")
    val field = plan.domain.modulus
    val radix = 1 << radixLog
    val peCount = math.min(requestedPeCount, math.max(1, plan.domain.size / radix))
    val mapping = bankMapping(plan.domain.size, math.max(radix * peCount, streamingWidth))
    val streamCycles = plan.domain.size / streamingWidth
    for cycle <- 0 until streamCycles do
      val inputBanks = Vector.tabulate(streamingWidth)(lane => mapping.bank(plan.inputAddresses(cycle * streamingWidth + lane)))
      val outputBanks = Vector.tabulate(streamingWidth)(lane => mapping.bank(plan.outputAddresses(cycle * streamingWidth + lane)))
      require(inputBanks.distinct.size == inputBanks.size, s"input cycle $cycle has a bank conflict")
      require(outputBanks.distinct.size == outputBanks.size, s"output cycle $cycle has a bank conflict")

    def scaleOperation(address: Int, factor: BigInt): PeOperation =
      PeOperation(PeOperationKind.Scale, Vector(address), Vector(PeAssignment(address, Vector(PeTerm(0, field.normalize(factor))))))

    val inputScale = plan.inputFactors.zipWithIndex.collect {
      case (factor, streamIndex) if field.normalize(factor) != 1 => scaleOperation(plan.inputAddresses(streamIndex), factor)
    }

    val transformStages: Vector[Vector[PeOperation]] = plan match
      case complete: NttPlan =>
        RadixFusionPlan(complete, radixLog).stages.map { stage =>
          stage.blocks.map { block =>
            val slots = block.indices.zipWithIndex.toMap
            val steps = complete.stages.slice(stage.firstRadix2Stage, stage.firstRadix2Stage + stage.radixLog).flatMap(_.butterflies).collect {
              case butterfly if slots.contains(butterfly.left) =>
                PeButterflyStep(slots(butterfly.left), slots(butterfly.right), butterfly.twiddle, PeOperationKind.DecimationInTime)
            }
            PeOperation(if radixLog == 1 then PeOperationKind.DecimationInTime else PeOperationKind.Dense, block.indices, block.assignments.map { assignment =>
              PeAssignment(assignment.output, assignment.terms.map(term => PeTerm(slots(term.index), term.coefficient)))
            }, steps)
          }
        }
      case incomplete: IncompleteNttPlan =>
        incomplete.stages.map(_.butterflies.map { butterfly =>
          val negativeTwiddle = field.subtract(0, butterfly.twiddle)
          val outputs = butterfly.kind match
            case ButterflyKind.DecimationInTime => Vector(
              PeAssignment(butterfly.left, Vector(PeTerm(0, 1), PeTerm(1, butterfly.twiddle))),
              PeAssignment(butterfly.right, Vector(PeTerm(0, 1), PeTerm(1, negativeTwiddle)))
            )
            case ButterflyKind.GentlemanSande => Vector(
              PeAssignment(butterfly.left, Vector(PeTerm(0, 1), PeTerm(1, 1))),
              PeAssignment(butterfly.right, Vector(PeTerm(0, negativeTwiddle), PeTerm(1, butterfly.twiddle)))
            )
          PeOperation(
            if butterfly.kind == ButterflyKind.DecimationInTime then PeOperationKind.DecimationInTime else PeOperationKind.GentlemanSande,
            Vector(butterfly.left, butterfly.right), outputs
          )
        })
      case other => throw new IllegalArgumentException(s"unsupported plan ${other.getClass.getSimpleName}")

    val outputScale = plan.outputFactors.zipWithIndex.collect {
      case (factor, streamIndex) if field.normalize(factor) != 1 => scaleOperation(plan.outputAddresses(streamIndex), factor)
    }

    def packStage(stage: Int, operations: Vector[PeOperation]): Vector[PeBundle] =
      val pending = scala.collection.mutable.ArrayBuffer.from(operations)
      val result = scala.collection.mutable.ArrayBuffer.empty[PeBundle]
      while pending.nonEmpty do
        var usedBanks = Set.empty[Int]
        val selected = scala.collection.mutable.ArrayBuffer.empty[PeOperation]
        var index = 0
        while index < pending.size && selected.size < peCount do
          val operation = pending(index)
          val banks = operation.inputs.map(mapping.bank)
          require(banks.distinct.size == banks.size, s"radix-$radix operation has an internal bank conflict: ${operation.inputs}")
          if banks.forall(!usedBanks(_)) then
            selected += operation
            usedBanks ++= banks
            pending.remove(index)
          else index += 1
        require(selected.nonEmpty, "bank scheduler could not make progress")
        result += PeBundle(stage, selected.toVector)
      result.toVector

    val factorByAddress=plan.outputAddresses.zip(plan.outputFactors).toMap
    val foldedTransformStages = if radixLog>1 && transformStages.nonEmpty then
      transformStages.updated(transformStages.size-1,transformStages.last.map(operation=>operation.copy(postFactors=operation.outputs.map(output=>factorByAddress.getOrElse(output.address,BigInt(1))))))
    else transformStages
    val remainingOutputScale=if radixLog>1 then Vector.empty else outputScale
    val stageOperations = (if inputScale.nonEmpty then Vector(inputScale) else Vector.empty) ++ foldedTransformStages ++
      (if remainingOutputScale.nonEmpty then Vector(remainingOutputScale) else Vector.empty)
    PeNttSchedule(plan, radixLog, peCount, mapping, stageOperations.zipWithIndex.flatMap((operations,stage) => packStage(stage,operations)))
