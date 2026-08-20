package ngen.transform

import ngen.rtl.{FullThroughputPlan, StreamingCompose, StreamingKernel, StreamingShape}

/** Executable recursive-stage lowering shared by full-throughput RTL backends. */
object FullThroughputNttPlan:
  def build(plan: NttPlan, streamingWidth: Int, stageLatency: Int = 1): FullThroughputPlan =
    require(stageLatency > 0)
    val field = plan.domain.modulus
    val input = StreamingKernel("input-layout", plan.domain.size, 0, values =>
      val work = Array.fill(plan.domain.size)(BigInt(0))
      values.indices.foreach(index => work(plan.inputAddresses(index)) = field.multiply(values(index), plan.inputFactors(index)))
      work.toVector
    )
    val stages = plan.stages.map { stage =>
      StreamingKernel(s"radix2-stage-${stage.stage}", plan.domain.size, stageLatency, values =>
        val next = values.toArray
        stage.butterflies.foreach { butterfly =>
          val left = values(butterfly.left)
          val right = values(butterfly.right)
          butterfly.kind match
            case ButterflyKind.DecimationInTime =>
              val product = field.multiply(right, butterfly.twiddle)
              next(butterfly.left) = field.add(left, product)
              next(butterfly.right) = field.subtract(left, product)
            case ButterflyKind.GentlemanSande =>
              next(butterfly.left) = field.add(left, right)
              next(butterfly.right) = field.multiply(field.subtract(right, left), butterfly.twiddle)
        }
        next.toVector
      )
    }
    val output = StreamingKernel("output-layout", plan.domain.size, 0, values =>
      Vector.tabulate(plan.domain.size)(index => field.multiply(values(plan.outputAddresses(index)), plan.outputFactors(index)))
    )
    FullThroughputPlan(StreamingShape(plan.domain.size, streamingWidth), StreamingCompose(input +: stages :+ output))
