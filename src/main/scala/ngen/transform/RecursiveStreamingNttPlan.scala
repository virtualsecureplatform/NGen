package ngen.transform

import ngen.rtl.{FullThroughputPlan, StreamingCompose, StreamingKernel, StreamingShape, TwiddleStoragePlan}

final case class RecursiveStreamingPlan(plan: FullThroughputPlan, radix: Int, levels: Int, twiddles: TwiddleStoragePlan)

/** General radix-2^r recursive grouping over the canonical executable plan.
  * A final short level is allowed when r does not divide log2(N).
  */
object RecursiveStreamingNttPlan:
  def build(plan: NttPlan, streamingWidth: Int, radixLog: Int, levelLatency: Int = 1): RecursiveStreamingPlan =
    require(radixLog > 0 && levelLatency > 0)
    val field = plan.domain.modulus
    val input = StreamingKernel("recursive-input-layout", plan.domain.size, 0, values =>
      val work=Array.fill(plan.domain.size)(BigInt(0))
      values.indices.foreach(i=>work(plan.inputAddresses(i))=field.multiply(values(i),plan.inputFactors(i)))
      work.toVector
    )
    val levels = plan.stages.grouped(radixLog).zipWithIndex.map { case (group, level) =>
      StreamingKernel(s"recursive-radix${1 << group.size}-level-$level",plan.domain.size,levelLatency,values=>
        group.foldLeft(values){case(current,stage)=>
          val next=current.toArray
          stage.butterflies.foreach{b=>
            val left=current(b.left);val right=current(b.right)
            b.kind match
              case ButterflyKind.DecimationInTime =>
                val product=field.multiply(right,b.twiddle);next(b.left)=field.add(left,product);next(b.right)=field.subtract(left,product)
              case ButterflyKind.GentlemanSande =>
                next(b.left)=field.add(left,right);next(b.right)=field.multiply(field.subtract(right,left),b.twiddle)
          }
          next.toVector
        }
      )
    }.toVector
    val output = StreamingKernel("recursive-output-layout",plan.domain.size,0,values=>
      Vector.tabulate(plan.domain.size)(i=>field.multiply(values(plan.outputAddresses(i)),plan.outputFactors(i)))
    )
    val root=StreamingCompose(input+:levels:+output)
    val full=FullThroughputPlan(StreamingShape(plan.domain.size,streamingWidth),root)
    val twiddleCount=plan.stages.flatMap(_.butterflies.map(_.twiddle)).distinct.size.max(1)
    RecursiveStreamingPlan(full,1 << radixLog,levels.size,TwiddleStoragePlan.choose(twiddleCount,plan.domain.modulus.bitWidth,streamingWidth/2 max 1))
