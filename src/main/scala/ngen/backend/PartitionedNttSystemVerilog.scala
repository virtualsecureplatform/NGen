package ngen.backend

import ngen.transform.{NttPlan, DataOrder}
import ngen.rtl.{PeNttSchedule, ProfileName, ReductionKind, StreamProtocol}

/** A pipeline of independently buffered PE engines, each reusing PEs across a
  * contiguous subset of stages. It is deliberately distinct from an MDC/SDF
  * pipeline: full-frame memories and handshakes are part of its measured cost.
  */
object PartitionedNttSystemVerilog:
  def partitions(plan: NttPlan, groups: Int): Vector[NttPlan] =
    require(groups >= 1 && groups <= plan.stages.size, "stage groups must be within 1..stage count")
    val identity = Vector.tabulate(plan.domain.size)(i => i)
    val ones = Vector.fill(plan.domain.size)(BigInt(1))
    Vector.tabulate(groups) { group =>
      val begin = group * plan.stages.size / groups
      val end = (group + 1) * plan.stages.size / groups
      plan.copy(
        inputOrder = if group == 0 then plan.inputOrder else DataOrder.Natural,
        outputOrder = if group == groups - 1 then plan.outputOrder else DataOrder.Natural,
        inputAddresses = if group == 0 then plan.inputAddresses else identity,
        inputFactors = if group == 0 then plan.inputFactors else ones,
        stages = plan.stages.slice(begin,end).zipWithIndex.map((stage,index) => stage.copy(stage=index, butterflies=stage.butterflies.map(_.copy(stage=index)))),
        outputAddresses = if group == groups - 1 then plan.outputAddresses else identity,
        outputFactors = if group == groups - 1 then plan.outputFactors else ones
      )
    }

  def emit(plan: NttPlan, lanes: Int, peCount: Int, groups: Int, top: String, profile: ProfileName, reduction: ReductionKind): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val width=plan.domain.modulus.bitWidth
    val plans=partitions(plan,groups)
    val definitions=plans.zipWithIndex.map { (part,index) =>
      val name=s"${top}Group$index"
      PeStreamingNttSystemVerilog.emit(PeNttSchedule.build(part,1,peCount,lanes),lanes,name,profile,reduction,StreamProtocol.ReadyValid)
        .replace("NGenInternalPipelinedButterfly",s"${name}Butterfly")
    }.mkString("\n")
    val ports=Vector.tabulate(lanes)(lane=>s"input [${width-1}:0] i$lane,output [${width-1}:0] o$lane").mkString(",\n")
    val links=(1 until groups).map { link =>
      s"wire valid_$link,ready_$link;"+Vector.tabulate(lanes)(lane=>s"wire [${width-1}:0] data_${link}_$lane;").mkString
    }.mkString("\n")
    val instances=plans.indices.map { index =>
      val vin=if index==0 then "in_valid" else s"valid_$index"
      val rin=if index==0 then "in_ready" else s"ready_$index"
      val vout=if index==groups-1 then "out_valid" else s"valid_${index+1}"
      val rout=if index==groups-1 then "out_ready" else s"ready_${index+1}"
      val dataPorts=Vector.tabulate(lanes) { lane =>
        val in=if index==0 then s"i$lane" else s"data_${index}_$lane"
        val out=if index==groups-1 then s"o$lane" else s"data_${index+1}_$lane"
        s".i$lane($in),.o$lane($out)"
      }.mkString(",")
      s"${top}Group$index group_$index(.clock(clock),.reset(reset),.in_valid($vin),.in_ready($rin),.out_valid($vout),.out_ready($rout),$dataPorts);"
    }.mkString("\n")
    s"""$definitions
       |module $top(input clock,input reset,input in_valid,output in_ready,output out_valid,input out_ready,
       |$ports);
       |$links
       |$instances
       |endmodule
       |""".stripMargin
