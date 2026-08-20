package ngen.backend

import ngen.arithmetic.{BarrettField, ShoupField}
import ngen.rtl.{ProfileName, ReductionKind}
import ngen.transform.{ButterflyKind, PlannedButterfly, StreamingNttPlan}

/**
  * Generic stage-parallel lowering for complete cyclic/negacyclic plans.
  *
  * Every transform stage gets one registered boundary.  All butterflies in
  * that stage are emitted into the same combinational stage function, so the
  * architecture is reusable across arbitrary NTT-friendly primes while the
  * scheduler remains independent of a particular preset.
  */
object StageParallelNttSystemVerilog:
  def stageCount(plan: StreamingNttPlan): Int = plan.stages.size

  def emit(
      plan: StreamingNttPlan,
      streamingWidth: Int,
      top: String = "main",
      profile: ProfileName = ProfileName.Baseline,
      reduction: ReductionKind = ReductionKind.Barrett
  ): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    require(streamingWidth > 0 && Integer.bitCount(streamingWidth) == 1 && streamingWidth <= plan.domain.size)
    require(plan.domain.size % streamingWidth == 0)
    require(Set(ReductionKind.Barrett, ReductionKind.Montgomery, ReductionKind.Shoup)(reduction),
      "generic stage-parallel RTL supports Barrett, Montgomery, or Shoup reduction")
    val domain = plan.domain
    val field = domain.modulus
    val width = field.bitWidth
    val size = domain.size
    val inputCycles = size / streamingWidth
    val stageGap = if profile == ProfileName.F300 then 1 else 0
    val barrett = BarrettField(field)
    val shoup = ShoupField(field)
    val wordRadix = BigInt(1) << width
    val montgomeryQInv = (-field.q.modInverse(wordRadix)).mod(wordRadix)
    val montgomeryR2 = field.multiply(wordRadix, wordRadix)

    def normalized(value: BigInt): BigInt = field.normalize(value)
    def encodedConstant(value: BigInt): BigInt =
      if reduction == ReductionKind.Montgomery then field.multiply(normalized(value), wordRadix) else normalized(value)
    def literal(value: BigInt): String = s"${width}'d${encodedConstant(value)}"
    def precondition(value: BigInt): String = s"${width}'d${shoup.prepare(value).precondition}"
    def mul(value: String, factor: BigInt): String =
      val normalizedFactor = normalized(factor)
      reduction match
        case ReductionKind.Shoup => s"field_mul($value,${literal(normalizedFactor)},${precondition(normalizedFactor)})"
        case _ => s"field_mul($value,${literal(normalizedFactor)})"

    def montgomeryRawMul(value: String, rawFactor: BigInt): String =
      s"field_mul($value,${width}'d${field.normalize(rawFactor)})"

    def inputValue(value: String, factor: BigInt): String =
      reduction match
        case ReductionKind.Montgomery => montgomeryRawMul(value, field.multiply(montgomeryR2, factor))
        case _ if normalized(factor) == 1 => value
        case _ => mul(value, factor)

    def outputValue(value: String, factor: BigInt): String =
      reduction match
        case ReductionKind.Montgomery => montgomeryRawMul(mul(value, factor), BigInt(1))
        case _ if normalized(factor) == 1 => value
        case _ => mul(value, factor)

    def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")
    def stageLines(stage: Vector[PlannedButterfly]): Vector[String] =
      val result = scala.collection.mutable.ArrayBuffer.empty[String]
      stage.foreach { butterfly =>
        val left = butterfly.left
        val right = butterfly.right
        butterfly.kind match
          case ButterflyKind.DecimationInTime =>
            result += s"tmp = stage_next[$left];"
            result += s"tmp2 = ${mul(s"stage_next[$right]", butterfly.twiddle)};"
            result += s"stage_next[$left] = mod_add(tmp,tmp2);"
            result += s"stage_next[$right] = mod_sub(tmp,tmp2);"
          case ButterflyKind.GentlemanSande =>
            result += s"tmp = stage_next[$left];"
            result += s"tmp2 = stage_next[$right];"
            result += s"stage_next[$left] = mod_add(tmp,tmp2);"
            result += s"stage_next[$right] = ${mul("mod_sub(tmp2,tmp)", butterfly.twiddle)};"
      }
      result.toVector

    val stageCases = plan.stages.map(stage => s"begin\n${lines(stageLines(stage.butterflies),10)}\n      end").zipWithIndex.map { case (body,index) => s"$index: $body" }.mkString("\n")
    val inputPorts = Vector.tabulate(streamingWidth)(lane => s"input [${width - 1}:0] i$lane")
    val outputPorts = Vector.tabulate(streamingWidth)(lane => s"output reg [${width - 1}:0] o$lane")
    val ports = Vector("input clock", "input reset", "input next", "output ready", "output reg next_out") ++ inputPorts ++ outputPorts
    def captureAssignments(cycle: Int): Vector[String] = Vector.tabulate(streamingWidth) { lane =>
      val index = cycle * streamingWidth + lane
      val address = plan.inputAddresses(index)
      s"work[$address] <= ${inputValue(s"i$lane", plan.inputFactors(index))};"
    }
    def outputAssignments(cycle: Int): Vector[String] = Vector.tabulate(streamingWidth) { lane =>
      val index = cycle * streamingWidth + lane
      val address = plan.outputAddresses(index)
      s"o$lane <= ${outputValue(s"work[$address]", plan.outputFactors(index))};"
    }
    val firstCapture = captureAssignments(0).mkString(" ")
    val captureCases = (1 until inputCycles).map(cycle => s"$cycle: begin ${captureAssignments(cycle).mkString(" ")} end").mkString("\n          ")
    val outputCases = (0 until inputCycles).map(cycle => s"$cycle: begin ${outputAssignments(cycle).mkString(" ")} end").mkString("\n          ")
    val resetOutputs = Vector.tabulate(streamingWidth)(lane => s"o$lane <= '0;").mkString(" ")
    val afterCapture = if plan.stages.nonEmpty then "state <= EXECUTE; stage_index <= 0;" else "state <= OUTPUT_DATA; output_count <= 0;"
    val afterFirstCapture = if inputCycles == 1 then afterCapture else "state <= CAPTURE; capture_count <= 1;"
    val reductionParameters = reduction match
      case ReductionKind.Barrett => s"  localparam [${2 * width - 1}:0] BARRETT_MU=${2 * width}'d${barrett.mu};"
      case ReductionKind.Montgomery => s"  localparam [${width - 1}:0] MONTGOMERY_QINV=${width}'d$montgomeryQInv; localparam [${width - 1}:0] MONTGOMERY_R2=${width}'d${montgomeryR2};"
      case ReductionKind.Shoup => "  // Shoup reciprocals are emitted beside each fixed constant."
      case _ => ""
    val multiplyFunction = reduction match
      case ReductionKind.Barrett =>
        s"""  function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b);
           |    reg [${2 * width - 1}:0] product; reg [${4 * width - 1}:0] scaled; reg [${2 * width - 1}:0] quotient; reg [${3 * width - 1}:0] quotient_product; reg signed [${3 * width}:0] remainder;
           |    begin product={{$width{1'b0}},a}*{{$width{1'b0}},b}; scaled={{${2 * width}{1'b0}},product}*{{${2 * width}{1'b0}},BARRETT_MU}; quotient=scaled[${4 * width - 1}:${2 * width}]; quotient_product={{$width{1'b0}},quotient}*{{${2 * width}{1'b0}},MODULUS}; remainder=$$signed({${width + 1}'d0,product})-$$signed({1'b0,quotient_product}); if(remainder<0)remainder=remainder+MODULUS_REMAINDER; if(remainder>=MODULUS_REMAINDER)remainder=remainder-MODULUS_REMAINDER; if(remainder>=MODULUS_REMAINDER)remainder=remainder-MODULUS_REMAINDER; field_mul=remainder[${width - 1}:0]; end
           |  endfunction""".stripMargin
      case ReductionKind.Montgomery =>
        s"""  function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b_montgomery);
           |    reg [${2 * width - 1}:0] product,correction_product,multiple; reg [${width - 1}:0] correction; reg [${2 * width}:0] sum; reg [$width:0] reduced;
           |    begin product={{$width{1'b0}},a}*{{$width{1'b0}},b_montgomery}; correction_product={{$width{1'b0}},product[${width - 1}:0]}*{{$width{1'b0}},MONTGOMERY_QINV}; correction=correction_product[${width - 1}:0]; multiple={{$width{1'b0}},correction}*{{$width{1'b0}},MODULUS}; sum={1'b0,product}+{1'b0,multiple}; reduced=sum[${2 * width}:$width]; if(reduced>={1'b0,MODULUS})reduced=reduced-{1'b0,MODULUS}; field_mul=reduced[${width - 1}:0]; end
           |  endfunction""".stripMargin
      case ReductionKind.Shoup =>
        s"""  function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b,input [${width - 1}:0] b_shoup);
           |    reg [${2 * width - 1}:0] product,approximate_product,quotient_product; reg [${width - 1}:0] approximate_quotient; reg [${2 * width}:0] remainder;
           |    begin product={{$width{1'b0}},a}*{{$width{1'b0}},b}; approximate_product={{$width{1'b0}},a}*{{$width{1'b0}},b_shoup}; approximate_quotient=approximate_product[${2 * width - 1}:$width]; quotient_product={{$width{1'b0}},approximate_quotient}*{{$width{1'b0}},MODULUS}; remainder={1'b0,product}-{1'b0,quotient_product}; if(remainder>={{${width + 1}{1'b0}},MODULUS})remainder=remainder-{{${width + 1}{1'b0}},MODULUS}; field_mul=remainder[${width - 1}:0]; end
           |  endfunction""".stripMargin
      case _ => throw new IllegalArgumentException("unsupported stage-parallel reduction")
    s"""// Generated by NGen's generic stage-parallel NTT backend.
       |/* verilator lint_off DECLFILENAME */
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer N=$size; localparam integer STREAMING_WIDTH=$streamingWidth; localparam integer INPUT_CYCLES=$inputCycles; localparam integer OUTPUT_CYCLES=$inputCycles; localparam integer STAGE_COUNT=${plan.stages.size}; localparam integer STAGE_GAP=$stageGap;
       |  localparam [${width - 1}:0] MODULUS=${width}'d${field.q}; localparam signed [${3 * width}:0] MODULUS_REMAINDER=${3 * width + 1}'sd${field.q}; localparam [${width}:0] MODULUS_EXT=${width + 1}'d${field.q};
       |$reductionParameters
       |  function automatic [${width - 1}:0] mod_add(input [${width - 1}:0] a,input [${width - 1}:0] b); reg [$width:0] sum; begin sum={1'b0,a}+{1'b0,b}; if(sum>=MODULUS_EXT)sum=sum-MODULUS_EXT; mod_add=sum[${width - 1}:0]; end endfunction
       |  function automatic [${width - 1}:0] mod_sub(input [${width - 1}:0] a,input [${width - 1}:0] b); reg [$width:0] difference; begin if(a>=b)difference={1'b0,a}-{1'b0,b};else difference={1'b0,a}+MODULUS_EXT-{1'b0,b}; mod_sub=difference[${width - 1}:0]; end endfunction
       |$multiplyFunction
       |  reg [${width - 1}:0] work [0:N-1]; reg [${width - 1}:0] stage_next [0:N-1]; reg [${width - 1}:0] tmp,tmp2; integer j;
       |  integer capture_count,stage_index,output_count,gap_count; localparam [1:0] IDLE=0,CAPTURE=1,EXECUTE=2,OUTPUT_DATA=3; reg [1:0] state;
       |  assign ready=(state==IDLE)||((state==OUTPUT_DATA)&&(output_count==OUTPUT_CYCLES-1));
       |  always @(*) begin for(j=0;j<N;j=j+1)stage_next[j]=work[j];tmp=0;tmp2=0;case(stage_index) $stageCases default:begin end endcase end
       |  always @(posedge clock) begin
       |    if(reset)begin state<=IDLE;capture_count<=0;stage_index<=0;output_count<=0;gap_count<=0;next_out<=0;$resetOutputs end else begin next_out<=0;
       |      case(state)
       |        IDLE: if(next)begin $firstCapture $afterFirstCapture end
       |        CAPTURE: begin case(capture_count) $captureCases default:begin end endcase if(capture_count==INPUT_CYCLES-1)begin $afterCapture end else capture_count<=capture_count+1; end
       |        EXECUTE: begin if(gap_count!=0)gap_count<=gap_count-1;else begin for(j=0;j<N;j=j+1)work[j]<=stage_next[j];if(stage_index==STAGE_COUNT-1)begin state<=OUTPUT_DATA;output_count<=0;end else begin stage_index<=stage_index+1;gap_count<=STAGE_GAP;end end end
       |        OUTPUT_DATA: begin case(output_count) $outputCases default:begin end endcase if(output_count==0)next_out<=1;if(output_count==OUTPUT_CYCLES-1)begin if(next)begin $firstCapture $afterFirstCapture end else state<=IDLE;end else output_count<=output_count+1; end
       |        default: state<=IDLE;
       |      endcase
       |    end
       |  end
       |endmodule
       |/* verilator lint_on DECLFILENAME */
       |""".stripMargin
