package ngen.backend

import ngen.arithmetic.ShoupField
import ngen.rtl.ReductionKind
import ngen.transform.{ButterflyKind, StreamingNttPlan}

/** Frame-elastic spatial pipeline. Every stage owns its complete coefficient vector. */
object OverlappedStageNttSystemVerilog:
  def emit(plan: StreamingNttPlan, lanes: Int, top: String, reduction: ReductionKind): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val field=plan.domain.modulus
    val n=plan.domain.size
    val w=field.bitWidth
    val stages=plan.stages.size
    require(stages>0 && lanes>0 && Integer.bitCount(lanes)==1 && n%lanes==0)
    require(Set(ReductionKind.Barrett,ReductionKind.Montgomery,ReductionKind.Shoup)(reduction))
    val beats=n/lanes
    val countWidth=math.max(1,Integer.numberOfTrailingZeros(beats))
    val radix=BigInt(1)<<w
    val shoup=ShoupField(field)
    def literal(value: BigInt): String = s"${w}'d${field.normalize(value)}"
    def encoded(value: BigInt): BigInt = if reduction==ReductionKind.Montgomery then field.multiply(value,radix) else value
    def precon(value: BigInt): String = if reduction==ReductionKind.Shoup then s"${w}'d${shoup.prepare(value).precondition}" else literal(0)
    def select(values: Seq[String],index: String): String =
      values.indices.drop(1).reverse.foldLeft(values.head)((rest,i)=>s"($index==$i ? ${values(i)} : $rest)")
    val ports=(0 until lanes).map(i=>s"input [${w-1}:0] i$i,output [${w-1}:0] o$i").mkString(",")
    val capture=(0 until lanes).flatMap { lane =>
      val factors=(0 until beats).map(b=>plan.inputFactors(b*lanes+lane))
      val constants=factors.map(f=>literal(if reduction==ReductionKind.Montgomery then field.multiply(radix*radix,f) else f))
      Seq(s"wire [${w-1}:0] capture_$lane;",
        s"${top}_FieldMul capture_mul_$lane(.a(i$lane),.b(${select(constants,"capture_beat")}),.b_shoup(${select(factors.map(precon),"capture_beat")}),.result(capture_$lane));")
    }.mkString("\n")
    val captureCases=(0 until beats).map { b =>
      val assignments=(0 until lanes).map(l=>s"captured[${plan.inputAddresses(b*lanes+l)}]<=capture_$l;").mkString
      s"$b:begin $assignments end"
    }.mkString("\n")
    val network=plan.stages.zipWithIndex.flatMap { (stage,s) =>
      val covered=stage.butterflies.flatMap(b=>Vector(b.left,b.right)).sorted
      require(covered==(0 until n).toVector,"each stage must cover every coefficient exactly once")
      stage.butterflies.zipWithIndex.map { (b,i) =>
        def input(index: Int): String = if s==0 then s"captured[$index]" else s"stage_${s-1}[$index]"
        val gs=if b.kind==ButterflyKind.GentlemanSande then "1'b1" else "1'b0"
        s"${top}_Butterfly #(.GS($gs)) butterfly_${s}_$i(.left(${input(b.left)}),.right(${input(b.right)}),.twiddle(${literal(encoded(b.twiddle))}),.twiddle_shoup(${precon(b.twiddle)}),.left_out(next_$s[${b.left}]),.right_out(next_$s[${b.right}]));"
      }
    }.mkString("\n")
    val storage=(0 until stages).map(s=>s"reg [${w-1}:0] stage_$s[0:${n-1}];wire [${w-1}:0] next_$s[0:${n-1}];").mkString("\n")
    val control=(0 until stages).map { s =>
      val readyNext=if s==stages-1 then s"(out_ready && output_beat==${beats-1})" else s"stage_ready[${s+1}]"
      s"assign stage_ready[$s]=!stage_valid[$s] || $readyNext;"
    }.mkString("\n")
    val advance=(0 until stages).map { s =>
      val valid=if s==0 then "capture_full" else s"stage_valid[${s-1}]"
      s"if(stage_ready[$s])begin stage_valid[$s]<=$valid;if($valid)begin for(j=0;j<N;j=j+1)stage_$s[j]<=next_$s[j];end end"
    }.mkString("\n")
    val outputs=(0 until lanes).flatMap { l =>
      val factors=(0 until beats).map(b=>plan.outputFactors(b*lanes+l))
      val words=(0 until beats).map(b=>s"stage_${stages-1}[${plan.outputAddresses(b*lanes+l)}]")
      Seq(s"wire [${w-1}:0] scaled_$l;",
        s"${top}_FieldMul output_mul_$l(.a(${select(words,"output_beat")}),.b(${select(factors.map(f=>literal(encoded(f))),"output_beat")}),.b_shoup(${select(factors.map(precon),"output_beat")}),.result(scaled_$l));",
        if reduction==ReductionKind.Montgomery then
          s"${top}_FieldMul output_convert_$l(.a(scaled_$l),.b(${literal(1)}),.b_shoup(${literal(0)}),.result(o$l));"
        else s"assign o$l=scaled_$l;")
    }.mkString("\n")
    s"""// Frame-overlapped stage-parallel NTT: independent frame registers and backpressure.
       |module $top(input clock,reset,in_valid,output in_ready,out_valid,input out_ready,$ports);
       |localparam N=$n;
       |reg [${w-1}:0] captured[0:${n-1}];reg capture_full;
       |reg [${countWidth-1}:0] capture_beat,output_beat;
       |reg [${stages-1}:0] stage_valid;wire [${stages-1}:0] stage_ready;integer j;
       |$storage
       |assign in_ready=!capture_full || stage_ready[0];
       |assign out_valid=stage_valid[${stages-1}];
       |$capture
       |$network
       |$control
       |$outputs
       |always @(posedge clock)begin
       |if(reset)begin capture_full<=0;capture_beat<=0;output_beat<=0;stage_valid<=0;end
       |else begin
       |if(capture_full && stage_ready[0])capture_full<=0;
       |if(in_valid && in_ready)begin
       |case(capture_beat) $captureCases endcase
       |if(capture_beat==${beats-1})begin capture_beat<=0;capture_full<=1;end else capture_beat<=capture_beat+1;
       |end
       |$advance
       |if(out_valid && out_ready)begin if(output_beat==${beats-1})output_beat<=0;else output_beat<=output_beat+1;end
       |end
       |end
       |endmodule
       |${StageParallelArithmetic.emit(field,top,reduction)}
       |""".stripMargin
