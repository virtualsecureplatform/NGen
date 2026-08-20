package ngen.backend

import ngen.rtl.{ProfileName, TransposeKind}

/** Round-robin full-throughput shell around independent stage-parallel YATA engines. */
object YataFullThroughputSystemVerilog:
  val EngineCount = 3

  def emit(logSize: Int, streamingLog: Int, profile: ProfileName, top: String): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val lanes = 1 << streamingLog
    val cycles = (1 << logSize) / lanes
    val coreTop = s"${top}Engine"
    val coreRtl = YataPipelinedSystemVerilog.emit(logSize,streamingLog,profile,coreTop,TransposeKind.Indexed)
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++ Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output [26:0] io_intt_out_$i") ++ Vector("output io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++ Vector.tabulate(lanes)(i => s"output [31:0] io_ntt_out_$i") ++ Vector("output io_ntt_validout")
    val declarations = (0 until EngineCount).flatMap { engine =>
      Vector(s"wire engine_${engine}_intt_validout,engine_${engine}_ntt_validout;") ++
        Vector.tabulate(lanes)(lane => s"wire [26:0] engine_${engine}_intt_out_$lane;") ++
        Vector.tabulate(lanes)(lane => s"wire [31:0] engine_${engine}_ntt_out_$lane;")
    }
    val instances = (0 until EngineCount).map { engine =>
      val connections = Vector(
        ".clock(clock)", ".reset(reset)",
        s".io_intt_validin(io_intt_validin&&(engine_select==2'd$engine))"
      ) ++ Vector.tabulate(lanes)(lane => s".io_intt_in_$lane(io_intt_in_$lane)") ++
        Vector.tabulate(lanes)(lane => s".io_intt_out_$lane(engine_${engine}_intt_out_$lane)") ++
        Vector(s".io_intt_validout(engine_${engine}_intt_validout)",s".io_ntt_validin(io_ntt_validin&&(engine_select==2'd$engine))") ++
        Vector.tabulate(lanes)(lane => s".io_ntt_in_$lane(io_ntt_in_$lane)") ++
        Vector.tabulate(lanes)(lane => s".io_ntt_out_$lane(engine_${engine}_ntt_out_$lane)") ++
        Vector(s".io_ntt_validout(engine_${engine}_ntt_validout)")
      s"$coreTop engine_$engine(${connections.mkString(",")});"
    }
    val inttValid = (0 until EngineCount).map(i => s"engine_${i}_intt_validout").mkString("||")
    val nttValid = (0 until EngineCount).map(i => s"engine_${i}_ntt_validout").mkString("||")
    def mux(lane: Int, inverse: Boolean): String =
      val suffix = if inverse then "intt" else "ntt"
      (0 until EngineCount - 1).map(i => s"engine_${i}_${suffix}_validout?engine_${i}_${suffix}_out_$lane:").mkString + s"engine_${EngineCount - 1}_${suffix}_out_$lane"
    val outputAssignments = Vector.tabulate(lanes)(lane => s"assign io_intt_out_$lane=${mux(lane,true)};") ++
      Vector.tabulate(lanes)(lane => s"assign io_ntt_out_$lane=${mux(lane,false)};")
    s"""$coreRtl
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer STREAM_CYCLES=$cycles,ENGINE_COUNT=$EngineCount;
       |  reg [1:0] engine_select; integer input_count;
       |${declarations.map("  "+_).mkString("\n")}
       |  ${instances.mkString("\n  ")}
       |  assign io_intt_validout=$inttValid;
       |  assign io_ntt_validout=$nttValid;
       |  ${outputAssignments.mkString("\n  ")}
       |  always @(posedge clock)begin
       |    if(reset)begin engine_select<=0;input_count<=0;end
       |    else if(io_intt_validin||io_ntt_validin)begin
       |      if(input_count==STREAM_CYCLES-1)begin input_count<=0;if(engine_select==ENGINE_COUNT-1)engine_select<=0;else engine_select<=engine_select+1;end
       |      else input_count<=input_count+1;
       |    end else input_count<=0;
       |  end
       |endmodule
       |""".stripMargin
