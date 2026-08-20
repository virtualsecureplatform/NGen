package ngen.backend

import ngen.arithmetic.YataField
import ngen.rtl.{ProfileName, StreamProtocol}

/** One native transaction pipeline: capture -> recursive radix-8 stages -> drain.
  * Unlike the original baseline, stages are not replicated inside independent
  * engines. A completed stream tensor advances into the next stage while the
  * following transaction is captured.
  */
object YataFullThroughputSystemVerilog:
  private def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")

  private val arithmetic =
    """
      |  localparam signed [53:0] P=54'sd40960001;
      |  function automatic signed [26:0] yata_add(input signed [53:0] x,input signed [53:0] y); reg signed [53:0] v; begin v=x+y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;yata_add=v[26:0];end endfunction
      |  function automatic signed [26:0] yata_sub(input signed [53:0] x,input signed [53:0] y); reg signed [53:0] v; begin v=x-y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;yata_sub=v[26:0];end endfunction
      |  function automatic signed [26:0] yata_sredc(input signed [53:0] x); reg[26:0] a0;reg signed[26:0] a1,m,t1;reg signed[53:0] mw,tw; begin a0=x[26:0];a1=x[53:27];mw=-(({27'd0,a0}*625)<<<16)+{27'd0,a0};m=mw[26:0];tw=(($signed(m)*625)<<<16)+$signed(m);t1=tw[53:27];yata_sredc=a1-t1;end endfunction
      |  function automatic signed [26:0] yata_mulredc(input signed [26:0] x,input signed [26:0] y); begin yata_mulredc=yata_sredc($signed(x)*$signed(y));end endfunction
      |  function automatic signed [53:0] yata_cmul(input signed [53:0] x,input[1:0] r,input[1:0] n); begin if(r==2&&n==1)yata_cmul=(x*25)<<<8;else if(r==3&&n==1)yata_cmul=(x*5)<<<4;else if(r==3&&n==2)yata_cmul=(x*25)<<<8;else if(r==3&&n==3)yata_cmul=(x*125)<<<12;else yata_cmul=x;end endfunction
      |""".stripMargin

  def pipelineDepth(logSize: Int): Int = YataPipelinedSystemVerilog.stageCounts(logSize)._1

  def emit(logSize: Int, streamingLog: Int, profile: ProfileName, top: String, protocol: StreamProtocol = StreamProtocol.NextPulse): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    require((logSize == 3 && streamingLog == 3) || (logSize == 6 && streamingLog == 3) || (logSize == 9 && streamingLog == 6))
    val size = 1 << logSize
    val lanes = 1 << streamingLog
    val cycles = size / lanes
    val tables = YataField.tables(logSize)
    val inverse = YataPipelinedSystemVerilog.inverseStages(logSize, tables)
    val forward = YataPipelinedSystemVerilog.forwardStages(logSize, tables)
    require(inverse.size == forward.size)
    val depth = inverse.size
    val stageModules = inverse.indices.map { index =>
      s"""module ${top}RecursiveStage$index(input inverse_operation,input [${size * 54 - 1}:0] in_bus,output reg [${size * 54 - 1}:0] out_bus);
         |$arithmetic
         |  reg signed [53:0] work[0:${size - 1}],stage_next[0:${size - 1}],tmp,tmp2;integer j;
         |  always @(*) begin for(j=0;j<$size;j=j+1)begin work[j]=in_bus[j*54+:54];stage_next[j]=work[j];end tmp=0;tmp2=0;
         |    if(inverse_operation)begin
         |${lines(inverse(index).lines,6)}
         |    end else begin
         |${lines(forward(index).lines,6)}
         |    end
         |    for(j=0;j<$size;j=j+1)out_bus[j*54+:54]=stage_next[j];
         |  end
         |endmodule""".stripMargin
    }.mkString("\n")
    val handshakePorts = protocol match
      case StreamProtocol.NextPulse => Vector.empty
      case StreamProtocol.ReadyValid => Vector("output io_in_ready", "input io_out_ready")
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++ Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output [26:0] io_intt_out_$i") ++ Vector("output io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++ Vector.tabulate(lanes)(i => s"output [31:0] io_ntt_out_$i") ++ Vector("output io_ntt_validout") ++ handshakePorts
    val pipeDecl = (0 until depth).map(i => s"reg [${size * 54 - 1}:0] pipe_$i;reg pipe_valid_$i,pipe_inverse_$i;wire [${size * 54 - 1}:0] stage_out_$i;").mkString("\n  ")
    val instances = (0 until depth).map { i =>
      val input = if i == 0 then "capture_bus" else s"pipe_${i - 1}"
      val inverseSignal = if i == 0 then "capture_inverse" else s"pipe_inverse_${i - 1}"
      s"${top}RecursiveStage$i stage_$i($inverseSignal,$input,stage_out_$i);"
    }.mkString("\n  ")
    val captureStoreCases = (0 until cycles).map { cycle =>
      val values = Vector.tabulate(lanes) { lane =>
        val inttIndex = lane * cycles + cycle
        val nttIndex = cycle * lanes + lane
        s"if(io_intt_validin)capture[$inttIndex]<={22'd0,io_intt_in_$lane};else capture[$nttIndex]<={{27{io_ntt_in_$lane[26]}},io_ntt_in_$lane};"
      }
      s"$cycle:begin\n${lines(values,10)}\n        end"
    }.mkString("\n")
    val captureOverlayCases = (0 until cycles).map { cycle =>
      val values = Vector.tabulate(lanes) { lane =>
        val inttIndex = lane * cycles + cycle
        val nttIndex = cycle * lanes + lane
        s"if(io_intt_validin)capture_bus[${inttIndex * 54}+:54]={22'd0,io_intt_in_$lane};else if(io_ntt_validin)capture_bus[${nttIndex * 54}+:54]={{27{io_ntt_in_$lane[26]}},io_ntt_in_$lane};"
      }
      s"$cycle:begin\n${lines(values,10)}\n        end"
    }.mkString("\n")
    // The combinational bus defaults to the stored tensor; the current final
    // beat is overlaid before stage zero samples it.
    val captureDefaults = (0 until size).map(i => s"capture_bus[${i * 54}+:54]=capture[$i];").mkString("\n    ")
    val outputAssignments = Vector.tabulate(lanes) { lane =>
      val inttIndex = s"output_count*$lanes+$lane"
      val nttIndex = s"$lane*$cycles+output_count"
      s"assign io_intt_out_$lane=output_bus[($inttIndex)*54+:27]; YataFullThroughputModSwitch modswitch_$lane(output_bus[($nttIndex)*54+:54],io_ntt_out_$lane);"
    }.mkString("\n  ")
    val resetPipe = (0 until depth).map(i => s"pipe_valid_$i<=0;pipe_inverse_$i<=0;pipe_$i<='0;").mkString(" ")
    val shiftPipe = (1 until depth).map(i => s"pipe_valid_$i<=pipe_valid_${i - 1};if(pipe_valid_${i - 1})begin pipe_$i<=stage_out_$i;pipe_inverse_$i<=pipe_inverse_${i - 1};end").mkString(" ")
    val accept = if protocol == StreamProtocol.NextPulse then "(io_intt_validin||io_ntt_validin)" else "((io_intt_validin||io_ntt_validin)&&io_in_ready)"
    val advanceGuard = if protocol == StreamProtocol.NextPulse then "1'b1" else "(!output_active||io_out_ready)"
    val readyAssignment = if protocol == StreamProtocol.NextPulse then "" else s"assign io_in_ready=$advanceGuard;"
    val sequentialBody =
      s"""$shiftPipe
         |      pipe_valid_0<=0;
         |      if($accept)begin case(input_count)$captureStoreCases endcase capture_inverse<=io_intt_validin;if(input_count==CYCLES-1)begin input_count<=0;pipe_0<=stage_out_0;pipe_valid_0<=1;pipe_inverse_0<=io_intt_validin;end else input_count<=input_count+1;end
         |      if(pipe_valid_${depth - 1})begin output_bus<=pipe_${depth - 1};output_inverse<=pipe_inverse_${depth - 1};output_active<=1;output_count<=0;end
         |      else if(output_active)begin if(output_count==CYCLES-1)begin output_active<=0;output_count<=0;end else output_count<=output_count+1;end""".stripMargin
    val sequential = protocol match
      case StreamProtocol.NextPulse => sequentialBody
      case StreamProtocol.ReadyValid => s"if($advanceGuard)begin\n$sequentialBody\n      end"
    s"""// Generated by NGen's native recursive full-throughput YATA backend.
       |/* verilator lint_off DECLFILENAME */
       |$stageModules
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer N=$size,LANES=$lanes,CYCLES=$cycles,PIPELINE_DEPTH=$depth;
       |  reg signed [53:0] capture[0:${size - 1}];reg [${size * 54 - 1}:0] capture_bus,output_bus;reg capture_inverse,output_inverse,output_active;integer input_count,output_count,j;
       |  $pipeDecl
       |  always @(*)begin $captureDefaults case(input_count)$captureOverlayCases endcase end
       |  $instances
       |  $readyAssignment
       |  assign io_intt_validout=output_active&&output_inverse;assign io_ntt_validout=output_active&&!output_inverse;
       |  $outputAssignments
       |  always @(posedge clock)begin
       |    if(reset)begin input_count<=0;output_count<=0;capture_inverse<=0;output_inverse<=0;output_active<=0;output_bus<='0;$resetPipe for(j=0;j<N;j=j+1)capture[j]<='0;end
       |    else begin
       |      $sequential
       |    end
       |  end
       |endmodule
       |module YataFullThroughputModSwitch(input signed [53:0] value,output [31:0] torus);localparam signed[53:0]P=54'sd40960001;localparam[63:0]SCALE=64'd7036874245;reg signed[26:0]residue;reg[63:0]positive;reg[95:0]scaled;always @(*)begin residue=value[26:0];positive=(residue<0)?residue+P:residue;scaled=((positive*SCALE)+96'd33554432)>>26;end assign torus=scaled[31:0];endmodule
       |""".stripMargin
