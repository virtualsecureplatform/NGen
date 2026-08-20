package ngen.backend

import ngen.arithmetic.HogeField
import ngen.rtl.{ProfileName, SwitchTransposeSpec, TransposeKind}

/** A transaction-overlapped, two-pass radix-32 implementation of HOGE's NTT.
  *
  * Each radix-32 pass is split into five registered recursive levels.  The two
  * tensor transposes are streaming switch networks, so no transform-wide FSM or
  * 1024-element work array sits on the datapath.
  */
object HogeFullThroughputSystemVerilog:
  val StreamCycles = 32
  val RadixPipelineDepth = 5

  private def hex(value: BigInt): String = f"64'h${HogeField.normalize(value)}%016x"

  private def arithmetic: String =
    """
      |  localparam [63:0] HOGE_P=64'hffffffff00000001;
      |  function automatic [63:0] hoge_add(input [63:0] a,input [63:0] b); reg [64:0] s; begin s={1'b0,a}+{1'b0,b}; hoge_add=(s[64]||s[63:0]>=HOGE_P)?s[63:0]+64'h00000000ffffffff:s[63:0]; end endfunction
      |  function automatic [63:0] hoge_sub(input [63:0] a,input [63:0] b); reg [64:0] d; begin d={1'b0,a}-{1'b0,b}; hoge_sub=d[64]?d[63:0]-64'h00000000ffffffff:d[63:0]; end endfunction
      |  function automatic [63:0] hoge_mul(input [63:0] a,input [63:0] b); reg [127:0] p; reg [31:0] t0,t1,t2,t3; reg [63:0] lo,middle,res; begin p=a*b;lo=p[63:0];t0=p[31:0];t1=p[63:32];t2=p[95:64];t3=p[127:96];middle={32'd0,t1}+t2;res=(middle<<32)+t0-t3-t2;if((res>lo)&&(t2==0))res=res-64'h00000000ffffffff;if((res<lo)&&(t2!=0))res=res+64'h00000000ffffffff;hoge_mul=(res>=HOGE_P)?res+64'h00000000ffffffff:res; end endfunction
      |""".stripMargin

  private def radix32: String =
    val levels = (1 to 5).map { depth =>
      val size = 1 << depth
      val half = size / 2
      val source = if depth == 1 then "data_in" else s"level_${depth - 1}"
      val operations = (0 until 32 by size).flatMap { offset =>
        (0 until half).flatMap { index =>
          val left = offset + index
          val right = left + half
          val factor =
            if depth == 1 then BigInt(1)
            else BigInt(2).modPow(3 * (64 - (index << (6 - depth))), HogeField.Modulus)
          Vector(
            s"level_${depth}_next[$left]=hoge_add($source[$left],hoge_mul($source[$right],${hex(factor)}));",
            s"level_${depth}_next[$right]=hoge_sub($source[$left],hoge_mul($source[$right],${hex(factor)}));"
          )
        }
      }.mkString(" ")
      s"always @(*) begin $operations end"
    }.mkString("\n  ")
    val declarations = (1 to 5).map(d => s"reg [63:0] level_$d [0:31]; reg [63:0] level_${d}_next [0:31];").mkString("\n  ")
    val reset = (1 to 5).map(d => s"for(i=0;i<32;i=i+1) level_$d[i]<='0;").mkString(" ")
    val clock = (1 to 5).map(d => s"for(i=0;i<32;i=i+1) level_$d[i]<=level_${d}_next[i];").mkString(" ")
    s"""module HogeForwardRadix32Pipeline(input clock,input reset,input valid_in,input [4:0] cycle_in,input [2047:0] packed_in,output valid_out,output [4:0] cycle_out,output [2047:0] packed_out);
       |$arithmetic
       |  wire [63:0] data_in [0:31]; $declarations
       |  reg [4:0] cycle_pipe [0:4]; reg [4:0] valid_pipe; integer i;
       |  genvar g; generate for(g=0;g<32;g=g+1) begin assign data_in[g]=packed_in[g*64+:64]; assign packed_out[g*64+:64]=level_5[g]; end endgenerate
       |  assign valid_out=valid_pipe[4]; assign cycle_out=cycle_pipe[4];
       |  $levels
       |  always @(posedge clock) begin
       |    if(reset) begin valid_pipe<=0; for(i=0;i<5;i=i+1) cycle_pipe[i]<=0; $reset end
       |    else begin valid_pipe<={valid_pipe[3:0],valid_in}; cycle_pipe[0]<=cycle_in; for(i=1;i<5;i=i+1) cycle_pipe[i]<=cycle_pipe[i-1]; $clock end
       |  end
       |endmodule
       |""".stripMargin

  private def inverseRadix32(former: Boolean): String =
    val levels = (0 until 5).map { stage =>
      val depth = 5 - stage
      val size = 1 << depth
      val half = size / 2
      val source = if stage == 0 then "data_in" else s"level_$stage"
      val destination = stage + 1
      val operations = (0 until 32 by size).flatMap { offset =>
        (0 until half).flatMap { index =>
          val left = offset + index
          val right = left + half
          if former && stage == 0 then
            val pre = BigInt(2).modPow(48, HogeField.Modulus)
            val upper = BigInt(2).modPow(3 * index, HogeField.Modulus)
            val lower = BigInt(2).modPow(9 * index, HogeField.Modulus)
            Vector(
              s"level_${destination}_next[$left]=hoge_mul(hoge_add($source[$left],hoge_mul($source[$right],${hex(pre)})),${hex(upper)});",
              s"level_${destination}_next[$right]=hoge_mul(hoge_sub($source[$left],hoge_mul($source[$right],${hex(pre)})),${hex(lower)});"
            )
          else
            val factor = BigInt(2).modPow(3 * (index << (6 - depth)), HogeField.Modulus)
            Vector(
              s"level_${destination}_next[$left]=hoge_add($source[$left],$source[$right]);",
              s"level_${destination}_next[$right]=hoge_mul(hoge_sub($source[$left],$source[$right]),${hex(factor)});"
            )
        }
      }.mkString(" ")
      s"always @(*) begin $operations end"
    }.mkString("\n  ")
    val declarations = (1 to 5).map(d => s"reg [63:0] level_$d [0:31]; reg [63:0] level_${d}_next [0:31];").mkString("\n  ")
    val reset = (1 to 5).map(d => s"for(i=0;i<32;i=i+1) level_$d[i]<='0;").mkString(" ")
    val clock = (1 to 5).map(d => s"for(i=0;i<32;i=i+1) level_$d[i]<=level_${d}_next[i];").mkString(" ")
    val moduleName = if former then "HogeFormerInverseRadix32Pipeline" else "HogeInverseRadix32Pipeline"
    s"""module $moduleName(input clock,input reset,input valid_in,input [4:0] cycle_in,input [2047:0] packed_in,output valid_out,output [4:0] cycle_out,output [2047:0] packed_out);
       |$arithmetic
       |  wire [63:0] data_in [0:31]; $declarations
       |  reg [4:0] cycle_pipe [0:4]; reg [4:0] valid_pipe; integer i;
       |  genvar g; generate for(g=0;g<32;g=g+1) begin assign data_in[g]=packed_in[g*64+:64]; assign packed_out[g*64+:64]=level_5[g]; end endgenerate
       |  assign valid_out=valid_pipe[4]; assign cycle_out=cycle_pipe[4];
       |  $levels
       |  always @(posedge clock) begin
       |    if(reset) begin valid_pipe<=0; for(i=0;i<5;i=i+1) cycle_pipe[i]<=0; $reset end
       |    else begin valid_pipe<={valid_pipe[3:0],valid_in}; cycle_pipe[0]<=cycle_in; for(i=1;i<5;i=i+1) cycle_pipe[i]<=cycle_pipe[i-1]; $clock end
       |  end
       |endmodule
       |""".stripMargin

  private def factorFunction(name: String, values: Vector[BigInt]): String =
    val entries = values.zipWithIndex.map((v, i) => s"10'd$i: $name=${hex(v)};").mkString(" ")
    s"function automatic [63:0] $name(input [4:0] cycle,input [4:0] lane); begin case({cycle,lane}) $entries default: $name=64'd1; endcase end endfunction"

  def emit(top: String = "NTTWrap", inverse: Boolean = false, profile: ProfileName = ProfileName.Baseline, transpose: TransposeKind = TransposeKind.Switch): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    require(transpose == TransposeKind.Switch, "HOGE full-throughput uses recursive switch transposes")
    val tables = HogeField.tables(10)
    val invSize = HogeField.inversePowerOfTwo(10)
    val twiddles = Vector.tabulate(1024) { index =>
      val cycle = index / 32
      val lane = index % 32
      tables.forward(HogeField.reverse(cycle, 5) * lane)
    }
    val twists = Vector.tabulate(1024) { index =>
      val cycle = index / 32
      val lane = index % 32
      HogeField.multiply(tables.forwardTwist(lane * 32 + cycle), invSize)
    }
    val factors = factorFunction("twiddle_factor", twiddles) + "\n  " + factorFunction("twist_factor", twists)
    val inverseFactors = Vector.tabulate(1024) { index =>
      val cycle = index / 32
      val lane = index % 32
      HogeField.multiply(tables.inverse(HogeField.reverse(lane, 5) * cycle), tables.inverseTwist(cycle))
    }
    val transposeDefinitions = SwitchTransposeSystemVerilog.definitions(SwitchTransposeSpec(5, 64), "HogeFT")
    if inverse then
      val inverseFactorFunction = factorFunction("inverse_factor", inverseFactors)
      s"""// Generated by NGen's SGen-style full-throughput recursive HOGE inverse backend.
         |$transposeDefinitions
         |${inverseRadix32(former = true)}
         |${inverseRadix32(former = false)}
         |module $top(input clock,input reset,input io_enable,output io_validout,input [1023:0] io_in,output [2047:0] io_out);
         |$arithmetic
         |  $inverseFactorFunction
         |  reg [4:0] input_cycle,between_cycle;
         |  wire r1_valid,t1_valid,r2_valid; wire [4:0] r1_cycle,r2_cycle; wire [2047:0] widened,r1_data,r1_scaled,t1_data,r2_data;
         |  genvar w; generate for(w=0;w<32;w=w+1) begin assign widened[w*64+:64]={32'd0,io_in[w*32+:32]}; end endgenerate
         |  HogeFormerInverseRadix32Pipeline r1(clock,reset,io_enable,input_cycle,widened,r1_valid,r1_cycle,r1_data);
         |  genvar a; generate for(a=0;a<32;a=a+1) begin assign r1_scaled[a*64+:64]=hoge_mul(r1_data[a*64+:64],inverse_factor(r1_cycle,a)); end endgenerate
         |  HogeFTNGenSwitchTransposeNetwork_5 transpose1(clock,reset,r1_valid,r1_scaled,t1_valid,t1_data);
         |  HogeInverseRadix32Pipeline r2(clock,reset,t1_valid,between_cycle,t1_data,r2_valid,r2_cycle,r2_data);
         |  assign io_validout=r2_valid; assign io_out=r2_data;
         |  always @(posedge clock) begin if(reset) begin input_cycle<=0;between_cycle<=0; end else begin if(io_enable) input_cycle<=input_cycle+1'b1;else input_cycle<=0;if(t1_valid)between_cycle<=between_cycle+1'b1;else between_cycle<=0;end end
         |endmodule
         |""".stripMargin
    else
      s"""// Generated by NGen's SGen-style full-throughput recursive HOGE backend.
       |$transposeDefinitions
       |${radix32}
       |module $top(input clock,input reset,input io_enable,output io_ready,output io_validout,input [2047:0] io_in,output [2047:0] io_out);
       |$arithmetic
       |  $factors
       |  reg [4:0] input_cycle,between_cycle; integer lane;
       |  wire r1_valid,t1_valid,r2_valid,t2_valid; wire [4:0] r1_cycle,r2_cycle;
       |  wire [2047:0] r1_data,r1_scaled,t1_data,r2_data,r2_scaled;
       |  assign io_ready=1'b1;
       |  HogeForwardRadix32Pipeline r1(clock,reset,io_enable,input_cycle,io_in,r1_valid,r1_cycle,r1_data);
       |  genvar a; generate for(a=0;a<32;a=a+1) begin assign r1_scaled[a*64+:64]=hoge_mul(r1_data[a*64+:64],twiddle_factor(r1_cycle,a)); end endgenerate
       |  HogeFTNGenSwitchTransposeNetwork_5 transpose1(clock,reset,r1_valid,r1_scaled,t1_valid,t1_data);
       |  HogeForwardRadix32Pipeline r2(clock,reset,t1_valid,between_cycle,t1_data,r2_valid,r2_cycle,r2_data);
       |  genvar b; generate for(b=0;b<32;b=b+1) begin assign r2_scaled[b*64+:64]=hoge_mul(r2_data[b*64+:64],twist_factor(r2_cycle,b)); end endgenerate
       |  HogeFTNGenSwitchTransposeNetwork_5 transpose2(clock,reset,r2_valid,r2_scaled,t2_valid,io_out);
       |  assign io_validout=t2_valid;
       |  always @(posedge clock) begin
       |    if(reset) begin input_cycle<=0;between_cycle<=0; end
       |    else begin if(io_enable) input_cycle<=input_cycle+1'b1; else input_cycle<=0; if(t1_valid) between_cycle<=between_cycle+1'b1; else between_cycle<=0; end
       |  end
       |endmodule
       |""".stripMargin
