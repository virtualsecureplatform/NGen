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
  val FactorPipelineDepth = 9

  private def hex(value: BigInt): String = f"64'h${HogeField.normalize(value)}%016x"

  private[ngen] def arithmetic: String =
    """
      |  localparam [63:0] HOGE_P=64'hffffffff00000001;
      |  function automatic [63:0] hoge_add(input [63:0] a,input [63:0] b); reg [64:0] s; begin s={1'b0,a}+{1'b0,b}; hoge_add=(s[64]||s[63:0]>=HOGE_P)?s[63:0]+64'h00000000ffffffff:s[63:0]; end endfunction
      |  function automatic [63:0] hoge_sub(input [63:0] a,input [63:0] b); reg [64:0] d; begin d={1'b0,a}-{1'b0,b}; hoge_sub=d[64]?d[63:0]-64'h00000000ffffffff:d[63:0]; end endfunction
      |  // 2^96 = -1 modulo the Goldilocks prime. Constant shift amounts
      |  // specialize to wires and carry arithmetic without general multipliers.
      |  function automatic [63:0] hoge_shift(input [63:0] a,input integer exponent);
      |    integer amount; reg [63:0] lo,hi,upper,res; reg [31:0] low; reg [64:0] wide;
      |    begin
      |      amount=exponent%96;
      |      if(amount<32) begin
      |        lo=a<<amount; hi=a>>(64-amount);
      |        wide={1'b0,lo}+(({1'b0,hi}<<32)-hi);
      |        res=wide[64]?wide[63:0]+64'hffffffff:wide[63:0];
      |      end else if(amount<64) begin
      |        low=a<<(amount-32); lo={low,32'd0};
      |        hi=(a>>(64-amount))&64'hffffffff; upper=a>>(96-amount);
      |        res=lo+(hi<<32)-upper-hi;
      |        if(hi==0 && res>lo) res=res-64'hffffffff;
      |        else if(hi!=0 && res<lo) res=res+64'hffffffff;
      |      end else begin
      |        low=a<<(amount-64); lo={low,32'd0}-low;
      |        wide={1'b0,lo}-(a>>(96-amount));
      |        res=wide[64]?wide[63:0]-64'hffffffff:wide[63:0];
      |      end
      |      if(res>=HOGE_P) res=res+64'hffffffff;
      |      hoge_shift=(exponent%192>=96)?hoge_sub(64'd0,res):res;
      |    end
      |  endfunction
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
          val exponent = if depth == 1 then 0 else 3 * (64 - (index << (6 - depth)))
          Vector(
            s"twiddled_$depth[$right]=hoge_shift($source[$right],${exponent % 192});",
            s"level_${depth}_next[$left]=hoge_add($source[$left],twiddled_$depth[$right]);",
            s"level_${depth}_next[$right]=hoge_sub($source[$left],twiddled_$depth[$right]);"
          )
        }
      }.mkString(" ")
      s"always @(*) begin $operations end"
    }.mkString("\n  ")
    val declarations = (1 to 5).map(d => s"reg [63:0] level_$d [0:31]; reg [63:0] level_${d}_next [0:31]; reg [63:0] twiddled_$d [0:31];").mkString("\n  ")
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
            val pre = 48
            val upper = 3 * index
            val lower = 9 * index
            Vector(
              s"twiddled_$destination[$right]=hoge_shift($source[$right],$pre);",
              s"level_${destination}_next[$left]=hoge_shift(hoge_add($source[$left],twiddled_$destination[$right]),$upper);",
              s"level_${destination}_next[$right]=hoge_shift(hoge_sub($source[$left],twiddled_$destination[$right]),$lower);"
            )
          else
            val exponent = 3 * (index << (6 - depth))
            Vector(
              s"level_${destination}_next[$left]=hoge_add($source[$left],$source[$right]);",
              s"level_${destination}_next[$right]=hoge_shift(hoge_sub($source[$left],$source[$right]),$exponent);"
            )
        }
      }.mkString(" ")
      s"always @(*) begin $operations end"
    }.mkString("\n  ")
    val declarations = (1 to 5).map(d => s"reg [63:0] level_$d [0:31]; reg [63:0] level_${d}_next [0:31]; reg [63:0] twiddled_$d [0:31];").mkString("\n  ")
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

  // Each lane accepts a new coefficient every cycle. Factor lookup is captured
  // separately from multiplication, and the 64x64 product uses four registered
  // 32x32 partial products followed by two registered carry additions.
  private[ngen] def factorPipeline: String =
    """module HogeFactorPipeline(input clock,input reset,input valid_in,input [63:0] a,input [63:0] factor,output valid_out,output [63:0] result);
      |  localparam [63:0] P=64'hffffffff00000001,EPS=64'h00000000ffffffff;
      |  reg [8:0] valid_pipe;
      |  reg [63:0] a1,b1,p00,p01,p10,p11;
      |  reg [127:0] sum3,upper3,product4;
      |  reg [64:0] difference5,sum7;
      |  reg [63:0] epsilon5,epsilon6,base6,corrected8,result9;
      |  assign valid_out=valid_pipe[8];assign result=result9;
      |  always @(posedge clock) begin
      |    if(reset) begin valid_pipe<=0;a1<=0;b1<=0;p00<=0;p01<=0;p10<=0;p11<=0;sum3<=0;upper3<=0;product4<=0;difference5<=0;epsilon5<=0;epsilon6<=0;base6<=0;sum7<=0;corrected8<=0;result9<=0;end
      |    else begin
      |      valid_pipe<={valid_pipe[7:0],valid_in};a1<=a;b1<=factor;
      |      p00<=a1[31:0]*b1[31:0];p01<=a1[31:0]*b1[63:32];p10<=a1[63:32]*b1[31:0];p11<=a1[63:32]*b1[63:32];
      |      sum3<={64'd0,p00}+{32'd0,p01,32'd0};upper3<={p11,64'd0}+{32'd0,p10,32'd0};
      |      product4<=sum3+upper3;
      |      difference5<={1'b0,product4[63:0]}-{33'd0,product4[127:96]};epsilon5<={product4[95:64],32'd0}-{32'd0,product4[95:64]};
      |      base6<=difference5[64]?difference5[63:0]-EPS:difference5[63:0];epsilon6<=epsilon5;
      |      sum7<={1'b0,base6}+{1'b0,epsilon6};
      |      corrected8<=sum7[64]?sum7[63:0]+EPS:sum7[63:0];
      |      result9<=corrected8>=P?corrected8+EPS:corrected8;
      |    end
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
         |$factorPipeline
         |${inverseRadix32(former = true)}
         |${inverseRadix32(former = false)}
         |module $top(input clock,input reset,input io_enable,output io_validout,input [1023:0] io_in,output [2047:0] io_out);
         |$arithmetic
         |  $inverseFactorFunction
         |  reg [4:0] input_cycle,between_cycle;
         |  wire r1_valid,t1_valid,r2_valid; wire [31:0] scaled1_valid; wire [4:0] r1_cycle,r2_cycle; wire [2047:0] widened,r1_data,r1_scaled,t1_data,r2_data;
         |  genvar w; generate for(w=0;w<32;w=w+1) begin assign widened[w*64+:64]={32'd0,io_in[w*32+:32]}; end endgenerate
         |  HogeFormerInverseRadix32Pipeline r1(clock,reset,io_enable,input_cycle,widened,r1_valid,r1_cycle,r1_data);
         |  genvar a; generate for(a=0;a<32;a=a+1) begin HogeFactorPipeline scale(clock,reset,r1_valid,r1_data[a*64+:64],inverse_factor(r1_cycle,a),scaled1_valid[a],r1_scaled[a*64+:64]); end endgenerate
         |  HogeFTNGenSwitchTransposeNetwork_5 transpose1(clock,reset,scaled1_valid[0],r1_scaled,t1_valid,t1_data);
         |  HogeInverseRadix32Pipeline r2(clock,reset,t1_valid,between_cycle,t1_data,r2_valid,r2_cycle,r2_data);
         |  assign io_validout=r2_valid; assign io_out=r2_data;
         |  always @(posedge clock) begin if(reset) begin input_cycle<=0;between_cycle<=0; end else begin if(io_enable) input_cycle<=input_cycle+1'b1;else input_cycle<=0;if(t1_valid)between_cycle<=between_cycle+1'b1;else between_cycle<=0;end end
         |endmodule
         |""".stripMargin
    else
      s"""// Generated by NGen's SGen-style full-throughput recursive HOGE backend.
       |$transposeDefinitions
         |$factorPipeline
       |${radix32}
       |module $top(input clock,input reset,input io_enable,output io_ready,output io_validout,input [2047:0] io_in,output [2047:0] io_out);
       |$arithmetic
       |  $factors
       |  reg [4:0] input_cycle,between_cycle; integer lane;
       |  wire r1_valid,t1_valid,r2_valid,t2_valid; wire [31:0] scaled1_valid,scaled2_valid; wire [4:0] r1_cycle,r2_cycle;
       |  wire [2047:0] r1_data,r1_scaled,t1_data,r2_data,r2_scaled;
       |  assign io_ready=1'b1;
       |  HogeForwardRadix32Pipeline r1(clock,reset,io_enable,input_cycle,io_in,r1_valid,r1_cycle,r1_data);
       |  genvar a; generate for(a=0;a<32;a=a+1) begin HogeFactorPipeline scale(clock,reset,r1_valid,r1_data[a*64+:64],twiddle_factor(r1_cycle,a),scaled1_valid[a],r1_scaled[a*64+:64]); end endgenerate
       |  HogeFTNGenSwitchTransposeNetwork_5 transpose1(clock,reset,scaled1_valid[0],r1_scaled,t1_valid,t1_data);
       |  HogeForwardRadix32Pipeline r2(clock,reset,t1_valid,between_cycle,t1_data,r2_valid,r2_cycle,r2_data);
       |  genvar b; generate for(b=0;b<32;b=b+1) begin HogeFactorPipeline scale(clock,reset,r2_valid,r2_data[b*64+:64],twist_factor(r2_cycle,b),scaled2_valid[b],r2_scaled[b*64+:64]); end endgenerate
       |  HogeFTNGenSwitchTransposeNetwork_5 transpose2(clock,reset,scaled2_valid[0],r2_scaled,t2_valid,io_out);
       |  assign io_validout=t2_valid;
       |  always @(posedge clock) begin
       |    if(reset) begin input_cycle<=0;between_cycle<=0; end
       |    else begin if(io_enable) input_cycle<=input_cycle+1'b1; else input_cycle<=0; if(t1_valid) between_cycle<=between_cycle+1'b1; else between_cycle<=0; end
       |  end
       |endmodule
       |""".stripMargin
