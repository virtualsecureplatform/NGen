package ngen.backend

/** Registered Goldilocks shift/reduction and butterflies, with fixed II one.
  * Every operand and valid token crosses the same number of stage boundaries.
  */
object HogeRadixPipeline:
  val ShiftDepth = 7
  val ButterflyDepth = 2
  val Depth = 5 * (ShiftDepth + ButterflyDepth)
  val FormerDepth = Depth + ShiftDepth

  private[ngen] val primitives: String =
    """module HogeShiftPipeline #(parameter integer EXP=0)(input clock,input reset,input valid_in,input[63:0]a,output valid_out,output[63:0]result);
      |  localparam integer AMOUNT=EXP%96;
      |  localparam[63:0]P=64'hffffffff00000001,EPS=64'hffffffff;
      |  reg[6:0]valid_pipe;reg[63:0]lo1,negative1,epsilon2,epsilon3,base3,corrected5,canonical6,result7;
      |  reg[31:0]hi1;reg[64:0]difference2,sum4;
      |  assign valid_out=valid_pipe[6];assign result=result7;
      |  always @(posedge clock)begin
      |    if(reset)begin valid_pipe<=0;lo1<=0;negative1<=0;hi1<=0;difference2<=0;epsilon2<=0;epsilon3<=0;base3<=0;sum4<=0;corrected5<=0;canonical6<=0;result7<=0;end
      |    else begin
      |      valid_pipe<={valid_pipe[5:0],valid_in};
      |      if(AMOUNT<64)begin lo1<=a<<AMOUNT;hi1<=a>>(64-AMOUNT);negative1<=AMOUNT<32?64'd0:a>>(96-AMOUNT);end
      |      else begin lo1<=0;hi1<=a<<(AMOUNT-64);negative1<=a>>(96-AMOUNT);end
      |      difference2<={1'b0,lo1}-{1'b0,negative1};epsilon2<={hi1,32'd0}-{32'd0,hi1};
      |      base3<=difference2[64]?difference2[63:0]-EPS:difference2[63:0];epsilon3<=epsilon2;
      |      sum4<={1'b0,base3}+{1'b0,epsilon3};
      |      corrected5<=sum4[64]?sum4[63:0]+EPS:sum4[63:0];
      |      canonical6<=corrected5>=P?corrected5+EPS:corrected5;
      |      result7<=EXP%192>=96?(canonical6==0?64'd0:P-canonical6):canonical6;
      |    end
      |  end
      |endmodule
      |module HogeButterflyPipeline(input clock,input reset,input valid_in,input[63:0]a,b,output valid_out,output reg[63:0]sum,difference);
      |  localparam[63:0]P=64'hffffffff00000001,EPS=64'hffffffff;
      |  reg[1:0]valid_pipe;reg[64:0]s1,d1;
      |  assign valid_out=valid_pipe[1];
      |  always @(posedge clock)begin
      |    if(reset)begin valid_pipe<=0;s1<=0;d1<=0;sum<=0;difference<=0;end
      |    else begin valid_pipe<={valid_pipe[0],valid_in};s1<={1'b0,a}+{1'b0,b};d1<={1'b0,a}-{1'b0,b};
      |      sum<=s1[64]||s1[63:0]>=P?s1[63:0]+EPS:s1[63:0];difference<=d1[64]?d1[63:0]-EPS:d1[63:0];end
      |  end
      |endmodule
      |""".stripMargin

  def emit(inverse: Boolean = false, former: Boolean = false): String =
    val name = if !inverse then "HogeForwardRadix32Pipeline" else if former then "HogeFormerInverseRadix32Pipeline" else "HogeInverseRadix32Pipeline"
    val stages = (0 until 5).map { stage =>
      val depth = if inverse then 5-stage else stage+1
      val size = 1 << depth
      val half = size/2
      val source = s"data_$stage"
      val dest = s"data_${stage+1}"
      val extra = inverse && former && stage == 0
      val delay = ShiftDepth + ButterflyDepth + (if extra then ShiftDepth else 0)
      val pairs = (0 until 32 by size).flatMap { offset =>
        (0 until half).map { index =>
          val left=offset+index;val right=left+half;val id=s"s${stage}_p$left"
          val isFirst=left==0
          val finalValid=if isFirst then s"valid_${stage+1}" else ""
          val setup=s"wire[63:0] ${id}_a,${id}_b,${id}_sum,${id}_diff;wire ${id}_valid;"
          if !inverse then
            val exponent=if depth==1 then 0 else 3*(64-(index << (6-depth)))%192
            s"""$setup
               |HogeShiftPipeline #(.EXP(0)) ${id}_left(clock,reset,valid_$stage,$source[$left],${id}_valid,${id}_a);
               |HogeShiftPipeline #(.EXP($exponent)) ${id}_right(clock,reset,valid_$stage,$source[$right],,${id}_b);
               |HogeButterflyPipeline ${id}_butterfly(clock,reset,${id}_valid,${id}_a,${id}_b,$finalValid,$dest[$left],$dest[$right]);""".stripMargin
          else
            val pre = if extra then
              s"""HogeShiftPipeline #(.EXP(0)) ${id}_left(clock,reset,valid_$stage,$source[$left],${id}_valid,${id}_a);
                 |HogeShiftPipeline #(.EXP(48)) ${id}_right(clock,reset,valid_$stage,$source[$right],,${id}_b);""".stripMargin
            else s"assign ${id}_a=$source[$left];assign ${id}_b=$source[$right];assign ${id}_valid=valid_$stage;"
            val upper=if extra then 3*index else 0
            val lower=if extra then 9*index else 3*(index << (6-depth))
            s"""$setup
               |$pre
               |wire ${id}_butterfly_valid;
               |HogeButterflyPipeline ${id}_butterfly(clock,reset,${id}_valid,${id}_a,${id}_b,${id}_butterfly_valid,${id}_sum,${id}_diff);
               |HogeShiftPipeline #(.EXP($upper)) ${id}_upper(clock,reset,${id}_butterfly_valid,${id}_sum,$finalValid,$dest[$left]);
               |HogeShiftPipeline #(.EXP($lower)) ${id}_lower(clock,reset,${id}_butterfly_valid,${id}_diff,,$dest[$right]);""".stripMargin
        }
      }.mkString("\n")
      s"""reg[4:0] tag_$stage[0:${delay-1}];integer i$stage;
         |assign cycle_${stage+1}=tag_$stage[${delay-1}];
         |always @(posedge clock)begin if(reset)begin for(i$stage=0;i$stage<$delay;i$stage=i$stage+1)tag_$stage[i$stage]<=0;end
         |else begin tag_$stage[0]<=cycle_$stage;for(i$stage=1;i$stage<$delay;i$stage=i$stage+1)tag_$stage[i$stage]<=tag_$stage[i$stage-1];end end
         |$pairs""".stripMargin
    }.mkString("\n")
    val declarations=(0 to 5).map(i=>s"wire[63:0]data_$i[0:31];wire valid_$i;wire[4:0]cycle_$i;").mkString("\n")
    s"""module $name(input clock,input reset,input valid_in,input[4:0]cycle_in,input[2047:0]packed_in,output valid_out,output[4:0]cycle_out,output[2047:0]packed_out);
       |$declarations
       |assign valid_0=valid_in;assign cycle_0=cycle_in;assign valid_out=valid_5;assign cycle_out=cycle_5;
       |genvar g;generate for(g=0;g<32;g=g+1)begin assign data_0[g]=packed_in[g*64+:64];assign packed_out[g*64+:64]=data_5[g];end endgenerate
       |$stages
       |endmodule
       |""".stripMargin
