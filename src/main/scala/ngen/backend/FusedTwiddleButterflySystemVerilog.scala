package ngen.backend

import ngen.algebra.Modulus
import ngen.rtl.DspMultiplyPlan

object FusedTwiddleButterflySystemVerilog:
  def emit(modulus:Modulus,top:String="NGenFusedTwiddleButterfly",dspDecompose:Boolean=false):String=
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val width=modulus.bitWidth
    val dsp=DspMultiplyPlan(width,width)
    val reducer=PrimeReductionSystemVerilog.emit(modulus,s"${top}Reducer")
    val partials=(for i<-0 until dsp.leftSlices;j<-0 until dsp.rightSlices yield
      val leftLow=i*dsp.dspLeftBits;val rightLow=j*dsp.dspRightBits
      val leftWidth=math.min(dsp.dspLeftBits,width-leftLow);val rightWidth=math.min(dsp.dspRightBits,width-rightLow)
      val index=i*dsp.rightSlices+j
      val shift=leftLow+rightLow
      (s"(* use_dsp=\"yes\" *) wire [${leftWidth+rightWidth-1}:0] tile_$index=right[$leftLow+:$leftWidth]*twiddle_montgomery[$rightLow+:$rightWidth];",
       s"({{${2*width-leftWidth-rightWidth}{1'b0}},tile_$index}<<$shift)")
    ).toVector
    val tileDeclarations=if dspDecompose then partials.map(_._1).mkString("\n  ") else ""
    val productTree=if dspDecompose then partials.map(_._2).mkString("+") else "right*twiddle_montgomery"
    s"""$reducer
       |module $top(input clock,input valid_in,input [$width-1:0] left,input [$width-1:0] right,input [$width-1:0] twiddle_montgomery,output reg valid_out,output reg [$width-1:0] sum,output reg [$width-1:0] difference);
       |  localparam [$width-1:0] Q=$width'd${modulus.q};localparam [$width:0] Q_EXT=${width+1}'d${modulus.q};localparam integer DSP_DECOMPOSE=${if dspDecompose then 1 else 0},DSP_TILES=${if dspDecompose then dsp.dspCount else 0},PARTIAL_ADDER_LEVELS=${if dspDecompose then dsp.adderLevels else 0};
       |  $tileDeclarations
       |  wire [${2*width-1}:0] product_tree=$productTree;reg [${2*width-1}:0] product;reg [$width-1:0] left_0;wire [$width-1:0] reduced;reg valid_0;wire [$width:0] add_value={1'b0,left_0}+{1'b0,reduced};wire [$width:0] sub_value={1'b0,left_0}+Q_EXT-{1'b0,reduced};wire [$width:0] corrected_add=add_value>=Q_EXT?add_value-Q_EXT:add_value;wire [$width:0] corrected_sub=sub_value>=Q_EXT?sub_value-Q_EXT:sub_value;
       |  ${top}Reducer reducer(product,reduced);
       |  always @(posedge clock)begin product<=product_tree;left_0<=left;valid_0<=valid_in;sum<=corrected_add[$width-1:0];difference<=corrected_sub[$width-1:0];valid_out<=valid_0;end
       |endmodule
       |""".stripMargin
