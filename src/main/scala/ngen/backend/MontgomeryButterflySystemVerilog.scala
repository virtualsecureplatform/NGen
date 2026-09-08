package ngen.backend

import ngen.algebra.Modulus

/** One operation per cycle. Register every multiply and separate reduction from butterfly addition. */
private[backend] object MontgomeryButterflySystemVerilog:
  val Latency = 7
  def latency(width: Int): Int = if width > 32 then 9 else Latency

  def emit(field: Modulus, top: String): String =
    val w=field.bitWidth
    val wide=w>32
    val stages=latency(w)-1
    val extra=stages-6
    val half=(w+1)/2
    val productRegisters=if wide then
      s"reg [${2*half-1}:0] partial_ll,partial_lh,partial_hl,partial_hh;reg [${3*half-1}:0] product_low,product_high;"
    else ""
    val productStep=if wide then
      s"""partial_ll<=multiply_0[${half-1}:0]*constant_0[${half-1}:0];
         |   partial_lh<=multiply_0[${half-1}:0]*constant_0[${w-1}:$half];
         |   partial_hl<=multiply_0[${w-1}:$half]*constant_0[${half-1}:0];
         |   partial_hh<=multiply_0[${w-1}:$half]*constant_0[${w-1}:$half];
         |   product_low<=partial_ll+(partial_lh<<$half);
         |   product_high<=partial_hl+(partial_hh<<$half);
         |   product_1<=product_low+(product_high<<$half);""".stripMargin
    else "product_1<=bypass_pipe[0]?multiply_0:(multiply_0*constant_0);"
    val constantInput=if wide then s"(constant_in==MONT_ONE)?${w}'d1:constant_in" else "constant_in"
    val r=BigInt(1)<<w
    val qinv=(-field.q.modInverse(r)).mod(r)
    val one=r.mod(field.q)
    s"""module $top #(parameter TAG_WIDTH=1)(
       | input clock,reset,valid_in,input [1:0] kind_in,
       | input [${w-1}:0] a_in,b_in,constant_in,precon_in,input [TAG_WIDTH-1:0] tag_in,
       | output reg valid_out,output reg [${w-1}:0] out0,out1,output reg [TAG_WIDTH-1:0] tag_out);
       | localparam [${w-1}:0] MODULUS=${w}'d${field.q},QINV=${w}'d$qinv,MONT_ONE=${w}'d$one;
       | localparam [${w}:0] MODULUS_EXT={1'b0,MODULUS};
       | reg [${stages-1}:0] valid_pipe,bypass_pipe;
       | reg [1:0] kind_pipe[0:${stages-1}];
       | reg [${w-1}:0] a_pipe[0:${stages-1}],b_pipe[0:${stages-1}];
       | reg [TAG_WIDTH-1:0] tag_pipe[0:${stages-1}];
       | reg [${w-1}:0] multiply_0,constant_0,correction_2,reduced_5;
       | reg [${2*w-1}:0] product_1,product_2,product_3,multiple_3;
       | $productRegisters
       | reg [${w}:0] candidate_4;
       | wire [${2*w}:0] sum_3={1'b0,product_3}+{1'b0,multiple_3};
       | integer j;
       | function automatic [${w-1}:0] mod_add(input [${w-1}:0] a,b);
       |  reg [${w}:0] v;begin v={1'b0,a}+{1'b0,b};if(v>=MODULUS_EXT)v=v-MODULUS_EXT;mod_add=v[${w-1}:0];end
       | endfunction
       | function automatic [${w-1}:0] mod_sub(input [${w-1}:0] a,b);
       |  reg [${w}:0] v;begin v={1'b0,a}-{1'b0,b};if(v[$w])v=v+MODULUS_EXT;mod_sub=v[${w-1}:0];end
       | endfunction
       | always @(posedge clock)begin
       |  if(reset)begin valid_pipe<=0;valid_out<=0;out0<=0;out1<=0;tag_out<=0;end
       |  else begin
       |   valid_pipe<={valid_pipe[${stages-2}:0],valid_in};
       |   bypass_pipe<={bypass_pipe[${stages-2}:0],constant_in==MONT_ONE};
       |   kind_pipe[0]<=kind_in;a_pipe[0]<=a_in;b_pipe[0]<=b_in;tag_pipe[0]<=tag_in;
       |   for(j=1;j<$stages;j=j+1)begin
       |    kind_pipe[j]<=kind_pipe[j-1];a_pipe[j]<=a_pipe[j-1];b_pipe[j]<=b_pipe[j-1];tag_pipe[j]<=tag_pipe[j-1];
       |   end
       |   // Stage 0: isolate the inverse butterfly subtraction from multiplication.
       |   multiply_0<=(kind_in==1)?a_in:((kind_in==3)?mod_sub(b_in,a_in):b_in);
       |   constant_0<=$constantInput;
       |   // Product (three stages above 32 bits), correction, correction times modulus.
       |   $productStep
       |   product_2<=product_1;correction_2<=product_1[${w-1}:0]*QINV;
       |   product_3<=product_2;multiple_3<=correction_2*MODULUS;
       |   // Preserve the carry above 2W bits before dividing by the word radix.
       |   candidate_4<=bypass_pipe[${3+extra}]?{1'b0,product_3[${w-1}:0]}:sum_3[${2*w}:$w];
       |   reduced_5<=(candidate_4>=MODULUS_EXT)?candidate_4-MODULUS_EXT:candidate_4;
       |   // Final stage: butterfly outputs and their transaction tag retire together.
       |   valid_out<=valid_pipe[${stages-1}];tag_out<=tag_pipe[${stages-1}];
       |   if(kind_pipe[${stages-1}]==1)begin out0<=reduced_5;out1<=0;end
       |   else if(kind_pipe[${stages-1}]==2)begin out0<=mod_add(a_pipe[${stages-1}],reduced_5);out1<=mod_sub(a_pipe[${stages-1}],reduced_5);end
       |   else begin out0<=mod_add(a_pipe[${stages-1}],b_pipe[${stages-1}]);out1<=reduced_5;end
       |  end
       | end
       |endmodule
       |""".stripMargin
