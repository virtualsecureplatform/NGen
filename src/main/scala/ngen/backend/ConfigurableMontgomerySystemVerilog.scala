package ngen.backend

import ngen.algebra.Modulus
import ngen.rtl.NttPipeline

/** Each selected stage is an arithmetic partition; no latency-only padding. */
private[backend] object ConfigurableMontgomerySystemVerilog:
  def emit(field: Modulus, top: String, pipeline: NttPipeline): String =
    val w=field.bitWidth
    val h=(w+1)/2
    val m=pipeline.multiplier(w)
    val r=pipeline.reduction
    val stages=pipeline.butterflyLatency(w)-1
    val radix=BigInt(1)<<w
    val qinv=(-field.q.modInverse(radix)).mod(radix)
    val multiplyDeclarations=if m==1 then "" else
      s"reg [${2*h-1}:0] partial_ll,partial_lh,partial_hl,partial_hh;reg [${3*h-1}:0] product_low,product_high;"
    val multiply=if m==1 then "product<=multiply_input*constant_input;" else
      s"""partial_ll<=multiply_input[${h-1}:0]*constant_input[${h-1}:0];
         |partial_lh<=multiply_input[${h-1}:0]*constant_input[${w-1}:$h];
         |partial_hl<=multiply_input[${w-1}:$h]*constant_input[${h-1}:0];
         |partial_hh<=multiply_input[${w-1}:$h]*constant_input[${w-1}:$h];
         |product_low<=partial_ll+(partial_lh<<$h);
         |product_high<=partial_hl+(partial_hh<<$h);
         |product<=product_low+(product_high<<$h);""".stripMargin
    val reduceDeclarations=if r==3 then "" else s"reg [$w:0] candidate;" +
      (if r==5 then s"reg [$w:0] low_sum;reg [${w-1}:0] upper_a,upper_b;" else "")
    val reduce= r match
      case 3 => "reduced<=(quotient>=MODULUS_EXT)?quotient-MODULUS_EXT:quotient;"
      case 4 => "candidate<=quotient;reduced<=(candidate>=MODULUS_EXT)?candidate-MODULUS_EXT:candidate;"
      case 5 =>
        s"""low_sum<={1'b0,product_m[${w-1}:0]}+{1'b0,multiple[${w-1}:0]};
           |upper_a<=product_m[${2*w-1}:$w];upper_b<=multiple[${2*w-1}:$w];
           |candidate<={1'b0,upper_a}+{1'b0,upper_b}+low_sum[$w];
           |reduced<=(candidate>=MODULUS_EXT)?candidate-MODULUS_EXT:candidate;""".stripMargin
    s"""module $top #(parameter TAG_WIDTH=1)(
       |input clock,reset,valid_in,input [1:0] kind_in,
       |input [${w-1}:0] a_in,b_in,constant_in,precon_in,input [TAG_WIDTH-1:0] tag_in,
       |output reg valid_out,output reg [${w-1}:0] out0,out1,output reg [TAG_WIDTH-1:0] tag_out);
       |localparam [${w-1}:0] MODULUS=${w}'d${field.q},QINV=${w}'d$qinv;
       |localparam [$w:0] MODULUS_EXT={1'b0,MODULUS};
       |reg [${stages-1}:0] valid_pipe;
       |reg [1:0] kind_pipe[0:${stages-1}];
       |reg [${w-1}:0] a_pipe[0:${stages-1}],b_pipe[0:${stages-1}];
       |reg [TAG_WIDTH-1:0] tag_pipe[0:${stages-1}];
       |reg [${w-1}:0] multiply_input,constant_input,correction,reduced;
       |reg [${2*w-1}:0] product,product_c,product_m,multiple;
       |$multiplyDeclarations
       |$reduceDeclarations
       |wire [${2*w}:0] sum={1'b0,product_m}+{1'b0,multiple};
       |wire [$w:0] quotient=sum[${2*w}:$w];
       |integer j;
       |function automatic [${w-1}:0] mod_add(input [${w-1}:0] a,b);
       |reg [$w:0] v;begin v={1'b0,a}+{1'b0,b};if(v>=MODULUS_EXT)v=v-MODULUS_EXT;mod_add=v[${w-1}:0];end
       |endfunction
       |function automatic [${w-1}:0] mod_sub(input [${w-1}:0] a,b);
       |reg [$w:0] v;begin v={1'b0,a}-{1'b0,b};if(v[$w])v=v+MODULUS_EXT;mod_sub=v[${w-1}:0];end
       |endfunction
       |always @(posedge clock)begin
       |if(reset)begin valid_pipe<=0;valid_out<=0;out0<=0;out1<=0;tag_out<=0;end
       |else begin
       |valid_pipe<={valid_pipe[${stages-2}:0],valid_in};
       |kind_pipe[0]<=kind_in;a_pipe[0]<=a_in;b_pipe[0]<=b_in;tag_pipe[0]<=tag_in;
       |for(j=1;j<$stages;j=j+1)begin kind_pipe[j]<=kind_pipe[j-1];a_pipe[j]<=a_pipe[j-1];b_pipe[j]<=b_pipe[j-1];tag_pipe[j]<=tag_pipe[j-1];end
       |multiply_input<=(kind_in==1)?a_in:((kind_in==3)?mod_sub(b_in,a_in):b_in);
       |constant_input<=constant_in;
       |$multiply
       |product_c<=product;correction<=product[${w-1}:0]*QINV;
       |product_m<=product_c;multiple<=correction*MODULUS;
       |$reduce
       |valid_out<=valid_pipe[${stages-1}];tag_out<=tag_pipe[${stages-1}];
       |if(kind_pipe[${stages-1}]==1)begin out0<=reduced;out1<=0;end
       |else if(kind_pipe[${stages-1}]==2)begin out0<=mod_add(a_pipe[${stages-1}],reduced);out1<=mod_sub(a_pipe[${stages-1}],reduced);end
       |else begin out0<=mod_add(a_pipe[${stages-1}],b_pipe[${stages-1}]);out1<=reduced;end
       |end
       |end
       |endmodule
       |""".stripMargin
