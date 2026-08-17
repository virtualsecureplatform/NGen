package ngen.backend

import ngen.algebra.Modulus
import ngen.arithmetic.BarrettField
import ngen.rtl.ReductionKind

/** Three-stage, one-operation-per-cycle modular radix-2 butterfly pipeline. */
object PipelinedButterflySystemVerilog:
  val Latency = 3

  def emit(field: Modulus, reduction: ReductionKind, top: String = "NGenPipelinedButterfly", runtimeField: Boolean = false): String =
    require(Set(ReductionKind.Barrett, ReductionKind.Montgomery, ReductionKind.Shoup, ReductionKind.FermatShift)(reduction))
    val width = field.bitWidth
    val radix = BigInt(1) << width
    val qInv = (-field.q.modInverse(radix)).mod(radix)
    val mu = BarrettField(field).mu
    val bypassConstant = if reduction == ReductionKind.Montgomery then field.multiply(1,radix) else BigInt(1)
    val parameters = reduction match
      case ReductionKind.Barrett => if runtimeField then "" else s"localparam [${2 * width - 1}:0] MU=${2 * width}'d$mu;"
      case ReductionKind.Montgomery => s"localparam [${width - 1}:0] QINV=${width}'d$qInv;"
      case ReductionKind.Shoup => ""
      case ReductionKind.FermatShift => s"localparam integer FERMAT_PERIOD=${2 * (width - 1)};"
      case _ => ""
    val stageRegisters = reduction match
      case ReductionKind.Shoup =>
        s"reg [${2 * width - 1}:0] product_0,approximate_0,product_1,quotient_product_1;reg [${width - 1}:0] quotient_1;"
      case ReductionKind.Barrett =>
        s"reg [${2 * width - 1}:0] product_0,product_1;reg [${4 * width - 1}:0] scaled_1;"
      case ReductionKind.Montgomery =>
        s"reg [${2 * width - 1}:0] product_0,product_1,correction_product_1;"
      case ReductionKind.FermatShift => s"reg [${width - 1}:0] product_0,product_1;"
      case _ => ""
    val stageZero = reduction match
      case ReductionKind.Shoup =>
        s"if(constant_in==${width}'d1)begin product_0<=multiply_input;approximate_0<=0;end else begin product_0<={{$width{1'b0}},multiply_input}*{{$width{1'b0}},constant_in};approximate_0<={{$width{1'b0}},multiply_input}*{{$width{1'b0}},precon_in};end"
      case ReductionKind.Barrett | ReductionKind.Montgomery =>
        val bypass=if reduction==ReductionKind.Montgomery then field.multiply(1,radix) else BigInt(1)
        s"if(constant_in==${width}'d$bypass)product_0<=multiply_input;else product_0<={{$width{1'b0}},multiply_input}*{{$width{1'b0}},constant_in};"
      case ReductionKind.FermatShift => s"product_0<=fermat_mul(multiply_input,constant_in);"
      case _ => ""
    val stageOne = reduction match
      case ReductionKind.Shoup =>
        s"product_1<=product_0;quotient_1<=approximate_0[${2 * width - 1}:$width];quotient_product_1<={{$width{1'b0}},approximate_0[${2 * width - 1}:$width]}*{{$width{1'b0}},MODULUS};"
      case ReductionKind.Barrett =>
        s"product_1<=product_0;scaled_1<={{${2 * width}{1'b0}},product_0}*{{${2 * width}{1'b0}},MU};"
      case ReductionKind.Montgomery =>
        s"product_1<=product_0;correction_product_1<={{$width{1'b0}},product_0[${width - 1}:0]}*{{$width{1'b0}},QINV};"
      case ReductionKind.FermatShift => s"product_1<=product_0;"
      case _ => ""
    val stageTwo = reduction match
      case ReductionKind.Shoup =>
        s"wide_value={1'b0,product_1}-{1'b0,quotient_product_1};if(wide_value>={{${width + 1}{1'b0}},MODULUS})wide_value=wide_value-{{${width + 1}{1'b0}},MODULUS};reduced=wide_value[${width - 1}:0];"
      case ReductionKind.Barrett =>
        s"quotient_product_temp={{$width{1'b0}},scaled_1[${4 * width - 1}:${2 * width}]}*{{${2 * width}{1'b0}},MODULUS};signed_value=$$signed({${width + 1}'d0,product_1})-$$signed({1'b0,quotient_product_temp});if(signed_value<0)signed_value=signed_value+MODULUS_REMAINDER;if(signed_value>=MODULUS_REMAINDER)signed_value=signed_value-MODULUS_REMAINDER;if(signed_value>=MODULUS_REMAINDER)signed_value=signed_value-MODULUS_REMAINDER;reduced=signed_value[${width - 1}:0];"
      case ReductionKind.Montgomery =>
        s"if(bypass_1)reduced=product_1;else begin multiple_temp={{$width{1'b0}},correction_product_1[${width - 1}:0]}*{{$width{1'b0}},MODULUS};wide_value={1'b0,product_1}+{1'b0,multiple_temp};montgomery_value=wide_value[${2 * width}:$width];if(montgomery_value>={1'b0,MODULUS})montgomery_value=montgomery_value-{1'b0,MODULUS};reduced=montgomery_value[${width - 1}:0];end"
      case ReductionKind.FermatShift => s"reduced=product_1;"
      case _ => ""
    require(!runtimeField || reduction == ReductionKind.Barrett, "runtime modulus loading currently uses the generic Barrett pipeline")
    val runtimePorts=if runtimeField then s",input [${width-1}:0] modulus_in,input [${2*width-1}:0] reduction_constant_in" else ""
    val fieldConstants=if runtimeField then s"wire [${width-1}:0] MODULUS=modulus_in;wire [${width}:0] MODULUS_EXT={1'b0,modulus_in};wire signed [${3*width}:0] MODULUS_REMAINDER=$$signed({${2*width+1}'d0,modulus_in});wire [${2*width-1}:0] MU=reduction_constant_in;" else s"localparam [${width-1}:0] MODULUS=${width}'d${field.q};localparam [${width}:0] MODULUS_EXT=${width+1}'d${field.q};localparam signed [${3*width}:0] MODULUS_REMAINDER=${3*width+1}'sd${field.q};$parameters"
    s"""module $top #(parameter TAG_WIDTH=1)(
       |  input clock,input reset,input valid_in,input [1:0] kind_in,
       |  input [${width - 1}:0] a_in,input [${width - 1}:0] b_in,input [${width - 1}:0] constant_in,input [${width - 1}:0] precon_in,input [TAG_WIDTH-1:0] tag_in,
       |  output reg valid_out,output reg [${width - 1}:0] out0,output reg [${width - 1}:0] out1,output reg [TAG_WIDTH-1:0] tag_out$runtimePorts
       |);
       |  $fieldConstants
       |  reg valid_0,valid_1,bypass_0,bypass_1;reg [1:0] kind_0,kind_1;reg [${width - 1}:0] a_0,b_0,a_1,b_1;reg [TAG_WIDTH-1:0] tag_0,tag_1;$stageRegisters
       |  wire [${width - 1}:0] multiply_input=(kind_in==1)?a_in:((kind_in==3)?mod_sub(b_in,a_in):b_in);
       |  reg [${2 * width}:0] wide_value;reg [${2 * width - 1}:0] multiple_temp;reg [${3 * width - 1}:0] quotient_product_temp;reg signed [${3 * width}:0] signed_value;reg [${width}:0] montgomery_value;reg [${width - 1}:0] reduced;
       |  function automatic [${width - 1}:0] mod_add(input [${width - 1}:0] a,input [${width - 1}:0] b);reg [${width}:0] v;begin v={1'b0,a}+{1'b0,b};if(v>=MODULUS_EXT)v=v-MODULUS_EXT;mod_add=v[${width - 1}:0];end endfunction
       |  function automatic [${width - 1}:0] mod_sub(input [${width - 1}:0] a,input [${width - 1}:0] b);reg [${width}:0] v;begin if(a>=b)v={1'b0,a}-{1'b0,b};else v={1'b0,a}+MODULUS_EXT-{1'b0,b};mod_sub=v[${width - 1}:0];end endfunction
       |  function automatic [${width - 1}:0] fermat_mul(input [${width - 1}:0] a,input [${width - 1}:0] exponent);integer j;reg [${width}:0] v;begin v={1'b0,a};for(j=0;j<${2 * (width - 1)};j=j+1)begin if(j<exponent)begin v=v<<1;if(v>=MODULUS_EXT)v=v-MODULUS_EXT;end end fermat_mul=v[${width - 1}:0];end endfunction
       |  always @(posedge clock)begin
       |    if(reset)begin valid_0<=0;valid_1<=0;valid_out<=0;out0<=0;out1<=0;tag_out<=0;end
       |    else begin
       |      valid_0<=valid_in;bypass_0<=constant_in==${width}'d$bypassConstant;kind_0<=kind_in;a_0<=a_in;b_0<=b_in;tag_0<=tag_in;$stageZero
       |      valid_1<=valid_0;bypass_1<=bypass_0;kind_1<=kind_0;a_1<=a_0;b_1<=b_0;tag_1<=tag_0;$stageOne
       |      valid_out<=valid_1;tag_out<=tag_1;$stageTwo
       |      if(kind_1==1)begin out0<=reduced;out1<=0;end
       |      else if(kind_1==2)begin out0<=mod_add(a_1,reduced);out1<=mod_sub(a_1,reduced);end
       |      else begin out0<=mod_add(a_1,b_1);out1<=reduced;end
       |    end
       |  end
       |endmodule
       |""".stripMargin
