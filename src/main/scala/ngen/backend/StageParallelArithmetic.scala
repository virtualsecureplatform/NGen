package ngen.backend

import ngen.algebra.Modulus
import ngen.arithmetic.BarrettField
import ngen.rtl.ReductionKind

/** Shared structural stage arithmetic; frame scheduling lives in each backend. */
object StageParallelArithmetic:
  def emit(field: Modulus, top: String, reduction: ReductionKind): String =
    require(Set(ReductionKind.Barrett,ReductionKind.Montgomery,ReductionKind.Shoup)(reduction))
    val width=field.bitWidth
    val barrett=BarrettField(field)
    val wordRadix=BigInt(1)<<width
    val montgomeryQInv=(-field.q.modInverse(wordRadix)).mod(wordRadix)
    val structuralArithmetic = reduction match
      case ReductionKind.Barrett =>
        s"""  localparam signed [${3 * width}:0] MODULUS_REMAINDER=${3 * width + 1}'sd${field.q};
           |  localparam [${2 * width - 1}:0] BARRETT_MU=${2 * width}'d${barrett.mu};
           |  wire [${2 * width - 1}:0] product = {{$width{1'b0}},a}*{{$width{1'b0}},b};
           |  wire [${4 * width - 1}:0] scaled = {{${2 * width}{1'b0}},product}*{{${2 * width}{1'b0}},BARRETT_MU};
           |  wire [${2 * width - 1}:0] quotient = scaled[${4 * width - 1}:${2 * width}];
           |  wire [${3 * width - 1}:0] quotient_product = {{$width{1'b0}},quotient}*{{${2 * width}{1'b0}},MODULUS};
           |  wire signed [${3 * width}:0] remainder0 = $$signed({${width + 1}'d0,product})-$$signed({1'b0,quotient_product});
           |  wire signed [${3 * width}:0] remainder1 = remainder0 < 0 ? remainder0 + MODULUS_REMAINDER : remainder0;
           |  wire signed [${3 * width}:0] remainder2 = remainder1 >= MODULUS_REMAINDER ? remainder1 - MODULUS_REMAINDER : remainder1;
           |  wire signed [${3 * width}:0] remainder3 = remainder2 >= MODULUS_REMAINDER ? remainder2 - MODULUS_REMAINDER : remainder2;
           |  assign result = remainder3[${width - 1}:0];""".stripMargin
      case ReductionKind.Montgomery =>
        s"""  localparam [${width - 1}:0] MONTGOMERY_QINV=${width}'d$montgomeryQInv;
           |  wire [${2 * width - 1}:0] product = {{$width{1'b0}},a}*{{$width{1'b0}},b};
           |  wire [${2 * width - 1}:0] correction_product = {{$width{1'b0}},product[${width - 1}:0]}*{{$width{1'b0}},MONTGOMERY_QINV};
           |  wire [${width - 1}:0] correction = correction_product[${width - 1}:0];
           |  wire [${2 * width - 1}:0] multiple = {{$width{1'b0}},correction}*{{$width{1'b0}},MODULUS};
           |  wire [${2 * width}:0] sum = {1'b0,product}+{1'b0,multiple};
           |  wire [${width}:0] upper = sum[${2 * width}:$width];
           |  wire [${width}:0] reduced = upper >= {1'b0,MODULUS} ? upper-{1'b0,MODULUS} : upper;
           |  assign result = reduced[${width - 1}:0];""".stripMargin
      case ReductionKind.Shoup =>
        s"""  wire [${2 * width - 1}:0] product = {{$width{1'b0}},a}*{{$width{1'b0}},b};
           |  wire [${2 * width - 1}:0] approximate_product = {{$width{1'b0}},a}*{{$width{1'b0}},b_shoup};
           |  wire [${width - 1}:0] approximate_quotient = approximate_product[${2 * width - 1}:$width];
           |  wire [${2 * width - 1}:0] quotient_product = {{$width{1'b0}},approximate_quotient}*{{$width{1'b0}},MODULUS};
           |  wire [${2 * width}:0] remainder0 = {1'b0,product}-{1'b0,quotient_product};
           |  wire [${2 * width}:0] remainder1 = remainder0 >= {{${width + 1}{1'b0}},MODULUS} ? remainder0-{{${width + 1}{1'b0}},MODULUS} : remainder0;
           |  assign result = remainder1[${width - 1}:0];""".stripMargin
      case _ => ""
      s"""
         |module ${top}_FieldMul(input [${width - 1}:0] a, b, b_shoup,
         |  output [${width - 1}:0] result);
         |  localparam [${width - 1}:0] MODULUS=${width}'d${field.q};
         |$structuralArithmetic
         |endmodule
         |
         |module ${top}_Butterfly #(
         |  parameter GS = 1'b0
         |)(input [${width - 1}:0] left, right, twiddle, twiddle_shoup,
         |  output [${width - 1}:0] left_out, right_out);
         |  localparam [${width}:0] MODULUS_EXT=${width + 1}'d${field.q};
         |  wire [${width}:0] gs_difference = right >= left ? {1'b0,right}-{1'b0,left} : {1'b0,right}+MODULUS_EXT-{1'b0,left};
         |  wire [${width - 1}:0] mul_operand = GS ? gs_difference[${width - 1}:0] : right;
         |  wire [${width - 1}:0] multiplied;
         |  ${top}_FieldMul twiddle_mul (.a(mul_operand), .b(twiddle), .b_shoup(twiddle_shoup), .result(multiplied));
         |  wire [${width}:0] add_right = GS ? {1'b0,right} : {1'b0,multiplied};
         |  wire [${width}:0] sum = {1'b0,left} + add_right;
         |  wire [${width}:0] reduced_sum = sum >= MODULUS_EXT ? sum - MODULUS_EXT : sum;
         |  wire [${width}:0] dit_difference = left >= multiplied ? {1'b0,left}-{1'b0,multiplied} : {1'b0,left}+MODULUS_EXT-{1'b0,multiplied};
         |  assign left_out = reduced_sum[${width - 1}:0];
         |  assign right_out = GS ? multiplied : dit_difference[${width - 1}:0];
         |endmodule
         |""".stripMargin
