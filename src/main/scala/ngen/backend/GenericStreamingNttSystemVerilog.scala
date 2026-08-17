package ngen.backend

import ngen.arithmetic.{BarrettField, ShoupField}
import ngen.rtl.{ProfileName, ReductionKind}
import ngen.transform.{ButterflyKind, PlannedButterfly, StreamingNttPlan}

object GenericStreamingNttSystemVerilog:
  final case class Schedule(
      bundles: Vector[Vector[PlannedButterfly]],
      inputCycles: Int,
      outputCycles: Int,
      bundleGap: Int,
      latency: Int,
      initiationInterval: Int
  )

  def schedule(plan: StreamingNttPlan, streamingWidth: Int, profile: ProfileName = ProfileName.Baseline): Schedule =
    require(streamingWidth > 0 && Integer.bitCount(streamingWidth) == 1)
    require(streamingWidth <= plan.domain.size && plan.domain.size % streamingWidth == 0)
    val bundles = plan.stages.flatMap(_.butterflies.grouped(streamingWidth).map(_.toVector))
    val cycles = plan.domain.size / streamingWidth
    val bundleGap = if profile == ProfileName.F300 then 1 else 0
    val executionCycles = bundles.size + math.max(0, bundles.size - 1) * bundleGap
    // First input is captured with next. next_out rises when the first output register is written.
    val latency = cycles + executionCycles
    // A new first input may be captured together with the previous final output.
    val initiationInterval = cycles + executionCycles + cycles - 1
    Schedule(bundles, cycles, cycles, bundleGap, latency, initiationInterval)

  def emit(
      plan: StreamingNttPlan,
      streamingWidth: Int,
      top: String = "main",
      profile: ProfileName = ProfileName.Baseline,
      reduction: ReductionKind = ReductionKind.Barrett
  ): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    require(Set(ReductionKind.Barrett, ReductionKind.Montgomery, ReductionKind.Shoup)(reduction),
      "generic streamed RTL supports Barrett, Montgomery, or Shoup reduction")
    val domain = plan.domain
    val field = domain.modulus
    val barrett = BarrettField(field)
    val shoup = ShoupField(field)
    val width = field.bitWidth
    val generatedSchedule = schedule(plan, streamingWidth, profile)
    val inputCycles = generatedSchedule.inputCycles
    val outputCycles = generatedSchedule.outputCycles
    val bundles = generatedSchedule.bundles

    val montgomeryRadix = BigInt(1) << width
    val montgomeryQInv = (-field.q.modInverse(montgomeryRadix)).mod(montgomeryRadix)
    def encoded(value: BigInt): BigInt =
      val normalized = field.normalize(value)
      if reduction == ReductionKind.Montgomery then field.multiply(normalized, montgomeryRadix) else normalized
    def literal(value: BigInt): String = s"${width}'d${encoded(value)}"
    def shoupPrecondition(value: BigInt): BigInt = shoup.prepare(value).precondition
    def multiply(value: String, factor: BigInt): String =
      if field.normalize(factor) == 1 then value
      else if reduction == ReductionKind.Shoup then s"field_mul($value, ${literal(factor)}, ${width}'d${shoupPrecondition(factor)})"
      else s"field_mul($value, ${literal(factor)})"

    val reductionParameters = reduction match
      case ReductionKind.Barrett => s"  localparam [${2 * width - 1}:0] BARRETT_MU = ${2 * width}'d${barrett.mu};"
      case ReductionKind.Montgomery => s"  localparam [${width - 1}:0] MONTGOMERY_QINV = ${width}'d$montgomeryQInv;"
      case ReductionKind.Shoup => "  // Shoup reciprocals are precomputed beside each constant operand."
      case _ => throw new IllegalArgumentException("unsupported generic reduction")
    val multiplyFunction = reduction match
      case ReductionKind.Barrett =>
        s"""  function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a, input [${width - 1}:0] b);
           |    reg [${2 * width - 1}:0] product;
           |    reg [${4 * width - 1}:0] scaled;
           |    reg [${2 * width - 1}:0] quotient;
           |    reg [${3 * width - 1}:0] quotient_product;
           |    reg signed [${3 * width}:0] remainder;
           |    begin
           |      product = {{$width{1'b0}}, a} * {{$width{1'b0}}, b};
           |      scaled = {{${2 * width}{1'b0}}, product} * {{${2 * width}{1'b0}}, BARRETT_MU};
           |      quotient = scaled[${4 * width - 1}:${2 * width}];
           |      quotient_product = {{$width{1'b0}}, quotient} * {{${2 * width}{1'b0}}, MODULUS};
           |      remainder = $$signed({${width + 1}'d0, product}) - $$signed({1'b0, quotient_product});
           |      if (remainder < 0) remainder = remainder + MODULUS_REMAINDER;
           |      if (remainder >= MODULUS_REMAINDER) remainder = remainder - MODULUS_REMAINDER;
           |      if (remainder >= MODULUS_REMAINDER) remainder = remainder - MODULUS_REMAINDER;
           |      field_mul = remainder[${width - 1}:0];
           |    end
           |  endfunction""".stripMargin
      case ReductionKind.Montgomery =>
        s"""  function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a, input [${width - 1}:0] b_montgomery);
           |    reg [${2 * width - 1}:0] product, correction_product, multiple;
           |    reg [${width - 1}:0] correction;
           |    reg [${2 * width}:0] sum;
           |    reg [$width:0] reduced;
           |    begin
           |      product = {{$width{1'b0}}, a} * {{$width{1'b0}}, b_montgomery};
           |      correction_product = {{$width{1'b0}}, product[${width - 1}:0]} * {{$width{1'b0}}, MONTGOMERY_QINV};
           |      correction = correction_product[${width - 1}:0];
           |      multiple = {{$width{1'b0}}, correction} * {{$width{1'b0}}, MODULUS};
           |      sum = {1'b0, product} + {1'b0, multiple};
           |      reduced = sum[${2 * width}:$width];
           |      if (reduced >= {1'b0, MODULUS}) reduced = reduced - {1'b0, MODULUS};
           |      field_mul = reduced[${width - 1}:0];
           |    end
           |  endfunction""".stripMargin
      case ReductionKind.Shoup =>
        s"""  function automatic [${width - 1}:0] field_mul(
           |      input [${width - 1}:0] a,
           |      input [${width - 1}:0] b,
           |      input [${width - 1}:0] b_shoup
           |  );
           |    reg [${2 * width - 1}:0] product, approximate_product, quotient_product;
           |    reg [${width - 1}:0] approximate_quotient;
           |    reg [${2 * width}:0] remainder;
           |    begin
           |      product = {{$width{1'b0}}, a} * {{$width{1'b0}}, b};
           |      approximate_product = {{$width{1'b0}}, a} * {{$width{1'b0}}, b_shoup};
           |      approximate_quotient = approximate_product[${2 * width - 1}:$width];
           |      quotient_product = {{$width{1'b0}}, approximate_quotient} * {{$width{1'b0}}, MODULUS};
           |      remainder = {1'b0, product} - {1'b0, quotient_product};
           |      if (remainder >= {{${width + 1}{1'b0}}, MODULUS})
           |        remainder = remainder - {{${width + 1}{1'b0}}, MODULUS};
           |      field_mul = remainder[${width - 1}:0];
           |    end
           |  endfunction""".stripMargin
      case _ => throw new IllegalArgumentException("unsupported generic reduction")

    val inputPorts = Vector.tabulate(streamingWidth)(lane => s"input [${width - 1}:0] i$lane")
    val outputPorts = Vector.tabulate(streamingWidth)(lane => s"output reg [${width - 1}:0] o$lane")
    val ports = Vector("input clock", "input reset", "input next", "output ready", "output reg next_out") ++ inputPorts ++ outputPorts

    def captureAssignments(cycle: Int): Vector[String] = Vector.tabulate(streamingWidth) { lane =>
      val streamIndex = cycle * streamingWidth + lane
      val address = plan.inputAddresses(streamIndex)
      s"work[$address] <= ${multiply(s"i$lane", plan.inputFactors(streamIndex))};"
    }
    val firstCapture = captureAssignments(0).mkString(" ")
    val captureCases = (1 until inputCycles).map { cycle =>
      s"$cycle: begin ${captureAssignments(cycle).mkString(" ")} end"
    }.mkString("\n          ")

    val bundleCases = bundles.zipWithIndex.map { case (bundle, index) =>
      val operations = bundle.flatMap { operation =>
        operation.kind match
          case ButterflyKind.DecimationInTime =>
            val product = multiply(s"work[${operation.right}]", operation.twiddle)
            Vector(
              s"work[${operation.left}] <= mod_add(work[${operation.left}], $product);",
              s"work[${operation.right}] <= mod_sub(work[${operation.left}], $product);"
            )
          case ButterflyKind.GentlemanSande =>
            val differenceProduct = multiply(s"mod_sub(work[${operation.right}], work[${operation.left}])", operation.twiddle)
            Vector(
              s"work[${operation.left}] <= mod_add(work[${operation.left}], work[${operation.right}]);",
              s"work[${operation.right}] <= $differenceProduct;"
            )
      }
      s"$index: begin ${operations.mkString(" ")} end"
    }.mkString("\n          ")

    def outputAssignments(cycle: Int): Vector[String] = Vector.tabulate(streamingWidth) { lane =>
      val streamIndex = cycle * streamingWidth + lane
      val address = plan.outputAddresses(streamIndex)
      s"o$lane <= ${multiply(s"work[$address]", plan.outputFactors(streamIndex))};"
    }
    val outputCases = (0 until outputCycles).map { cycle =>
      s"$cycle: begin ${outputAssignments(cycle).mkString(" ")} end"
    }.mkString("\n          ")
    val resetOutputs = Vector.tabulate(streamingWidth)(lane => s"o$lane <= '0;").mkString(" ")
    val afterCapture = if bundles.nonEmpty then "state <= EXECUTE; bundle_index <= 0;" else "state <= OUTPUT_DATA; output_count <= 0;"
    val afterFirstCapture =
      if inputCycles == 1 then afterCapture
      else "state <= CAPTURE; capture_count <= 1;"

    s"""// Generated by NGen's generic streamed radix-2 backend.
       |/* verilator lint_off DECLFILENAME */
       |module $top(
       |${ports.map("  " + _).mkString(",\n")}
       |);
       |  localparam integer N = ${domain.size};
       |  localparam integer STREAMING_WIDTH = $streamingWidth;
       |  localparam integer INPUT_CYCLES = $inputCycles;
       |  localparam integer OUTPUT_CYCLES = $outputCycles;
       |  localparam integer BUNDLE_COUNT = ${bundles.size};
       |  localparam integer BUNDLE_GAP = ${generatedSchedule.bundleGap};
       |  localparam [1:0] IDLE = 0, CAPTURE = 1, EXECUTE = 2, OUTPUT_DATA = 3;
       |  localparam [${width - 1}:0] MODULUS = ${width}'d${field.q};
       |  localparam [${width}:0] MODULUS_EXT = ${width + 1}'d${field.q};
       |  localparam signed [${3 * width}:0] MODULUS_REMAINDER = ${3 * width + 1}'sd${field.q};
       |$reductionParameters
       |
       |  reg [1:0] state;
       |  integer capture_count, bundle_index, output_count, gap_count;
       |  reg [${width - 1}:0] work [0:N-1];
       |  assign ready = (state == IDLE) || ((state == OUTPUT_DATA) && (output_count == OUTPUT_CYCLES - 1));
       |
       |  function automatic [${width - 1}:0] mod_add(input [${width - 1}:0] a, input [${width - 1}:0] b);
       |    reg [$width:0] sum;
       |    reg [$width:0] reduced;
       |    begin
       |      sum = {1'b0, a} + {1'b0, b};
       |      if (sum >= MODULUS_EXT) reduced = sum - MODULUS_EXT;
       |      else reduced = sum;
       |      mod_add = reduced[${width - 1}:0];
       |    end
       |  endfunction
       |
       |  function automatic [${width - 1}:0] mod_sub(input [${width - 1}:0] a, input [${width - 1}:0] b);
       |    reg [$width:0] difference;
       |    begin
       |      if (a >= b) difference = {1'b0, a} - {1'b0, b};
       |      else difference = {1'b0, a} + MODULUS_EXT - {1'b0, b};
       |      mod_sub = difference[${width - 1}:0];
       |    end
       |  endfunction
       |
       |$multiplyFunction
       |
       |  always @(posedge clock) begin
       |    if (reset) begin
       |      state <= IDLE; capture_count <= 0; bundle_index <= 0; output_count <= 0; gap_count <= 0; next_out <= 0;
       |      $resetOutputs
       |    end else begin
       |      next_out <= 0;
       |      case (state)
       |        IDLE: if (next) begin
       |          $firstCapture
       |          $afterFirstCapture
       |        end
       |        CAPTURE: begin
       |          case (capture_count)
       |          $captureCases
       |          default: begin end
       |          endcase
       |          if (capture_count == INPUT_CYCLES - 1) begin $afterCapture end
       |          else capture_count <= capture_count + 1;
       |        end
       |        EXECUTE: begin
       |          if (gap_count != 0) gap_count <= gap_count - 1;
       |          else begin
       |            case (bundle_index)
       |            $bundleCases
       |            default: begin end
       |            endcase
       |            if (bundle_index == BUNDLE_COUNT - 1) begin state <= OUTPUT_DATA; output_count <= 0; end
       |            else begin bundle_index <= bundle_index + 1; gap_count <= BUNDLE_GAP; end
       |          end
       |        end
       |        OUTPUT_DATA: begin
       |          case (output_count)
       |          $outputCases
       |          default: begin end
       |          endcase
       |          if (output_count == 0) next_out <= 1;
       |          if (output_count == OUTPUT_CYCLES - 1) begin
       |            if (next) begin $firstCapture $afterFirstCapture end
       |            else state <= IDLE;
       |          end else output_count <= output_count + 1;
       |        end
       |        default: state <= IDLE;
       |      endcase
       |    end
       |  end
       |endmodule
       |/* verilator lint_on DECLFILENAME */
       |""".stripMargin
