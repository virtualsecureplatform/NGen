package ngen.backend

import ngen.rtl.SwitchTransposeSpec

object SwitchTransposeSystemVerilog:
  private def unitName(prefix: String, bits: Int): String = s"${prefix}NGenSwitchTransposeUnit_$bits"
  private def networkName(prefix: String, bits: Int): String = s"${prefix}NGenSwitchTransposeNetwork_$bits"

  private def unit(bits: Int, dataWidth: Int, prefix: String = "", resetData: Boolean = true): String =
    val width = 1 << bits
    val half = width / 2
    def upper(lane: Int, stage: Int) = s"upper_${lane}_$stage"
    def lower(lane: Int, stage: Int) = s"lower_${lane}_$stage"
    val registers = (for lane <- 0 until half; stage <- 0 until half yield
      s"reg [${dataWidth - 1}:0] ${upper(lane,stage)}; reg [${dataWidth - 1}:0] ${lower(lane,stage)};"
    ).mkString("\n  ")
    val outputs = (0 until half).flatMap { lane => Vector(
      s"assign data_out[${lane * dataWidth} +: $dataWidth] = ${lower(lane,half-1)};",
      s"assign data_out[${(lane + half) * dataWidth} +: $dataWidth] = select ? data_in[${lane * dataWidth} +: $dataWidth] : ${upper(lane,half-1)};"
    )}.mkString("\n  ")
    val resetPipes = (for lane <- 0 until half; stage <- 0 until half yield
      s"${upper(lane,stage)} <= '0; ${lower(lane,stage)} <= '0;"
    ).mkString("\n      ")
    val shiftPipes = (0 until half).flatMap { lane =>
      Vector(
        s"${upper(lane,0)} <= data_in[${(lane + half) * dataWidth} +: $dataWidth];",
        s"${lower(lane,0)} <= select ? ${upper(lane,half-1)} : data_in[${lane * dataWidth} +: $dataWidth];"
      ) ++ (1 until half).flatMap(stage => Vector(
        s"${upper(lane,stage)} <= ${upper(lane,stage-1)};",
        s"${lower(lane,stage)} <= ${lower(lane,stage-1)};"
      ))
    }.mkString("\n      ")
    // Validity and phase discard aborted work. HOGE can leave invalid delay
    // contents unreset to avoid distributing reset across the wide datapath.
    val dataBeforeControl = if resetData then "" else s"\n      $shiftPipes"
    val dataOnReset = if resetData then s"\n      $resetPipes" else ""
    val dataAfterReset = if resetData then s"\n      $shiftPipes" else ""
    // Each unit delays validity by half a frame, just like its data paths.
    // Track input phase independently so even a one-cycle inter-frame gap is
    // preserved while the previous frame is still draining downstream.
    val validShift = if half == 1 then "valid_delay <= valid_in;"
      else s"valid_delay <= {valid_delay[${half - 2}:0], valid_in};"
    s"""module ${unitName(prefix, bits)}(
       |  input clock, input reset, input valid_in,
       |  input [${width * dataWidth - 1}:0] data_in,
       |  output valid_out, output [${width * dataWidth - 1}:0] data_out
       |);
       |  reg [${bits - 1}:0] phase;
       |  reg [${half - 1}:0] valid_delay;
       |  wire select = phase[${bits - 1}];
       |  assign valid_out = valid_delay[${half - 1}];
       |  $registers
       |  $outputs
       |  always @(posedge clock) begin$dataBeforeControl
       |    if (reset) begin phase <= '0; valid_delay <= '0;$dataOnReset
       |    end else begin$dataAfterReset
       |      if (valid_in) phase <= phase + 1'b1;
       |      else phase <= '0;
       |      $validShift
       |    end
       |  end
       |endmodule
       |""".stripMargin

  private def network(bits: Int, dataWidth: Int, prefix: String = ""): String =
    val width = 1 << bits
    if bits == 1 then
      s"""module ${networkName(prefix, 1)}(input clock,input reset,input valid_in,input [${2 * dataWidth - 1}:0] data_in,output valid_out,output [${2 * dataWidth - 1}:0] data_out);
         |  ${unitName(prefix, 1)} unit(clock,reset,valid_in,data_in,valid_out,data_out);
         |endmodule
         |""".stripMargin

    else
      val halfBits = width * dataWidth / 2
      s"""module ${networkName(prefix, bits)}(input clock,input reset,input valid_in,input [${width * dataWidth - 1}:0] data_in,output valid_out,output [${width * dataWidth - 1}:0] data_out);
         |  wire unit_valid; wire [${width * dataWidth - 1}:0] unit_data; wire lower_valid, upper_valid;
         |  ${unitName(prefix, bits)} unit(clock,reset,valid_in,data_in,unit_valid,unit_data);
         |  ${networkName(prefix, bits - 1)} lower(clock,reset,unit_valid,unit_data[0 +: $halfBits],lower_valid,data_out[0 +: $halfBits]);
         |  ${networkName(prefix, bits - 1)} upper(clock,reset,unit_valid,unit_data[$halfBits +: $halfBits],upper_valid,data_out[$halfBits +: $halfBits]);
         |  assign valid_out = lower_valid & upper_valid;
         |endmodule
         |""".stripMargin

  def definitions(spec: SwitchTransposeSpec, prefix: String = "", resetData: Boolean = true): String =
    require(prefix.matches("[A-Za-z_][A-Za-z0-9_]*") || prefix.isEmpty, s"invalid switch-transpose prefix '$prefix'")
    require(spec.square, "recursive switch-unit definitions require a square lane/time sub-network")
    (1 to spec.logSize).map(bits => unit(bits,spec.dataWidth,prefix,resetData) + network(bits,spec.dataWidth,prefix)).mkString("\n")

  private def rectangular(spec: SwitchTransposeSpec, top: String, fixedRate: Boolean, ratePreserving: Boolean): String =
    val inBits = spec.inputLanes * spec.dataWidth
    val externalLanes = if ratePreserving then spec.inputLanes else spec.outputLanes
    val externalCycles = if ratePreserving then spec.inputCycles else spec.outputCycles
    val outBits = externalLanes * spec.dataWidth
    val outputAssignments = (buffer: Int) => Vector.tabulate(spec.outputLanes) { lane =>
      s"data_out[$lane*${spec.dataWidth}+:${spec.dataWidth}]<=storage_$buffer[$lane*${spec.inputLanes}+output_count];"
    }.mkString(" ")
    val ratePreservingAssignments = (buffer: Int) => Vector.tabulate(externalLanes) { lane =>
      val linear = s"(output_count*${spec.inputLanes}+$lane)"
      s"data_out[$lane*${spec.dataWidth}+:${spec.dataWidth}]<=storage_$buffer[($linear%${spec.inputCycles})*${spec.inputLanes}+($linear/${spec.inputCycles})];"
    }.mkString(" ")
    val captureAssignments = (buffer: Int) => Vector.tabulate(spec.inputLanes) { lane =>
      s"storage_$buffer[input_count*${spec.inputLanes}+$lane]<=data_in[$lane*${spec.dataWidth}+:${spec.dataWidth}];"
    }.mkString(" ")
    val readyPort = if fixedRate then "" else ",output input_ready"
    val readyAssignment = if fixedRate then "wire input_ready=capture_active||(buffer_state[0]==FREE)||(buffer_state[1]==FREE);" else "assign input_ready=capture_active||(buffer_state[0]==FREE)||(buffer_state[1]==FREE);"
    val outputCycleCount = if ratePreserving then spec.inputCycles else spec.outputCycles
    val frameInterval = if ratePreserving then spec.inputCycles else math.max(spec.inputCycles,spec.outputCycles)
    val minimumGap = if ratePreserving then 0 else math.max(0,spec.outputCycles-spec.inputCycles)
    val outputMode = if ratePreserving then "1" else "0"
    val output0 = if ratePreserving then ratePreservingAssignments(0) else outputAssignments(0)
    val output1 = if ratePreserving then ratePreservingAssignments(1) else outputAssignments(1)
    val modeDescription = if ratePreserving then s"Rate-preserving mode: externally ${spec.inputCycles}x${spec.inputLanes}; transposed rows are packed/split."
      else if fixedRate then s"Fixed-rate mode: start frames at least ${math.max(spec.inputCycles,spec.outputCycles)} cycles apart."
      else "Ready mode: input_ready applies rate backpressure."
    s"""// Generated by NGen's rectangular SwitchTranspose adapter.
       |// It exchanges cycle and lane coordinates: ${spec.inputCycles}x${spec.inputLanes} -> ${spec.outputCycles}x${spec.outputLanes}.
       |// $modeDescription
       |module $top(input clock,input reset,input valid_in$readyPort,input [$inBits-1:0] data_in,output reg valid_out,output reg [$outBits-1:0] data_out);
       |  localparam integer INPUT_CYCLES=${spec.inputCycles},INPUT_LANES=${spec.inputLanes},OUTPUT_CYCLES=$externalCycles,OUTPUT_LANES=$externalLanes,FRAME_INTERVAL=$frameInterval,MIN_FRAME_GAP=$minimumGap;
       |  localparam [1:0] FREE=0,CAPTURE=1,QUEUED=2,OUTPUT=3;
       |  reg [1:0] buffer_state [0:1];reg capture_active,capture_buffer,output_active,output_buffer;reg [${spec.dataWidth - 1}:0] storage_0 [0:${spec.inputCycles * spec.inputLanes - 1}],storage_1 [0:${spec.inputCycles * spec.inputLanes - 1}];integer input_count,output_count,lane;
       |  $readyAssignment
       |  always @(posedge clock) begin
       |    if(reset) begin input_count<=0;output_count<=0;capture_active<=0;output_active<=0;capture_buffer<=0;output_buffer<=0;buffer_state[0]<=FREE;buffer_state[1]<=FREE;valid_out<=0;data_out<='0;for(lane=0;lane<INPUT_CYCLES*INPUT_LANES;lane=lane+1)begin storage_0[lane]<='0;storage_1[lane]<='0;end end
       |    else begin
       |      valid_out<=0;
       |      if(valid_in&&input_ready) begin
       |        if(!capture_active) begin
       |          if(buffer_state[0]==FREE) begin ${captureAssignments(0)} buffer_state[0]<=CAPTURE;capture_buffer<=0;end
       |          else begin ${captureAssignments(1)} buffer_state[1]<=CAPTURE;capture_buffer<=1;end
       |          capture_active<=1;
       |        end else if(capture_buffer==0) begin ${captureAssignments(0)} end else begin ${captureAssignments(1)} end
       |        if(input_count==INPUT_CYCLES-1) begin if(capture_active?capture_buffer:(buffer_state[0]==FREE?0:1)) buffer_state[1]<=QUEUED;else buffer_state[0]<=QUEUED;capture_active<=0;input_count<=0;end else input_count<=input_count+1;
       |      end
       |      if(!output_active) begin
       |        if(buffer_state[0]==QUEUED) begin buffer_state[0]<=OUTPUT;output_buffer<=0;output_active<=1;output_count<=1;valid_out<=1;$output0 if(OUTPUT_CYCLES==1)begin buffer_state[0]<=FREE;output_active<=0;output_count<=0;end end
       |        else if(buffer_state[1]==QUEUED) begin buffer_state[1]<=OUTPUT;output_buffer<=1;output_active<=1;output_count<=1;valid_out<=1;$output1 if(OUTPUT_CYCLES==1)begin buffer_state[1]<=FREE;output_active<=0;output_count<=0;end end
       |      end else begin
       |        valid_out<=1;if(output_buffer==0) begin $output0 end else begin $output1 end
       |        if(output_count==OUTPUT_CYCLES-1) begin if(output_buffer==0)buffer_state[0]<=FREE;else buffer_state[1]<=FREE;output_active<=0;output_count<=0;end else output_count<=output_count+1;
       |      end
       |    end
       |  end
       |endmodule
       |""".stripMargin

  def emit(spec: SwitchTransposeSpec, top: String = "SwitchTransposeTop", fixedRate: Boolean = false, ratePreserving: Boolean = false): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid module name: $top")
    if !spec.square then rectangular(spec, top, fixedRate, ratePreserving)
    else
      val dependencies = definitions(spec)
      s"""// Generated by NGen from the HOGE SwitchTransposeUnit architecture.
         |$dependencies
         |module $top(input clock,input reset,input valid_in,input [${spec.size * spec.dataWidth - 1}:0] data_in,output valid_out,output [${spec.size * spec.dataWidth - 1}:0] data_out);
         |  NGenSwitchTransposeNetwork_${spec.logSize} network(clock,reset,valid_in,data_in,valid_out,data_out);
         |endmodule
         |""".stripMargin
