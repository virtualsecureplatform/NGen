package ngen.backend

import ngen.rtl.SwitchTransposeSpec

object GenericSwitchTransposeWrapper:
  def emit(coreRtl: String, top: String, coreTop: String, lanes: Int, dataWidth: Int, transformSize: Int): String =
    require(lanes > 1 && Integer.bitCount(lanes) == 1)
    require(transformSize >= lanes && transformSize % lanes == 0)
    if lanes * lanes != transformSize then
      return rectangular(coreRtl, top, coreTop, lanes, dataWidth, transformSize)
    val bits = Integer.numberOfTrailingZeros(lanes)
    val spec = SwitchTransposeSpec(bits, dataWidth)
    val inputPorts = Vector.tabulate(lanes)(lane => s"input [${dataWidth - 1}:0] i$lane")
    val outputPorts = Vector.tabulate(lanes)(lane => s"output [${dataWidth - 1}:0] o$lane")
    val packInputs = Vector.tabulate(lanes)(lane => s"assign input_packed[${lane * dataWidth}+:$dataWidth]=i$lane;")
    val unpackCoreInputs = Vector.tabulate(lanes)(lane => s"wire [${dataWidth - 1}:0] core_i$lane=input_transposed[${lane * dataWidth}+:$dataWidth];")
    val packCoreOutputs = Vector.tabulate(lanes)(lane => s"wire [${dataWidth - 1}:0] core_o$lane;assign core_output_packed[${lane * dataWidth}+:$dataWidth]=core_o$lane;")
    val unpackOutputs = Vector.tabulate(lanes)(lane => s"assign o$lane=output_transposed[${lane * dataWidth}+:$dataWidth];")
    val coreConnections = Vector(".clock(clock)", ".reset(reset)", ".next(core_next)", ".ready(core_ready)", ".next_out(core_next_out)") ++
      Vector.tabulate(lanes)(lane => s".i$lane(core_i$lane)") ++ Vector.tabulate(lanes)(lane => s".o$lane(core_o$lane)")
    s"""$coreRtl
       |${SwitchTransposeSystemVerilog.definitions(spec)}
       |module $top(
       |  input clock,input reset,input next,output ready,output next_out,
       |${(inputPorts ++ outputPorts).map("  "+_).mkString(",\n")}
       |);
       |  wire [${lanes * dataWidth - 1}:0] input_packed,input_transposed,core_output_packed,output_transposed;
       |  wire input_switch_valid,core_ready,core_next_out,output_switch_valid;
       |  reg input_active,output_active;integer input_count,core_input_count,output_feed_count,output_count;
       |  wire input_feed_valid=(next&&ready)||input_active;
       |  wire core_next=input_switch_valid&&(core_input_count==0);
       |  wire output_feed_valid=core_next_out||output_active;
       |  assign ready=core_ready&&!input_active;
       |  assign next_out=output_switch_valid&&(output_count==0);
       |  ${packInputs.mkString("\n  ")}
       |  ${unpackCoreInputs.mkString("\n  ")}
       |  ${packCoreOutputs.mkString("\n  ")}
       |  ${unpackOutputs.mkString("\n  ")}
       |  NGenSwitchTransposeNetwork_$bits input_switch(clock,reset,input_feed_valid,input_packed,input_switch_valid,input_transposed);
       |  $coreTop core(${coreConnections.mkString(",")});
       |  NGenSwitchTransposeNetwork_$bits output_switch(clock,reset,output_feed_valid,core_output_packed,output_switch_valid,output_transposed);
       |  always @(posedge clock)begin
       |    if(reset)begin input_active<=0;output_active<=0;input_count<=0;core_input_count<=0;output_feed_count<=0;output_count<=0;end
       |    else begin
       |      if(next&&ready)begin input_active<=${if lanes == 1 then 0 else 1};input_count<=1;end
       |      else if(input_active)begin if(input_count==$lanes-1)begin input_active<=0;input_count<=0;end else input_count<=input_count+1;end
       |      if(input_switch_valid)begin if(core_input_count==$lanes-1)core_input_count<=0;else core_input_count<=core_input_count+1;end else core_input_count<=0;
       |      if(core_next_out)begin output_active<=${if lanes == 1 then 0 else 1};output_feed_count<=1;end
       |      else if(output_active)begin if(output_feed_count==$lanes-1)begin output_active<=0;output_feed_count<=0;end else output_feed_count<=output_feed_count+1;end
       |      if(output_switch_valid)begin if(output_count==$lanes-1)output_count<=0;else output_count<=output_count+1;end else output_count<=0;
       |    end
       |  end
       |endmodule
       |""".stripMargin

  private def rectangular(coreRtl: String, top: String, coreTop: String,
                          lanes: Int, dataWidth: Int, transformSize: Int): String =
    val cycles = transformSize / lanes
    val spec = SwitchTransposeSpec(Integer.numberOfTrailingZeros(cycles),
                                   Integer.numberOfTrailingZeros(lanes), dataWidth)
    val inputAdapter = s"${top}InputTranspose"
    val outputAdapter = s"${top}OutputTranspose"
    val inputPorts = Vector.tabulate(lanes)(lane => s"input [${dataWidth - 1}:0] i$lane")
    val outputPorts = Vector.tabulate(lanes)(lane => s"output [${dataWidth - 1}:0] o$lane")
    val packInputs = Vector.tabulate(lanes)(lane => s"assign input_packed[${lane * dataWidth}+:$dataWidth]=i$lane;")
    val unpackCoreInputs = Vector.tabulate(lanes)(lane => s"wire [${dataWidth - 1}:0] core_i$lane=input_transposed[${lane * dataWidth}+:$dataWidth];")
    val packCoreOutputs = Vector.tabulate(lanes)(lane => s"wire [${dataWidth - 1}:0] core_o$lane;assign core_output_packed[${lane * dataWidth}+:$dataWidth]=core_o$lane;")
    val unpackOutputs = Vector.tabulate(lanes)(lane => s"assign o$lane=output_transposed[${lane * dataWidth}+:$dataWidth];")
    val coreConnections = Vector(".clock(clock)", ".reset(reset)", ".next(core_next)",
      ".ready(core_ready)", ".next_out(core_next_out)") ++
      Vector.tabulate(lanes)(lane => s".i$lane(core_i$lane)") ++
      Vector.tabulate(lanes)(lane => s".o$lane(core_o$lane)")
    s"""$coreRtl
       |${SwitchTransposeSystemVerilog.emit(spec, inputAdapter, ratePreserving = true)}
       |${SwitchTransposeSystemVerilog.emit(spec, outputAdapter, ratePreserving = true)}
       |module $top(
       |  input clock,input reset,input next,output ready,output next_out,
       |${(inputPorts ++ outputPorts).map("  "+_).mkString(",\n")}
       |);
       |  wire [${lanes * dataWidth - 1}:0] input_packed,input_transposed,core_output_packed,output_transposed;
       |  wire input_adapter_ready,input_switch_valid,core_ready,core_next_out;
       |  wire output_adapter_ready,output_switch_valid;
       |  reg busy,input_active,output_feed_active;
       |  integer input_count,core_input_count,output_feed_count,output_count;
       |  wire input_feed_valid=(next&&ready)||input_active;
       |  wire core_next=input_switch_valid&&(core_input_count==0);
       |  wire output_feed_valid=core_next_out||output_feed_active;
       |  assign ready=!busy&&core_ready&&input_adapter_ready;
       |  assign next_out=output_switch_valid&&(output_count==0);
       |  ${packInputs.mkString("\n  ")}
       |  ${unpackCoreInputs.mkString("\n  ")}
       |  ${packCoreOutputs.mkString("\n  ")}
       |  ${unpackOutputs.mkString("\n  ")}
       |  $inputAdapter input_switch(.clock(clock),.reset(reset),.valid_in(input_feed_valid),
       |    .input_ready(input_adapter_ready),.data_in(input_packed),
       |    .valid_out(input_switch_valid),.data_out(input_transposed));
       |  $coreTop core(${coreConnections.mkString(",")});
       |  $outputAdapter output_switch(.clock(clock),.reset(reset),.valid_in(output_feed_valid),
       |    .input_ready(output_adapter_ready),.data_in(core_output_packed),
       |    .valid_out(output_switch_valid),.data_out(output_transposed));
       |  always @(posedge clock) begin
       |    if(reset) begin busy<=0;input_active<=0;output_feed_active<=0;
       |      input_count<=0;core_input_count<=0;output_feed_count<=0;output_count<=0;end
       |    else begin
       |      if(next&&ready) begin busy<=1;input_active<=${if cycles == 1 then 0 else 1};input_count<=1;end
       |      else if(input_active) begin if(input_count==$cycles-1) begin input_active<=0;input_count<=0;end
       |        else input_count<=input_count+1;end
       |      if(input_switch_valid) begin if(core_input_count==$cycles-1) core_input_count<=0;
       |        else core_input_count<=core_input_count+1;end
       |      if(core_next_out) begin output_feed_active<=${if cycles == 1 then 0 else 1};output_feed_count<=1;end
       |      else if(output_feed_active) begin if(output_feed_count==$cycles-1) begin output_feed_active<=0;output_feed_count<=0;end
       |        else output_feed_count<=output_feed_count+1;end
       |      if(output_switch_valid) begin if(output_count==$cycles-1) begin output_count<=0;busy<=0;end
       |        else output_count<=output_count+1;end
       |    end
       |  end
       |endmodule
       |""".stripMargin
