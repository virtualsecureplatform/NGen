package ngen.backend

/** One independently stalled output beat. Input capture remains owned by the core. */
object ElasticOutputSystemVerilog:
  def emit(core: String, top: String, coreTop: String, lanes: Int, width: Int): String =
    val ports=(0 until lanes).map(i=>s"input [${width-1}:0] i$i,output reg [${width-1}:0] o$i").mkString(",")
    val wires=(0 until lanes).map(i=>s"wire [${width-1}:0] data_$i;").mkString
    val connections=(0 until lanes).map(i=>s".i$i(i$i),.o$i(data_$i)").mkString(",")
    val capture=(0 until lanes).map(i=>s"o$i<=data_$i;").mkString
    val reset=(0 until lanes).map(i=>s"o$i<=0;").mkString
    s"""$core
       |module $top(input clock,reset,in_valid,output in_ready,output reg out_valid,input out_ready,$ports);
       |wire core_valid;wire advance=!out_valid || out_ready;$wires
       |$coreTop core(.clock(clock),.reset(reset),.in_valid(in_valid),.in_ready(in_ready),.out_valid(core_valid),.out_ready(advance),$connections);
       |always @(posedge clock)begin
       |if(reset)begin out_valid<=0;$reset end
       |else if(advance)begin out_valid<=core_valid;if(core_valid)begin $capture end end
       |end
       |endmodule
       |""".stripMargin
