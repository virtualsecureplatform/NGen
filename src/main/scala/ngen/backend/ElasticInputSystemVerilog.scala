package ngen.backend

/** Register an input beat before core capture, retaining it until the core accepts it. */
object ElasticInputSystemVerilog:
  def emit(core: String, top: String, coreTop: String, lanes: Int, width: Int): String =
    val ports=(0 until lanes).map(i=>s"input [${width-1}:0] i$i,output [${width-1}:0] o$i").mkString(",")
    val registers=(0 until lanes).map(i=>s"reg [${width-1}:0] data_$i;").mkString
    val connections=(0 until lanes).map(i=>s".i$i(data_$i),.o$i(o$i)").mkString(",")
    val capture=(0 until lanes).map(i=>s"data_$i<=i$i;").mkString
    s"""$core
       |module $top(input clock,reset,in_valid,output in_ready,out_valid,input out_ready,$ports);
       |reg input_valid;wire core_ready;wire advance=!input_valid || core_ready;$registers
       |assign in_ready=advance;
       |$coreTop core(.clock(clock),.reset(reset),.in_valid(input_valid),.in_ready(core_ready),.out_valid(out_valid),.out_ready(out_ready),$connections);
       |always @(posedge clock)begin
       |if(reset)input_valid<=0;
       |else if(advance)begin input_valid<=in_valid;if(in_valid)begin $capture end end
       |end
       |endmodule
       |""".stripMargin
