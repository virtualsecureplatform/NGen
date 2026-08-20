package ngen.backend

/** AXI4-Stream shell for a ready/valid coefficient-vector core. */
object Axi4StreamWrapper:
  def emit(coreRtl:String,top:String,coreTop:String,lanes:Int,width:Int,cycles:Int):String=
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*")&&coreTop.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    require(lanes>0&&width>0&&cycles>0)
    val bits=lanes*width
    val inputs=Vector.tabulate(lanes)(i=>s".i$i(s_axis_tdata[${i*width}+:$width])")
    val outputs=Vector.tabulate(lanes)(i=>s".o$i(m_axis_tdata[${i*width}+:$width])")
    val connections=(Vector(".clock(aclk)",".reset(!aresetn)",".in_valid(s_axis_tvalid)",".in_ready(s_axis_tready)",".out_valid(core_out_valid)",".out_ready(m_axis_tready)")++inputs++outputs).mkString(",")
    s"""$coreRtl
       |module $top(
       |  input aclk,input aresetn,
       |  input [$bits-1:0] s_axis_tdata,input s_axis_tvalid,output s_axis_tready,input s_axis_tlast,
       |  output [$bits-1:0] m_axis_tdata,output m_axis_tvalid,input m_axis_tready,output m_axis_tlast
       |);
       |  localparam integer STREAM_CYCLES=$cycles;wire core_out_valid;integer input_count,output_count;
       |  assign m_axis_tvalid=core_out_valid;assign m_axis_tlast=core_out_valid&&(output_count==STREAM_CYCLES-1);
       |  $coreTop core($connections);
       |  always @(posedge aclk)begin
       |    if(!aresetn)begin input_count<=0;output_count<=0;end
       |    else begin
       |      if(s_axis_tvalid&&s_axis_tready)begin assert(s_axis_tlast==(input_count==STREAM_CYCLES-1));if(input_count==STREAM_CYCLES-1)input_count<=0;else input_count<=input_count+1;end
       |      if(m_axis_tvalid&&m_axis_tready)begin if(output_count==STREAM_CYCLES-1)output_count<=0;else output_count<=output_count+1;end
       |    end
       |  end
       |endmodule
       |""".stripMargin
