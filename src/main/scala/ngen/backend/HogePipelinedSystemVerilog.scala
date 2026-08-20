package ngen.backend

import ngen.arithmetic.HogeField
import ngen.rtl.{ProfileName, TransposeKind}

/** Stage-parallel radix-32 HOGE streaming backend. */
object HogePipelinedSystemVerilog:
  private final case class Stage(label: String, lines: Vector[String])

  private def hex(value: BigInt): String = f"64'h${HogeField.normalize(value)}%016x"

  private def butterfly(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.empty[String]
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$left] = hoge_add(stage_next[$left],stage_next[$right]);"
      result += s"stage_next[$right] = hoge_sub(tmp,stage_next[$right]);"
    result.toVector

  private def inverseButterfly(offset: Int, size: Int, radixLog: Int): Vector[String] =
    if radixLog == 0 then Vector.empty
    else
      val result = scala.collection.mutable.ArrayBuffer.from(butterfly(offset, size))
      val block = size >> radixLog
      for lane <- 1 until 1 << (radixLog - 1); index <- 0 until block do
        val position = offset + size / 2 + lane * block + index
        result += s"stage_next[$position] = hoge_mul(stage_next[$position],${hex(BigInt(2).modPow(3 * (lane << (6 - radixLog)), HogeField.Modulus))});"
      result ++= inverseButterfly(offset, size / 2, radixLog - 1)
      result ++= inverseButterfly(offset + size / 2, size / 2, radixLog - 1)
      result.toVector

  private def forwardButterfly(offset: Int, size: Int, radixLog: Int): Vector[String] =
    if radixLog == 0 then Vector.empty
    else
      val result = scala.collection.mutable.ArrayBuffer.from(forwardButterfly(offset + size / 2, size / 2, radixLog - 1))
      result ++= forwardButterfly(offset, size / 2, radixLog - 1)
      val block = size >> radixLog
      if radixLog != 1 then
        for lane <- 1 until 1 << (radixLog - 1); index <- 0 until block do
          val position = offset + size / 2 + lane * block + index
          result += s"stage_next[$position] = hoge_mul(stage_next[$position],${hex(BigInt(2).modPow(3 * (64 - (lane << (6 - radixLog))), HogeField.Modulus))});"
      result ++= butterfly(offset, size)
      result.toVector

  private def inverseStages(logSize: Int, radixLog: Int): Vector[Stage] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    val result = scala.collection.mutable.ArrayBuffer.empty[Stage]
    result += Stage("inverse_twist", Vector.tabulate(size)(index => s"stage_next[$index] = hoge_mul(stage_next[$index],${hex(tables.inverseTwist(index))});"))
    var sizeLog = logSize
    while sizeLog > radixLog do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      for block <- 0 until blockCount do
        val offset = block * blockSize
        lines ++= inverseButterfly(offset, blockSize, radixLog)
        val subblock = blockSize >> radixLog
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          lines += s"stage_next[$position] = hoge_mul(stage_next[$position],${hex(tables.inverse(ngen.arithmetic.HogeField.reverse(lane, radixLog) * blockCount * index))});"
      result += Stage(s"inverse_radix${1 << radixLog}_$sizeLog", lines.toVector)
      sizeLog -= radixLog
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    for block <- 0 until 1 << (logSize - radixLog) do lines ++= inverseButterfly(block << radixLog, 1 << radixLog, radixLog)
    result += Stage("inverse_radix_final", lines.toVector)
    result.toVector

  private def forwardStages(logSize: Int, radixLog: Int): Vector[Stage] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    val result = scala.collection.mutable.ArrayBuffer.empty[Stage]
    val initial = scala.collection.mutable.ArrayBuffer.empty[String]
    for block <- 0 until 1 << (logSize - radixLog) do initial ++= forwardButterfly(block << radixLog, 1 << radixLog, radixLog)
    result += Stage("forward_radix_initial", initial.toVector)
    var sizeLog = 2 * radixLog
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> radixLog
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          lines += s"stage_next[$position] = hoge_mul(stage_next[$position],${hex(tables.forward(ngen.arithmetic.HogeField.reverse(lane, radixLog) * blockCount * index))});"
        lines ++= forwardButterfly(offset, blockSize, radixLog)
      result += Stage(s"forward_radix${1 << radixLog}_$sizeLog", lines.toVector)
      sizeLog += radixLog
    val inverseSize = HogeField.inversePowerOfTwo(logSize)
    result += Stage("forward_twist_scale", Vector.tabulate(size) { index =>
      val factor = HogeField.multiply(tables.forwardTwist(index), inverseSize)
      s"stage_next[$index] = hoge_mul(stage_next[$index],${hex(factor)});"
    })
    result.toVector

  def stageCounts(logSize: Int, radixLog: Int): (Int, Int) =
    (inverseStages(logSize, radixLog).size, forwardStages(logSize, radixLog).size)

  def emit(top: String, inverse: Boolean, profile: ProfileName, transpose: TransposeKind = TransposeKind.Indexed): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    require(transpose == TransposeKind.Indexed, "stage-parallel HOGE currently requires indexed stream boundaries")
    val logSize = 10
    val radixLog = 5
    val size = 1 << logSize
    val lanes = 32
    val cycles = size / lanes
    val stages = if inverse then inverseStages(logSize, radixLog) else forwardStages(logSize, radixLog)
    def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")
    def inputCases: String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes) { lane =>
        val index = if inverse then lane * cycles + cycle else cycle * lanes + lane
        s"input$index=io_in[$lane*${if inverse then 32 else 64}+:${if inverse then 32 else 64}];"
      }
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    def outputCases: String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes)(lane => s"io_out[$lane*64+:64]<=work[${cycle * lanes + lane}];")
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    val inputWidth = if inverse then 32 else 64
    val ports = Vector("input clock", "input reset", "input io_enable") ++
      (if inverse then Vector.empty else Vector("output io_ready")) ++ Vector("output reg io_validout", s"input [${lanes * inputWidth - 1}:0] io_in", s"output reg [${lanes * 64 - 1}:0] io_out")
    val inputDeclarations = Vector.tabulate(size)(i => s"reg [${inputWidth - 1}:0] input$i").mkString("; ") + ";"
    val profileGap = if profile == ProfileName.F300 then 1 else 0
    val ready = if inverse then "" else "  assign io_ready=io_enable&&!output_active&&!executing&&!finishing;"
    val initialize = Vector.tabulate(size)(i => s"work[$i]<=${if inverse then s"{32'd0,input$i}" else s"input$i"};").mkString(" ")
    val resetWork = Vector.tabulate(size)(i => s"work[$i]<='0;").mkString(" ")
    val arithmetic =
      """
      |  localparam [63:0] HOGE_P=64'hffffffff00000001;
      |  function automatic [63:0] hoge_normalize(input [63:0] value); begin hoge_normalize=(value>=HOGE_P)?value+64'h00000000ffffffff:value; end endfunction
      |  function automatic [63:0] hoge_add(input [63:0] a,input [63:0] b); reg [64:0] sum; begin sum={1'b0,a}+{1'b0,b};hoge_add=(sum[64]||sum[63:0]>=HOGE_P)?sum[63:0]+64'h00000000ffffffff:sum[63:0]; end endfunction
      |  function automatic [63:0] hoge_sub(input [63:0] a,input [63:0] b); reg [64:0] diff; begin diff={1'b0,a}-{1'b0,b};hoge_sub=diff[64]?diff[63:0]-64'h00000000ffffffff:diff[63:0]; end endfunction
      |  function automatic [63:0] hoge_mul(input [63:0] a,input [63:0] b); reg [127:0] product;reg [31:0] t0,t1,t2,t3;reg [63:0] lo,middle,res; begin product=a*b;lo=product[63:0];t0=product[31:0];t1=product[63:32];t2=product[95:64];t3=product[127:96];middle={32'd0,t1}+t2;res=(middle<<32)+t0-t3-t2;if((res>lo)&&(t2==0))res=res-64'h00000000ffffffff;if((res<lo)&&(t2!=0))res=res+64'h00000000ffffffff;hoge_mul=hoge_normalize(res); end endfunction
      |  """.stripMargin
    val stageModules = stages.zipWithIndex.map { case (stage,index) =>
      s"""module HogePipelinedStage_$index(
         |  input [${size * 64 - 1}:0] in_bus,
         |  output reg [${size * 64 - 1}:0] out_bus
         |);
         |$arithmetic
         |  reg [63:0] stage_next [0:${size - 1}]; reg [63:0] tmp; integer j;
         |  always @(*) begin
         |    for(j=0;j<$size;j=j+1) stage_next[j]=in_bus[j*64+:64];
         |    tmp=0;
         |${lines(stage.lines,4)}
         |    for(j=0;j<$size;j=j+1) out_bus[j*64+:64]=stage_next[j];
         |  end
         |endmodule""".stripMargin
    }.mkString("\n")
    val workBus = Vector.tabulate(size)(i => s"assign work_bus[${i * 64}+:64]=work[$i];").mkString("\n  ")
    val stageBusDeclarations = stages.indices.map(index => s"wire [${size * 64 - 1}:0] stage_bus_$index;").mkString("\n  ")
    val stageInstances = stages.indices.map(index => s"HogePipelinedStage_$index stage_$index(work_bus,stage_bus_$index);").mkString("\n  ")
    val stageSelectCases = stages.indices.map(index => s"$index: selected_stage_bus=stage_bus_$index;").mkString(" ")
    val stageMux = s"always @(*) begin selected_stage_bus=work_bus; case(stage_index) $stageSelectCases default: selected_stage_bus=work_bus; endcase end"
    val outputBusDeclarations = s"wire [${size * 64 - 1}:0] work_bus; reg [${size * 64 - 1}:0] selected_stage_bus;"
    s"""// Generated by NGen's stage-parallel HOGE radix-32 backend.
       |/* verilator lint_off DECLFILENAME */
       |$stageModules
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer N=$size; localparam integer CYCLES=$cycles; localparam integer STEP_GAP=$profileGap;
       |  $inputDeclarations
       |  reg [63:0] work [0:${size - 1}]; integer j;
       |  $outputBusDeclarations
       |$workBus
       |$stageBusDeclarations
       |$stageInstances
       |  integer input_count,output_count,stage_index,stall_count; reg output_active,executing,finishing;
       |  $ready
       |  $stageMux
       |  always @(posedge clock) begin
       |    if(reset) begin input_count<=0;output_count<=0;stage_index<=0;stall_count<=0;output_active<=0;executing<=0;finishing<=0;io_validout<=0;io_out<='0;$resetWork end
       |    else begin io_validout<=0;
       |      if(executing) begin if(stall_count>0) stall_count<=stall_count-1; else begin for(j=0;j<N;j=j+1) work[j]<=selected_stage_bus[j*64+:64]; if(stage_index==${stages.size}-1) begin stage_index<=0;executing<=0;finishing<=1;end else begin stage_index<=stage_index+1;stall_count<=STEP_GAP;end end
       |      end else if(finishing) begin finishing<=0;output_active<=1;output_count<=0;
       |      end else if(output_active) begin io_validout<=1;case(output_count) $outputCases endcase if(output_count==CYCLES-1) begin output_count<=0;output_active<=0;end else output_count<=output_count+1;
       |      end else if(io_enable) begin case(input_count) $inputCases endcase if(input_count==CYCLES-1) begin input_count<=0;$initialize stage_index<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on DECLFILENAME */
       |""".stripMargin
