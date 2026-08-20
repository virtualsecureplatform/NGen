package ngen.backend

import ngen.arithmetic.{YataField, YataTables}
import ngen.rtl.{ProfileName, TransposeKind}

/**
  * Stage-parallel YATA backend.
  *
  * The original microcoded backend emits one state update per scheduled
  * operation.  That is useful as a reference, but leaves the complete
  * radix-8 schedule serialized.  This backend keeps the same signed residue
  * arithmetic and external protocol while grouping all independent blocks of
  * a radix stage behind one registered stage boundary.
  */
object YataPipelinedSystemVerilog:
  private[backend] final case class Stage(label: String, lines: Vector[String])

  private def reverse3(value: Int): Int = ((value & 1) << 2) | (value & 2) | ((value & 4) >> 2)
  private def lit(value: Long): String = if value < 0 then s"27'sd${-value}" else s"27'sd$value"

  private def pair(lines: scala.collection.mutable.ArrayBuffer[String], left: Int, right: Int, reduced: Boolean): Unit =
    lines += s"tmp = stage_next[$left];"
    lines += s"stage_next[$left] = yata_add(stage_next[$left], stage_next[$right]);"
    if reduced then lines += s"stage_next[$right] = yata_sub(tmp, stage_next[$right]);"
    else lines += s"stage_next[$right] = tmp - stage_next[$right];"

  private def bothMod(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.empty[String]
    for index <- 0 until size / 2 do pair(result, offset + index, offset + index + size / 2, reduced = true)
    result.toVector

  private def addAdd(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.empty[String]
    for index <- 0 until size / 2 do pair(result, offset + index, offset + index + size / 2, reduced = false)
    result.toVector

  private def bothSredc(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.empty[String]
    for index <- 0 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$left] = yata_sredc(stage_next[$left] + stage_next[$right]);"
      result += s"stage_next[$right] = yata_sredc(tmp - stage_next[$right]);"
    result.toVector

  private def cmul(index: Int, radix: Int, number: Int): String =
    s"stage_next[$index] = yata_cmul(stage_next[$index], 2'd$radix, 2'd$number);"

  private def inverseRadix4(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.from(addAdd(offset, size))
    result ++= bothMod(offset, size / 2)
    val block = size >> 2
    for index <- 0 until block do result += cmul(offset + index + size / 2 + block, 2, 1)
    result ++= bothSredc(offset + size / 2, size / 2)
    result.toVector

  private def inverseRadix8(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.from(addAdd(offset, size))
    result ++= inverseRadix4(offset, size / 2)
    val block = size >> 3
    for index <- 0 until block do
      val position = offset + size / 2 + 2 * block + index
      val left = offset + size / 2 + index
      result += cmul(position, 3, 2)
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$left] = stage_next[$left] + stage_next[$position];"
      result += s"stage_next[$position] = tmp - stage_next[$position];"
    for index <- 0 until block do
      val left = offset + size / 2 + block + index
      val right = offset + size / 2 + 3 * block + index
      result += s"tmp = yata_cmul(stage_next[$left], 2'd3, 2'd3);"
      result += s"tmp2 = yata_cmul(stage_next[$left], 2'd3, 2'd1);"
      result += s"stage_next[$left] = tmp2 + yata_cmul(stage_next[$right], 2'd3, 2'd3);"
      result += s"stage_next[$right] = tmp + yata_cmul(stage_next[$right], 2'd3, 2'd1);"
    result ++= bothSredc(offset + size / 2, size / 4)
    result ++= bothSredc(offset + 3 * size / 4, size / 4)
    result.toVector

  private def forwardRadix4(offset: Int, size: Int, reduce: Boolean): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.from(bothMod(offset, size / 2))
    result ++= bothMod(offset + size / 2, size / 2)
    for index <- 0 until size / 4 do pair(result, offset + index, offset + index + size / 2, reduced = true)
    for index <- size / 4 until size / 2 do
      val left = offset + index
      val right = left + size / 2
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$right] = -yata_cmul(stage_next[$right], 2'd2, 2'd1);"
      if reduce then
        result += s"stage_next[$left] = yata_sredc(stage_next[$left] + stage_next[$right]);"
        result += s"stage_next[$right] = yata_sredc(tmp - stage_next[$right]);"
      else
        result += s"stage_next[$left] = stage_next[$left] + stage_next[$right];"
        result += s"stage_next[$right] = tmp - stage_next[$right];"
    result.toVector

  private def forwardRadix8(offset: Int, size: Int): Vector[String] =
    val result = scala.collection.mutable.ArrayBuffer.from(forwardRadix4(offset, size / 2, reduce = false))
    result ++= bothMod(offset + size / 2, size / 4)
    result ++= bothMod(offset + 3 * size / 4, size / 4)
    val block = size >> 3
    for index <- 0 until block do
      val left = offset + size / 2 + index
      val right = left + size / 4
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$left] = yata_add(stage_next[$left], stage_next[$right]);"
      result += s"stage_next[$right] = -yata_cmul(tmp - stage_next[$right], 2'd3, 2'd2);"
    for index <- 0 until block do
      val left = offset + size / 2 + block + index
      val right = offset + size / 2 + 3 * block + index
      result += s"tmp = -yata_cmul(stage_next[$left], 2'd3, 2'd1);"
      result += s"tmp2 = stage_next[$right];"
      result += s"stage_next[$left] = -yata_cmul(stage_next[$left], 2'd3, 2'd3) - yata_cmul(stage_next[$right], 2'd3, 2'd1);"
      result += s"stage_next[$right] = tmp - yata_cmul(tmp2, 2'd3, 2'd3);"
    for index <- 0 until block do pair(result, offset + index, offset + size / 2 + index, reduced = true)
    for group <- 1 until 4; index <- group * block until (group + 1) * block do
      val left = offset + index
      val right = left + size / 2
      result += s"tmp = stage_next[$left];"
      result += s"stage_next[$left] = yata_sredc(stage_next[$left] + stage_next[$right]);"
      result += s"stage_next[$right] = yata_sredc(tmp - stage_next[$right]);"
    result.toVector

  private[backend] def inverseStages(logSize: Int, tables: YataTables): Vector[Stage] =
    val size = 1 << logSize
    val result = scala.collection.mutable.ArrayBuffer.empty[Stage]
    result += Stage("inverse_twist", Vector.tabulate(size)(index => s"stage_next[$index] = yata_mulredc(work[$index][26:0], ${lit(tables.inttTwist(index))});"))
    var sizeLog = logSize
    while sizeLog > 3 do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      for block <- 0 until blockCount do
        val offset = block * blockSize
        lines ++= inverseRadix8(offset, blockSize)
        val subblock = blockSize >> 3
        for lane <- 1 until 8; index <- 0 until subblock do
          val position = offset + lane * subblock + index
          val table = if lane > 1 then tables.inttTable1 else tables.inttTable0
          lines += s"stage_next[$position] = yata_mulredc(stage_next[$position][26:0], ${lit(table(reverse3(lane) * blockCount * index))});"
      result += Stage(s"inverse_radix8_$sizeLog", lines.toVector)
      sizeLog -= 3
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]
    for block <- 0 until 1 << (logSize - 3) do lines ++= inverseRadix8(block * 8, 8)
    result += Stage("inverse_radix8_final", lines.toVector)
    result.toVector

  private[backend] def forwardStages(logSize: Int, tables: YataTables): Vector[Stage] =
    val size = 1 << logSize
    val result = scala.collection.mutable.ArrayBuffer.empty[Stage]
    val initial = scala.collection.mutable.ArrayBuffer.empty[String]
    for block <- 0 until 1 << (logSize - 3) do initial ++= forwardRadix8(block * 8, 8)
    result += Stage("forward_radix8_initial", initial.toVector)
    var sizeLog = 6
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> 3
      val lines = scala.collection.mutable.ArrayBuffer.empty[String]
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 0 until 8; index <- 0 until subblock do
          val tableOne = ((index >> (sizeLog - 6)) & 3) != 0
          val stride = reverse3(lane) * blockCount
          if stride == 0 then
            if tableOne then lines += s"stage_next[${offset + lane * subblock + index}] = yata_mulredc(stage_next[${offset + lane * subblock + index}][26:0], ${lit(YataField.R2)});"
          else
            val table = if tableOne then tables.nttTable1 else tables.nttTable0
            val position = offset + lane * subblock + index
            lines += s"stage_next[$position] = yata_mulredc(stage_next[$position][26:0], ${lit(table(stride * index))});"
        lines ++= forwardRadix8(offset, blockSize)
      result += Stage(s"forward_radix8_$sizeLog", lines.toVector)
      sizeLog += 3
    result += Stage("forward_twist", Vector.tabulate(size)(index => s"stage_next[$index] = yata_mulredc(stage_next[$index][26:0], ${lit(tables.nttTwist(index))});"))
    result.toVector

  def stageCounts(logSize: Int): (Int, Int) =
    val tables = YataField.tables(logSize)
    (inverseStages(logSize, tables).size, forwardStages(logSize, tables).size)

  def emit(logSize: Int, streamingLog: Int, profile: ProfileName, top: String, transpose: TransposeKind = TransposeKind.Indexed): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid module name: $top")
    require((logSize == 3 && streamingLog == 3) || (logSize == 6 && streamingLog == 3) || (logSize == 9 && streamingLog == 6))
    require(transpose == TransposeKind.Indexed, "stage-parallel YATA currently requires indexed stream boundaries")
    val size = 1 << logSize
    val lanes = 1 << streamingLog
    val cycles = size / lanes
    val tables = YataField.tables(logSize)
    val inverse = inverseStages(logSize, tables)
    val forward = forwardStages(logSize, tables)
    def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")
    def inputCases(isIntt: Boolean): String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes) { lane =>
        val index = if isIntt then lane * cycles + cycle else cycle * lanes + lane
        s"${if isIntt then s"intt$index" else s"ntt$index"}=${if isIntt then s"io_intt_in_$lane" else s"io_ntt_in_$lane"};"
      }
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    def outputCases(isIntt: Boolean): String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes) { lane =>
        val index = if isIntt then cycle * lanes + lane else lane * cycles + cycle
        if isIntt then s"io_intt_out_$lane<=work[$index][26:0];" else s"io_ntt_out_$lane<=torus$index;"
      }
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    def stageCases(stages: Vector[Stage]): String = stages.zipWithIndex.map { case (stage,index) =>
      s"$index: begin\n${lines(stage.lines,12)}\n        end"
    }.mkString("\n")
    val inputDeclarations = Vector.tabulate(size)(i => s"reg [31:0] intt$i; reg signed [26:0] ntt$i;")
    val workArrayDeclarations = s"reg signed [53:0] work [0:${size - 1}]; reg signed [53:0] stage_next [0:${size - 1}]; reg signed [53:0] tmp,tmp2; integer j;"
    val initializeI = Vector.tabulate(size)(i => s"work[$i]<={{27{1'b0}},intt$i[26:0]};")
    val initializeN = Vector.tabulate(size)(i => s"work[$i]<={{27{ntt$i[26]}},ntt$i};")
    val resetWork = Vector.tabulate(size)(i => s"work[$i]<='0;").mkString(" ")
    val stageLogic =
      s"always @(*) begin for(j=0;j<$size;j=j+1) stage_next[j]=work[j]; tmp=0; tmp2=0; if(inverse_operation) begin case(stage_index) ${stageCases(inverse)} default: begin end endcase end else begin case(stage_index) ${stageCases(forward)} default: begin end endcase end end"
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++ Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output reg [26:0] io_intt_out_$i") ++ Vector("output reg io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++ Vector.tabulate(lanes)(i => s"output reg [31:0] io_ntt_out_$i") ++ Vector("output reg io_ntt_validout")
    val profileGap = if profile == ProfileName.F300 then 1 else 0
    val arithmetic =
      """
      |  localparam signed [53:0] P=54'sd40960001;
      |  function automatic signed [26:0] yata_add(input signed [53:0] x,input signed [53:0] y); reg signed [53:0] v; begin v=x+y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;yata_add=v[26:0];end endfunction
      |  function automatic signed [26:0] yata_sub(input signed [53:0] x,input signed [53:0] y); reg signed [53:0] v; begin v=x-y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;yata_sub=v[26:0];end endfunction
      |  function automatic signed [26:0] yata_sredc(input signed [53:0] x); reg[26:0] a0;reg signed[26:0] a1,m,t1;reg signed[53:0] mw,tw; begin a0=x[26:0];a1=x[53:27];mw=-(({27'd0,a0}*625)<<<16)+{27'd0,a0};m=mw[26:0];tw=(($signed(m)*625)<<<16)+$signed(m);t1=tw[53:27];yata_sredc=a1-t1;end endfunction
      |  function automatic signed [26:0] yata_mulredc(input signed [26:0] x,input signed [26:0] y); begin yata_mulredc=yata_sredc($signed(x)*$signed(y));end endfunction
      |  function automatic signed [53:0] yata_cmul(input signed [53:0] x,input[1:0] r,input[1:0] n); begin if(r==2&&n==1)yata_cmul=(x*25)<<<8;else if(r==3&&n==1)yata_cmul=(x*5)<<<4;else if(r==3&&n==2)yata_cmul=(x*25)<<<8;else if(r==3&&n==3)yata_cmul=(x*125)<<<12;else yata_cmul=x;end endfunction
      |  """.stripMargin
    val modswitch = Vector.tabulate(size)(i => s"wire [31:0] torus$i;").mkString("\n  ")
    val modswitchLogic = Vector.tabulate(size)(i => s"YataPipelinedModSwitch modswitch_$i(work[$i],torus$i);").mkString("\n  ")
    s"""// Generated by NGen's stage-parallel YATA radix-8 backend.
       |/* verilator lint_off DECLFILENAME */
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer N=$size; localparam integer LANES=$lanes; localparam integer CYCLES=$cycles; localparam integer I_LENGTH=${inverse.size}; localparam integer F_LENGTH=${forward.size}; localparam integer STEP_GAP=$profileGap;
       |$arithmetic
       |${inputDeclarations.map("  "+_).mkString("\n")}
       |  $workArrayDeclarations
       |$modswitch
       |$modswitchLogic
       |  integer input_count,output_count,stage_index,stall_count; reg executing,inverse_operation,finishing,output_intt,output_ntt;
       |  $stageLogic
       |  always @(posedge clock) begin
       |    if(reset) begin io_intt_validout<=0;io_ntt_validout<=0;input_count<=0;output_count<=0;stage_index<=0;stall_count<=0;executing<=0;finishing<=0;output_intt<=0;output_ntt<=0;$resetWork end
       |    else begin io_intt_validout<=0;io_ntt_validout<=0;
       |      if(executing) begin if(stall_count>0) stall_count<=stall_count-1; else begin for(j=0;j<N;j=j+1) work[j]<=stage_next[j]; if(stage_index==(inverse_operation?I_LENGTH:F_LENGTH)-1) begin stage_index<=0;executing<=0;finishing<=1;end else begin stage_index<=stage_index+1;stall_count<=STEP_GAP;end end
       |      end else if(finishing) begin finishing<=0;if(inverse_operation)output_intt<=1;else output_ntt<=1;output_count<=0;
       |      end else if(output_intt) begin io_intt_validout<=1;case(output_count) ${outputCases(true)} endcase if(output_count==CYCLES-1) begin output_intt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(output_ntt) begin io_ntt_validout<=1;case(output_count) ${outputCases(false)} endcase if(output_count==CYCLES-1) begin output_ntt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(io_intt_validin) begin case(input_count) ${inputCases(true)} endcase if(input_count==CYCLES-1) begin input_count<=0;${lines(initializeI,8)} inverse_operation<=1;stage_index<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1;
       |      end else if(io_ntt_validin) begin case(input_count) ${inputCases(false)} endcase if(input_count==CYCLES-1) begin input_count<=0;${lines(initializeN,8)} inverse_operation<=0;stage_index<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |
       |module YataPipelinedModSwitch(input signed [53:0] value,output [31:0] torus);
       |  localparam signed [53:0] P=54'sd40960001; localparam [63:0] SCALE=64'd7036874245; reg signed [26:0] residue; reg [63:0] positive; reg [95:0] scaled;
       |  always @(*) begin residue=value[26:0];positive=(residue<0)?residue+P:residue;scaled=((positive*SCALE)+96'd33554432)>>26;end assign torus=scaled[31:0];
       |endmodule
       |/* verilator lint_on DECLFILENAME */
       |""".stripMargin
