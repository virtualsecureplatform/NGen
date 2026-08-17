package ngen.backend

import ngen.arithmetic.{YataField, YataTables}

/** Buffered YATA RAINTT backend for the characterized radix-8 streaming points. */
object YataStreamingSystemVerilog:
  private def literal(value: Long): String = if value < 0 then s"-27'sd${-value}" else s"27'sd$value"
  private def reverse3(value: Int): Int = ((value & 1) << 2) | (value & 2) | ((value & 4) >> 2)
  private def indented(lines: Seq[String], spaces: Int): String = lines.map(" " * spaces + _).mkString("\n")

  private def bothMod(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = yata_sword(work[$left]);",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = yata_sub_mod(temp, work[$right]);"
      )
    }.flatten

  private def addAdd(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = yata_sword(work[$left]);",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = temp - work[$right];"
      )
    }.flatten

  private def bothSredc(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = work[$left];",
        s"work[$left] = yata_sredc(work[$left] + work[$right]);",
        s"work[$right] = yata_sredc(temp - work[$right]);"
      )
    }.flatten

  private def inverseRadix4(offset: Int, size: Int): Vector[String] =
    val block = size >> 2
    addAdd(offset, size) ++ bothMod(offset, size / 2) ++
      Vector.tabulate(block)(index =>
        s"work[${offset + index + size / 2 + block}] = yata_const_twiddle_mul(work[${offset + index + size / 2 + block}], 2, 1);"
      ) ++ bothSredc(offset + size / 2, size / 2)

  private def inverseRadix8(offset: Int, size: Int): Vector[String] =
    val block = size >> 3
    val middle = Vector.tabulate(block) { index =>
      val left = offset + size / 2 + index
      val right = offset + size / 2 + 2 * block + index
      Vector(
        s"work[$right] = yata_const_twiddle_mul(work[$right], 3, 2);",
        s"temp = work[$left];",
        s"work[$left] = work[$left] + work[$right];",
        s"work[$right] = temp - work[$right];"
      )
    }.flatten
    val odd = Vector.tabulate(block) { index =>
      val left = offset + size / 2 + block + index
      val right = offset + size / 2 + 3 * block + index
      Vector(
        s"temp = yata_const_twiddle_mul(work[$left], 3, 3);",
        s"work[$left] = yata_const_twiddle_mul(work[$left], 3, 1) + yata_const_twiddle_mul(work[$right], 3, 3);",
        s"work[$right] = temp + yata_const_twiddle_mul(work[$right], 3, 1);"
      )
    }.flatten
    addAdd(offset, size) ++ inverseRadix4(offset, size / 2) ++ middle ++ odd ++
      bothSredc(offset + size / 2, size / 4) ++ bothSredc(offset + 3 * size / 4, size / 4)

  private def inverseTransform(logSize: Int, tables: YataTables): Vector[String] =
    val size = 1 << logSize
    var result = Vector.tabulate(size)(index =>
      s"work[$index] = yata_mul_sredc(intt_in_buf[$index][26:0], ${literal(tables.inttTwist(index))});"
    )
    var sizeLog = logSize
    while sizeLog > 3 do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = blockSize * block
        result ++= inverseRadix8(offset, blockSize)
        val subblock = blockSize >> 3
        for lane <- 1 until 8 do
          val stride = reverse3(lane) * blockCount
          val table = if lane > 1 then tables.inttTable1 else tables.inttTable0
          for index <- 0 until subblock do
            val position = offset + lane * subblock + index
            result :+= s"work[$position] = yata_mul_sredc(yata_sword(work[$position]), ${literal(table(stride * index))});"
      sizeLog -= 3
    for block <- 0 until 1 << (logSize - 3) do result ++= inverseRadix8(8 * block, 8)
    result

  private def forwardRadix4(offset: Int, size: Int, reduce: Boolean): Vector[String] =
    val first = Vector.tabulate(size / 4) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = work[$left];",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = yata_sub_mod(temp, work[$right]);"
      )
    }.flatten
    val second = Vector.tabulate(size / 4) { relative =>
      val left = offset + size / 4 + relative
      val right = left + size / 2
      val finish =
        if reduce then Vector(
          s"work[$left] = yata_sredc(work[$left] + work[$right]);",
          s"work[$right] = yata_sredc(temp - work[$right]);"
        )
        else Vector(s"work[$left] = work[$left] + work[$right];", s"work[$right] = temp - work[$right];")
      Vector(s"temp = work[$left];", s"work[$right] = -yata_const_twiddle_mul(work[$right], 2, 1);") ++ finish
    }.flatten
    bothMod(offset, size / 2) ++ bothMod(offset + size / 2, size / 2) ++ first ++ second

  private def forwardRadix8(offset: Int, size: Int): Vector[String] =
    val block = size >> 3
    val even = Vector.tabulate(block) { index =>
      val left = offset + size / 2 + index
      val right = left + size / 4
      Vector(
        s"temp = work[$left];",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = -yata_const_twiddle_mul(temp - work[$right], 3, 2);"
      )
    }.flatten
    val odd = Vector.tabulate(block) { index =>
      val left = offset + size / 2 + block + index
      val right = left + size / 4
      Vector(
        s"temp = -yata_const_twiddle_mul(work[$left], 3, 1);",
        s"work[$left] = -yata_const_twiddle_mul(work[$left], 3, 3) - yata_const_twiddle_mul(work[$right], 3, 1);",
        s"work[$right] = temp - yata_const_twiddle_mul(work[$right], 3, 3);"
      )
    }.flatten
    val finalButterflies = Vector.tabulate(4) { group =>
      Vector.tabulate(block) { index =>
        val left = offset + group * block + index
        val right = left + size / 2
        val reduced = group != 0
        Vector(s"temp = work[$left];") ++
          (if reduced then Vector(
            s"work[$left] = yata_sredc(work[$left] + work[$right]);",
            s"work[$right] = yata_sredc(temp - work[$right]);"
          ) else Vector(
            s"work[$left] = yata_add_mod(work[$left], work[$right]);",
            s"work[$right] = yata_sub_mod(temp, work[$right]);"
          ))
      }.flatten
    }.flatten
    forwardRadix4(offset, size / 2, reduce = false) ++ bothMod(offset + size / 2, size / 4) ++
      bothMod(offset + 3 * size / 4, size / 4) ++ even ++ odd ++ finalButterflies

  private def forwardTransform(logSize: Int, tables: YataTables): Vector[String] =
    val size = 1 << logSize
    var result = Vector.tabulate(size)(index => s"work[$index] = $$signed(ntt_in_buf[$index]);")
    for block <- 0 until 1 << (logSize - 3) do result ++= forwardRadix8(8 * block, 8)
    var sizeLog = 6
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> 3
      for block <- 0 until blockCount do
        val offset = blockSize * block
        for lane <- 0 until 8 do
          val stride = reverse3(lane) * blockCount
          for index <- 0 until subblock do
            val position = offset + lane * subblock + index
            val tableOne = ((index >> (sizeLog - 6)) & 3) != 0
            if stride == 0 && tableOne then
              result :+= s"work[$position] = yata_mul_sredc(yata_sword(work[$position]), ${literal(YataField.R2)});"
            else if stride != 0 then
              val table = if tableOne then tables.nttTable1 else tables.nttTable0
              result :+= s"work[$position] = yata_mul_sredc(yata_sword(work[$position]), ${literal(table(stride * index))});"
        result ++= forwardRadix8(offset, blockSize)
      sizeLog += 3
    result

  private def ports(top: String, lanes: Int): String =
    val declarations = Vector("input clock", "input reset", "input io_intt_validin") ++
      Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output reg [26:0] io_intt_out_$i") ++
      Vector("output reg io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output reg [31:0] io_ntt_out_$i") ++ Vector("output reg io_ntt_validout")
    s"module $top(\n${declarations.map("  " + _).mkString(",\n")}\n);"

  def emit(logSize: Int, streamingLog: Int, top: String): String =
    require(Set(3, 6, 9)(logSize), "YATA v0.1 supports log sizes 3, 6, and 9")
    val size = 1 << logSize
    val lanes = 1 << streamingLog
    require(size % lanes == 0 && (logSize, streamingLog) != (9, 3), "unsupported YATA streaming point")
    require((logSize == 3 && lanes == 8) || (logSize == 6 && lanes == 8) || (logSize == 9 && lanes == 64))
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val cycles = size / lanes
    val tables = YataField.tables(logSize)
    val inttBody = inverseTransform(logSize, tables)
    val nttBody = forwardTransform(logSize, tables)
    val inttStores = Vector.tabulate(size)(i => s"intt_out_buf[$i] <= work[$i][26:0];")
    val nttStores = Vector.tabulate(size) { i => Vector(
      s"mulres = yata_mul_sredc(yata_sword(work[$i]), ${literal(tables.nttTwist(i))});",
      "posres = (mulres < 0) ? (mulres + YATA_P) : mulres;",
      "scaled = ((posres * YATA_MODSWITCH_SCALE) + 96'd33554432) >> 26;",
      s"ntt_out_buf[$i] <= scaled[31:0];"
    )}.flatten
    val inttOutputs = Vector.tabulate(lanes)(lane => s"io_intt_out_$lane <= intt_out_buf[intt_out_count * $lanes + $lane];")
    val nttOutputs = Vector.tabulate(lanes)(lane => s"io_ntt_out_$lane <= ntt_out_buf[$lane * $cycles + ntt_out_count];")
    val inttInputs = Vector.tabulate(lanes)(lane => s"intt_in_buf[$lane * $cycles + intt_in_count] = io_intt_in_$lane;")
    val nttInputs = Vector.tabulate(lanes)(lane => s"ntt_in_buf[ntt_in_count * $lanes + $lane] = io_ntt_in_$lane;")

    s"""// Generated by NGen from the parameterized YATA radix-8 plan.
       |/* verilator lint_off WIDTHEXPAND */
       |/* verilator lint_off WIDTHTRUNC */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off BLKSEQ */
       |${ports(top, lanes)}
       |  localparam integer YATA_N = $size;
       |  localparam integer YATA_CYCLES = $cycles;
       |  localparam signed [53:0] YATA_P = 54'sd${YataField.Modulus};
       |  localparam signed [63:0] YATA_MODSWITCH_SCALE = 64'sd7036874245;
       |  reg [31:0] intt_in_buf [0:YATA_N-1];
       |  reg signed [26:0] intt_out_buf [0:YATA_N-1];
       |  reg signed [26:0] ntt_in_buf [0:YATA_N-1];
       |  reg [31:0] ntt_out_buf [0:YATA_N-1];
       |  reg signed [53:0] work [0:YATA_N-1];
       |  integer intt_in_count, ntt_in_count, intt_out_count, ntt_out_count;
       |  reg intt_output_active, ntt_output_active;
       |  reg signed [53:0] temp;
       |  reg signed [26:0] mulres;
       |  reg [63:0] posres;
       |  reg [95:0] scaled;
       |  integer i;
       |  function automatic signed [26:0] yata_sword(input signed [53:0] x); begin yata_sword = x[26:0]; end endfunction
       |  function automatic signed [26:0] yata_add_mod(input signed [53:0] a, input signed [53:0] b); reg signed [53:0] v; begin v=a+b; if(v>=YATA_P) yata_add_mod=v-YATA_P; else if(v<=-YATA_P) yata_add_mod=v+YATA_P; else yata_add_mod=v; end endfunction
       |  function automatic signed [26:0] yata_sub_mod(input signed [53:0] a, input signed [53:0] b); reg signed [53:0] v; begin v=a-b; if(v>=YATA_P) yata_sub_mod=v-YATA_P; else if(v<=-YATA_P) yata_sub_mod=v+YATA_P; else yata_sub_mod=v; end endfunction
       |  function automatic signed [26:0] yata_sredc(input signed [53:0] a); reg [26:0] a0; reg signed [26:0] a1,m,t1; reg signed [53:0] mw,tw; begin a0=a[26:0]; a1=a[53:27]; mw=-(({27'd0,a0}*54'sd625)<<<16)+{27'd0,a0}; m=mw[26:0]; tw=(($$signed(m)*54'sd625)<<<16)+$$signed(m); t1=tw[53:27]; yata_sredc=a1-t1; end endfunction
       |  function automatic signed [26:0] yata_mul_sredc(input signed [26:0] a,input signed [26:0] b); begin yata_mul_sredc=yata_sredc($$signed(a)*$$signed(b)); end endfunction
       |  function automatic signed [53:0] yata_const_twiddle_mul(input signed [53:0] a,input integer rb,input integer num); begin if(rb==2&&num==1) yata_const_twiddle_mul=(a*25)<<<8; else if(rb==3&&num==1) yata_const_twiddle_mul=(a*5)<<<4; else if(rb==3&&num==2) yata_const_twiddle_mul=(a*25)<<<8; else if(rb==3&&num==3) yata_const_twiddle_mul=(a*125)<<<12; else yata_const_twiddle_mul=a; end endfunction
       |  task automatic compute_intt; begin
       |${indented(inttBody ++ inttStores, 4)}
       |  end endtask
       |  task automatic compute_ntt; begin
       |${indented(nttBody ++ nttStores, 4)}
       |  end endtask
       |  always @(posedge clock) begin
       |    if(reset) begin io_intt_validout<=0; io_ntt_validout<=0; intt_in_count<=0; ntt_in_count<=0; intt_out_count<=0; ntt_out_count<=0; intt_output_active<=0; ntt_output_active<=0; for(i=0;i<YATA_N;i=i+1) begin intt_in_buf[i]<=0; intt_out_buf[i]<=0; ntt_in_buf[i]<=0; ntt_out_buf[i]<=0; end end
       |    else begin
       |      io_intt_validout<=0; io_ntt_validout<=0;
       |      if(intt_output_active) begin io_intt_validout<=1;
       |${indented(inttOutputs, 8)}
       |        if(intt_out_count==YATA_CYCLES-1) begin intt_output_active<=0; intt_out_count<=0; end else intt_out_count<=intt_out_count+1; end
       |      if(ntt_output_active) begin io_ntt_validout<=1;
       |${indented(nttOutputs, 8)}
       |        if(ntt_out_count==YATA_CYCLES-1) begin ntt_output_active<=0; ntt_out_count<=0; end else ntt_out_count<=ntt_out_count+1; end
       |      if(io_intt_validin) begin
       |${indented(inttInputs, 8)}
       |        if(intt_in_count==YATA_CYCLES-1) begin intt_in_count<=0; compute_intt(); intt_output_active<=1; intt_out_count<=0; end else intt_in_count<=intt_in_count+1; end
       |      if(io_ntt_validin) begin
       |${indented(nttInputs, 8)}
       |        if(ntt_in_count==YATA_CYCLES-1) begin ntt_in_count<=0; compute_ntt(); ntt_output_active<=1; ntt_out_count<=0; end else ntt_in_count<=ntt_in_count+1; end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on BLKSEQ */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on WIDTHTRUNC */
       |/* verilator lint_on WIDTHEXPAND */
       |""".stripMargin
