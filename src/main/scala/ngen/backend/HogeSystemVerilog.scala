package ngen.backend

import ngen.arithmetic.HogeField

object HogeSystemVerilog:
  private def hex(value: BigInt): String = f"64'h${HogeField.normalize(value)}%016x"
  private def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")

  private def butterfly(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(s"temp = work[$left];", s"work[$left] = hoge_add(work[$left], work[$right]);", s"work[$right] = hoge_sub(temp, work[$right]);")
    }.flatten

  private def inverseButterfly(offset: Int, size: Int, radixLog: Int): Vector[String] =
    if radixLog == 0 then Vector.empty
    else
      val block = size >> radixLog
      val shifts = (for
        lane <- 1 until 1 << (radixLog - 1)
        index <- 0 until block
      yield
        val position = offset + size / 2 + lane * block + index
        s"work[$position] = hoge_mul(work[$position], ${hex(BigInt(2).modPow(3 * (lane << (6 - radixLog)), HogeField.Modulus))});"
      ).toVector
      butterfly(offset, size) ++ shifts ++ inverseButterfly(offset, size / 2, radixLog - 1) ++ inverseButterfly(offset + size / 2, size / 2, radixLog - 1)

  private def forwardButterfly(offset: Int, size: Int, radixLog: Int): Vector[String] =
    if radixLog == 0 then Vector.empty
    else
      val block = size >> radixLog
      val shifts =
        if radixLog == 1 then Vector.empty
        else (for
          lane <- 1 until 1 << (radixLog - 1)
          index <- 0 until block
        yield
          val position = offset + size / 2 + lane * block + index
          s"work[$position] = hoge_mul(work[$position], ${hex(BigInt(2).modPow(3 * (64 - (lane << (6 - radixLog))), HogeField.Modulus))});"
        ).toVector
      forwardButterfly(offset + size / 2, size / 2, radixLog - 1) ++ forwardButterfly(offset, size / 2, radixLog - 1) ++ shifts ++ butterfly(offset, size)

  private def inverseTransform(logSize: Int, radixLog: Int): Vector[String] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    var result = Vector.tabulate(size)(i => s"work[$i] = hoge_mul({32'd0, intt_in_buf[$i]}, ${hex(tables.inverseTwist(i))});")
    var sizeLog = logSize
    while sizeLog > radixLog do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = block * blockSize
        result ++= inverseButterfly(offset, blockSize, radixLog)
        val subblock = blockSize >> radixLog
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          result :+= s"work[$position] = hoge_mul(work[$position], ${hex(tables.inverse(HogeField.reverse(lane, radixLog) * blockCount * index))});"
      sizeLog -= radixLog
    for block <- 0 until 1 << (logSize - radixLog) do result ++= inverseButterfly(block << radixLog, 1 << radixLog, radixLog)
    result

  private def forwardTransform(logSize: Int, radixLog: Int): Vector[String] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    var result = Vector.tabulate(size)(i => s"work[$i] = ntt_in_buf[$i];")
    for block <- 0 until 1 << (logSize - radixLog) do result ++= forwardButterfly(block << radixLog, 1 << radixLog, radixLog)
    var sizeLog = 2 * radixLog
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> radixLog
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          result :+= s"work[$position] = hoge_mul(work[$position], ${hex(tables.forward(HogeField.reverse(lane, radixLog) * blockCount * index))});"
        result ++= forwardButterfly(offset, blockSize, radixLog)
      sizeLog += radixLog
    val inverseSize = HogeField.inversePowerOfTwo(logSize)
    for index <- 0 until size do result :+= s"work[$index] = hoge_mul(hoge_mul(work[$index], ${hex(tables.forwardTwist(index))}), ${hex(inverseSize)});"
    result

  private val arithmetic = s"""
  localparam [63:0] HOGE_P = 64'hffffffff00000001;
  function automatic [63:0] hoge_normalize(input [63:0] value); begin hoge_normalize = (value >= HOGE_P) ? value + 64'h00000000ffffffff : value; end endfunction
  function automatic [63:0] hoge_add(input [63:0] a,input [63:0] b); reg [64:0] sum; begin sum={1'b0,a}+{1'b0,b}; hoge_add=(sum[64]||sum[63:0]>=HOGE_P)?sum[63:0]+64'h00000000ffffffff:sum[63:0]; end endfunction
  function automatic [63:0] hoge_sub(input [63:0] a,input [63:0] b); reg [64:0] diff; begin diff={1'b0,a}-{1'b0,b}; hoge_sub=diff[64]?diff[63:0]-64'h00000000ffffffff:diff[63:0]; end endfunction
  function automatic [63:0] hoge_mul(input [63:0] a,input [63:0] b);
    reg [127:0] product; reg [31:0] t0,t1,t2,t3; reg [63:0] lo,middle,res;
    begin product=a*b; lo=product[63:0]; t0=product[31:0]; t1=product[63:32]; t2=product[95:64]; t3=product[127:96]; middle={32'd0,t1}+t2; res=(middle<<32)+t0-t3-t2; if((res>lo)&&(t2==0)) res=res-64'h00000000ffffffff; if((res<lo)&&(t2!=0)) res=res+64'h00000000ffffffff; hoge_mul=hoge_normalize(res); end
  endfunction
  """

  def emitRadix32(top: String = "SmallHoge32P64Rtl"): String =
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++
      Vector.tabulate(32)(i => s"input [63:0] io_intt_in_$i") ++ Vector.tabulate(32)(i => s"output reg [63:0] io_intt_out_$i") ++
      Vector("output reg io_intt_validout", "input io_ntt_validin") ++ Vector.tabulate(32)(i => s"input [63:0] io_ntt_in_$i") ++
      Vector.tabulate(32)(i => s"output reg [63:0] io_ntt_out_$i") ++ Vector("output reg io_ntt_validout")
    val loadsI = Vector.tabulate(32)(i => s"work[$i] = io_intt_in_$i;")
    val loadsN = Vector.tabulate(32)(i => s"work[$i] = io_ntt_in_$i;")
    val storesI = Vector.tabulate(32)(i => s"io_intt_out_$i <= work[$i];")
    val storesN = Vector.tabulate(32)(i => s"io_ntt_out_$i <= work[$i];")
    s"""// Generated HOGE radix-32 graph.
       |/* verilator lint_off BLKSEQ */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off WIDTHEXPAND */
       |module $top(
       |${ports.map("  " + _).mkString(",\n")}
       |);
       |$arithmetic
       |  reg [63:0] work [0:31]; reg [63:0] temp; reg intt_pending,ntt_pending; integer i;
       |  task automatic compute_intt; begin ${lines(loadsI ++ inverseButterfly(0,32,5), 4)} end endtask
       |  task automatic compute_ntt; begin ${lines(loadsN ++ forwardButterfly(0,32,5), 4)} end endtask
       |  always @(posedge clock) begin if(reset) begin io_intt_validout<=0; io_ntt_validout<=0; intt_pending<=0; ntt_pending<=0; end else begin io_intt_validout<=intt_pending; io_ntt_validout<=ntt_pending; intt_pending<=io_intt_validin; ntt_pending<=io_ntt_validin; if(io_intt_validin) compute_intt(); if(io_ntt_validin) compute_ntt(); ${lines(storesI ++ storesN, 4)} end end
       |endmodule
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin

  def emitStreamingIntt(top: String = "INTTWrap"): String = emitStreaming(top, inverse = true)
  def emitStreamingNtt(top: String = "NTTWrap"): String = emitStreaming(top, inverse = false)

  private def emitStreaming(top: String, inverse: Boolean): String =
    val size = 1024
    val lanes = 32
    val cycles = 32
    val body = if inverse then inverseTransform(10,5) else forwardTransform(10,5)
    val inputWidth = if inverse then 32 else 64
    val outputWidth = 64
    val inputBuffer = if inverse then "intt_in_buf" else "ntt_in_buf"
    val outputBuffer = if inverse then "intt_out_buf" else "ntt_out_buf"
    val capture = Vector.tabulate(lanes) { lane =>
      val index = if inverse then s"$lane * $cycles + input_count" else s"input_count * $lanes + $lane"
      s"$inputBuffer[$index] = io_in[$lane * $inputWidth +: $inputWidth];"
    }
    val output = Vector.tabulate(lanes)(lane => s"io_out[$lane * $outputWidth +: $outputWidth] <= $outputBuffer[output_count * $lanes + $lane];")
    val stores = Vector.tabulate(size)(i => s"$outputBuffer[$i] <= work[$i];")
    val readyPort = if inverse then "" else "  output io_ready,\n"
    val readyAssign = if inverse then "" else "  assign io_ready = io_enable && !output_active;"
    s"""// Generated HOGE 1024-point ${if inverse then "INTT" else "NTT"}.
       |/* verilator lint_off BLKSEQ */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off WIDTHEXPAND */
       |module $top(
       |  input clock,
       |  input reset,
       |  input io_enable,
       |$readyPort  output reg io_validout,
       |  input [${lanes * inputWidth - 1}:0] io_in,
       |  output reg [${lanes * outputWidth - 1}:0] io_out
       |);
       |$arithmetic
       |  reg [31:0] intt_in_buf [0:1023]; reg [63:0] intt_out_buf [0:1023];
       |  reg [63:0] ntt_in_buf [0:1023]; reg [63:0] ntt_out_buf [0:1023];
       |  reg [63:0] work [0:1023]; reg [63:0] temp; integer i,input_count,output_count; reg output_active;
       |$readyAssign
       |  task automatic compute_transform; begin
       |${lines(body ++ stores,4)}
       |  end endtask
       |  always @(posedge clock) begin
       |    if(reset) begin io_validout<=0; input_count<=0; output_count<=0; output_active<=0; io_out<=0; end
       |    else begin io_validout<=0;
       |      if(output_active) begin io_validout<=1;
       |${lines(output,8)}
       |        if(output_count==$cycles-1) begin output_count<=0; output_active<=0; end else output_count<=output_count+1; end
       |      if(io_enable && !output_active) begin
       |${lines(capture,8)}
       |        if(input_count==$cycles-1) begin input_count<=0; compute_transform(); output_count<=0; output_active<=1; end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin
