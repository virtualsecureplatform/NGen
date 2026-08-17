package ngen.backend

import ngen.arithmetic.HogeField
import ngen.rtl.ProfileName
import ngen.rtl.{IndexedOperation, MicroProgram}

object HogeSystemVerilog:
  private sealed trait MicroOp extends IndexedOperation:
    def indices: Set[Int]
  private final case class Butterfly(left: Int, right: Int) extends MicroOp:
    override val indices = Set(left, right)
  private final case class Multiply(index: Int, constant: BigInt) extends MicroOp:
    override val indices = Set(index)

  private def hex(value: BigInt): String = f"64'h${HogeField.normalize(value)}%016x"
  private def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")

  private def butterfly(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(s"temp = work[$left];", s"work[$left] = hoge_add(work[$left], work[$right]);", s"work[$right] = hoge_sub(temp, work[$right]);")
    }.flatten

  private def butterflyOps(offset: Int, size: Int): Vector[MicroOp] =
    Vector.tabulate(size / 2)(index => Butterfly(offset + index, offset + index + size / 2))

  private def inverseButterflyOps(offset: Int, size: Int, radixLog: Int): Vector[MicroOp] =
    if radixLog == 0 then Vector.empty
    else
      val block = size >> radixLog
      val shifts = (for
        lane <- 1 until 1 << (radixLog - 1)
        index <- 0 until block
      yield Multiply(offset + size / 2 + lane * block + index, BigInt(2).modPow(3 * (lane << (6 - radixLog)), HogeField.Modulus))).toVector
      butterflyOps(offset, size) ++ shifts ++ inverseButterflyOps(offset, size / 2, radixLog - 1) ++ inverseButterflyOps(offset + size / 2, size / 2, radixLog - 1)

  private def forwardButterflyOps(offset: Int, size: Int, radixLog: Int): Vector[MicroOp] =
    if radixLog == 0 then Vector.empty
    else
      val block = size >> radixLog
      val shifts =
        if radixLog == 1 then Vector.empty
        else (for
          lane <- 1 until 1 << (radixLog - 1)
          index <- 0 until block
        yield Multiply(offset + size / 2 + lane * block + index, BigInt(2).modPow(3 * (64 - (lane << (6 - radixLog))), HogeField.Modulus))).toVector
      forwardButterflyOps(offset + size / 2, size / 2, radixLog - 1) ++ forwardButterflyOps(offset, size / 2, radixLog - 1) ++ shifts ++ butterflyOps(offset, size)

  private def inverseProgram(logSize: Int, radixLog: Int): Vector[MicroOp] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    var result = Vector.tabulate(size)(i => Multiply(i, tables.inverseTwist(i)): MicroOp)
    var sizeLog = logSize
    while sizeLog > radixLog do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = block * blockSize
        result ++= inverseButterflyOps(offset, blockSize, radixLog)
        val subblock = blockSize >> radixLog
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          result :+= Multiply(position, tables.inverse(HogeField.reverse(lane, radixLog) * blockCount * index))
      sizeLog -= radixLog
    for block <- 0 until 1 << (logSize - radixLog) do result ++= inverseButterflyOps(block << radixLog, 1 << radixLog, radixLog)
    result

  private def forwardProgram(logSize: Int, radixLog: Int): Vector[MicroOp] =
    val size = 1 << logSize
    val tables = HogeField.tables(logSize)
    var result = Vector.empty[MicroOp]
    for block <- 0 until 1 << (logSize - radixLog) do result ++= forwardButterflyOps(block << radixLog, 1 << radixLog, radixLog)
    var sizeLog = 2 * radixLog
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> radixLog
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 1 until 1 << radixLog; index <- 1 until subblock do
          val position = offset + lane * subblock + index
          result :+= Multiply(position, tables.forward(HogeField.reverse(lane, radixLog) * blockCount * index))
        result ++= forwardButterflyOps(offset, blockSize, radixLog)
      sizeLog += radixLog
    val inverseSize = HogeField.inversePowerOfTwo(logSize)
    for index <- 0 until size do
      result :+= Multiply(index, tables.forwardTwist(index))
      result :+= Multiply(index, inverseSize)
    result

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

  def emitStreamingIntt(top: String = "INTTWrap", profile: ProfileName = ProfileName.Baseline): String = emitStreaming(top, inverse = true, profile)
  def emitStreamingNtt(top: String = "NTTWrap", profile: ProfileName = ProfileName.Baseline): String = emitStreaming(top, inverse = false, profile)
  def streamingBundles(inverse: Boolean, profile: ProfileName = ProfileName.Baseline): Int =
    MicroProgram.schedule(if inverse then inverseProgram(10,5) else forwardProgram(10,5), 32).length * (if profile == ProfileName.F300 then 2 else 1)

  private def emitStreaming(top: String, inverse: Boolean, profile: ProfileName): String =
    val size = 1024
    val lanes = 32
    val cycles = 32
    val groups = MicroProgram.schedule(if inverse then inverseProgram(10,5) else forwardProgram(10,5), lanes).bundles
    require(groups.size < 3990, s"HOGE program exceeds watchdog: ${groups.size} bundles")
    val inputWidth = if inverse then 32 else 64
    val outputWidth = 64
    def w(index: Int) = s"w$index"
    def setup(operation: MicroOp, lane: Int): Vector[String] = operation match
      case Butterfly(left, right) => Vector(s"lane_kind_$lane=2'd1;", s"lane_a_$lane=${w(left)};", s"lane_b_$lane=${w(right)};")
      case Multiply(index, constant) => Vector(s"lane_kind_$lane=2'd2;", s"lane_a_$lane=${w(index)};", s"lane_constant_$lane=${hex(constant)};")
    def writeback(operation: MicroOp, lane: Int): Vector[String] = operation match
      case Butterfly(left, right) => Vector(s"${w(left)}<=lane_out_a_$lane;", s"${w(right)}<=lane_out_b_$lane;")
      case Multiply(index, _) => Vector(s"${w(index)}<=lane_out_a_$lane;")
    def cases(render: (MicroOp, Int) => Vector[String]): String = groups.zipWithIndex.map { case (group, pc) =>
      s"$pc: begin\n${lines(group.zipWithIndex.flatMap((operation, lane) => render(operation, lane)),12)}\n          end"
    }.mkString("\n")
    val inputDeclarations = Vector.tabulate(size)(i => s"reg [${inputWidth - 1}:0] input$i;")
    val workDeclarations = Vector.tabulate(size)(i => s"reg [63:0] ${w(i)};")
    val laneDeclarations = Vector.tabulate(lanes)(i => s"reg [1:0] lane_kind_$i; reg [63:0] lane_a_$i,lane_b_$i,lane_constant_$i; wire [63:0] lane_out_a_$i,lane_out_b_$i;")
    val laneDefaults = Vector.tabulate(lanes)(i => s"lane_kind_$i=0;lane_a_$i=0;lane_b_$i=0;lane_constant_$i=0;")
    val laneInstances = Vector.tabulate(lanes)(i => s"HogeMicroLane lane_$i(lane_kind_$i,lane_a_$i,lane_b_$i,lane_constant_$i,lane_out_a_$i,lane_out_b_$i);")
    def inputCases: String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes) { lane =>
        val index = if inverse then lane * cycles + cycle else cycle * lanes + lane
        s"input$index=io_in[$lane*$inputWidth+:$inputWidth];"
      }
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    def outputCases: String = (0 until cycles).map { cycle =>
      val assignments = Vector.tabulate(lanes)(lane => s"io_out[$lane*$outputWidth+:$outputWidth]<=${w(cycle * lanes + lane)};")
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    val initializeWork = Vector.tabulate(size)(i => s"${w(i)}<=${if inverse then s"{32'd0,input$i}" else s"input$i"};")
    val readyPort = if inverse then "" else "  output io_ready,\n"
    val readyAssign = if inverse then "" else "  assign io_ready = io_enable && !output_active && !executing && !finishing;"
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
       |  localparam integer PROGRAM_LENGTH = ${groups.size};
       |  localparam integer STEP_GAP = ${if profile == ProfileName.F300 then 1 else 0};
       |${lines(inputDeclarations ++ workDeclarations ++ laneDeclarations ++ laneInstances,2)}
       |  integer input_count,output_count,pc,stall_count; reg output_active,executing,finishing;
       |$readyAssign
       |  always @(*) begin ${lines(laneDefaults,4)} case(pc)
       |${cases(setup)}
       |  endcase end
       |  always @(posedge clock) begin
       |    if(reset) begin io_validout<=0; input_count<=0; output_count<=0; pc<=0; stall_count<=0; output_active<=0; executing<=0; finishing<=0; io_out<=0; end
       |    else begin io_validout<=0;
       |      if(executing) begin
       |        if(stall_count>0) stall_count<=stall_count-1; else begin
       |        case(pc)
       |${cases(writeback)}
       |        endcase
       |        if(pc==PROGRAM_LENGTH-1) begin pc<=0; executing<=0; finishing<=1; end else begin pc<=pc+1; stall_count<=STEP_GAP; end end
       |      end else if(finishing) begin finishing<=0; output_active<=1; output_count<=0;
       |      end else if(output_active) begin io_validout<=1;
       |        case(output_count) $outputCases endcase
       |        if(output_count==$cycles-1) begin output_count<=0; output_active<=0; end else output_count<=output_count+1; end
       |      else if(io_enable) begin
       |        case(input_count) $inputCases endcase
       |        if(input_count==$cycles-1) begin input_count<=0;
       |${lines(initializeWork,10)}
       |          pc<=0; stall_count<=0; executing<=1; end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |
       |module HogeMicroLane(input [1:0] kind,input [63:0] a,input [63:0] b,input [63:0] constant,output reg [63:0] out_a,output reg [63:0] out_b);
       |$arithmetic
       |  always @(*) begin out_a=0;out_b=0;case(kind) 1:begin out_a=hoge_add(a,b);out_b=hoge_sub(a,b);end 2:out_a=hoge_mul(a,constant);default:begin end endcase end
       |endmodule
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin
