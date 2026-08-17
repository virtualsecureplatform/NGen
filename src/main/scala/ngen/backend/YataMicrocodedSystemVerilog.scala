package ngen.backend

import ngen.arithmetic.{YataField, YataTables}
import ngen.rtl.ProfileName

object YataMicrocodedSystemVerilog:
  private final case class MicroOp(kind: Int, left: Int, right: Int = 0, constant: Long = 0, radixLog: Int = 0, number: Int = 0):
    val indices: Set[Int] = if kind == 4 || kind == 10 then Set(left) else Set(left, right)

  private def reverse3(value: Int): Int = ((value & 1) << 2) | (value & 2) | ((value & 4) >> 2)
  private def bothMod(offset: Int, size: Int): Vector[MicroOp] = Vector.tabulate(size / 2)(i => MicroOp(1, offset + i, offset + i + size / 2))
  private def addAdd(offset: Int, size: Int): Vector[MicroOp] = Vector.tabulate(size / 2)(i => MicroOp(2, offset + i, offset + i + size / 2))
  private def bothSredc(offset: Int, size: Int): Vector[MicroOp] = Vector.tabulate(size / 2)(i => MicroOp(3, offset + i, offset + i + size / 2))

  private def inverseRadix4(offset: Int, size: Int): Vector[MicroOp] =
    val block = size >> 2
    addAdd(offset, size) ++ bothMod(offset, size / 2) ++
      Vector.tabulate(block)(i => MicroOp(4, offset + i + size / 2 + block, radixLog = 2, number = 1)) ++
      bothSredc(offset + size / 2, size / 2)

  private def inverseRadix8(offset: Int, size: Int): Vector[MicroOp] =
    val block = size >> 3
    addAdd(offset, size) ++ inverseRadix4(offset, size / 2) ++
      Vector.tabulate(block)(i => MicroOp(5, offset + size / 2 + i, offset + size / 2 + 2 * block + i)) ++
      Vector.tabulate(block)(i => MicroOp(6, offset + size / 2 + block + i, offset + size / 2 + 3 * block + i)) ++
      bothSredc(offset + size / 2, size / 4) ++ bothSredc(offset + 3 * size / 4, size / 4)

  private def forwardRadix4(offset: Int, size: Int): Vector[MicroOp] =
    bothMod(offset, size / 2) ++ bothMod(offset + size / 2, size / 2) ++
      Vector.tabulate(size / 4)(i => MicroOp(1, offset + i, offset + i + size / 2)) ++
      Vector.tabulate(size / 4)(i => MicroOp(7, offset + size / 4 + i, offset + 3 * size / 4 + i))

  private def forwardRadix8(offset: Int, size: Int): Vector[MicroOp] =
    val block = size >> 3
    forwardRadix4(offset, size / 2) ++ bothMod(offset + size / 2, size / 4) ++ bothMod(offset + 3 * size / 4, size / 4) ++
      Vector.tabulate(block)(i => MicroOp(8, offset + size / 2 + i, offset + 3 * size / 4 + i)) ++
      Vector.tabulate(block)(i => MicroOp(9, offset + size / 2 + block + i, offset + 3 * size / 4 + block + i)) ++
      Vector.tabulate(block)(i => MicroOp(1, offset + i, offset + size / 2 + i)) ++
      (1 until 4).flatMap(group => Vector.tabulate(block)(i => MicroOp(3, offset + group * block + i, offset + size / 2 + group * block + i))).toVector

  private def inverseProgram(logSize: Int, tables: YataTables): Vector[MicroOp] =
    val size = 1 << logSize
    var result = Vector.tabulate(size)(i => MicroOp(10, i, constant = tables.inttTwist(i)))
    var sizeLog = logSize
    while sizeLog > 3 do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      for block <- 0 until blockCount do
        val offset = block * blockSize
        result ++= inverseRadix8(offset, blockSize)
        val subblock = blockSize >> 3
        for lane <- 1 until 8; index <- 0 until subblock do
          val table = if lane > 1 then tables.inttTable1 else tables.inttTable0
          result :+= MicroOp(10, offset + lane * subblock + index, constant = table(reverse3(lane) * blockCount * index))
      sizeLog -= 3
    for block <- 0 until 1 << (logSize - 3) do result ++= inverseRadix8(block * 8, 8)
    result

  private def forwardProgram(logSize: Int, tables: YataTables): Vector[MicroOp] =
    val size = 1 << logSize
    var result = Vector.empty[MicroOp]
    for block <- 0 until 1 << (logSize - 3) do result ++= forwardRadix8(block * 8, 8)
    var sizeLog = 6
    while sizeLog <= logSize do
      val blockSize = 1 << sizeLog
      val blockCount = 1 << (logSize - sizeLog)
      val subblock = blockSize >> 3
      for block <- 0 until blockCount do
        val offset = block * blockSize
        for lane <- 0 until 8; index <- 0 until subblock do
          val tableOne = ((index >> (sizeLog - 6)) & 3) != 0
          if reverse3(lane) == 0 && tableOne then result :+= MicroOp(10, offset + lane * subblock + index, constant = YataField.R2)
          else if reverse3(lane) != 0 then
            val table = if tableOne then tables.nttTable1 else tables.nttTable0
            result :+= MicroOp(10, offset + lane * subblock + index, constant = table(reverse3(lane) * blockCount * index))
        result ++= forwardRadix8(offset, blockSize)
      sizeLog += 3
    for index <- 0 until size do result :+= MicroOp(10, index, constant = tables.nttTwist(index))
    result

  private def bundle(program: Vector[MicroOp], width: Int): Vector[Vector[MicroOp]] =
    val groups = scala.collection.mutable.ArrayBuffer.empty[Vector[MicroOp]]
    var current = Vector.empty[MicroOp]
    var used = Set.empty[Int]
    program.foreach { op =>
      if current.size == width || op.indices.exists(used) then
        groups += current; current = Vector.empty; used = Set.empty
      current :+= op; used ++= op.indices
    }
    if current.nonEmpty then groups += current
    groups.toVector

  private def lit(value: Long): String = if value < 0 then s"-27'sd${-value}" else s"27'sd$value"
  private def lines(values: Seq[String], indent: Int): String = values.map(" " * indent + _).mkString("\n")

  def scheduleLengths(logSize: Int, streamingLog: Int, profile: ProfileName): (Int, Int) =
    val tables = YataField.tables(logSize)
    val factor = if profile == ProfileName.F300 then 2 else 1
    (bundle(inverseProgram(logSize, tables), 1 << streamingLog).size * factor,
      bundle(forwardProgram(logSize, tables), 1 << streamingLog).size * factor)

  def emit(logSize: Int, streamingLog: Int, profile: ProfileName, top: String): String =
    val size = 1 << logSize
    val lanes = 1 << streamingLog
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid module name: $top")
    require((logSize == 3 && streamingLog == 3) || (logSize == 6 && streamingLog == 3) || (logSize == 9 && streamingLog == 6))
    val cycles = size / lanes
    val tables = YataField.tables(logSize)
    val inverse = bundle(inverseProgram(logSize, tables), lanes)
    val forward = bundle(forwardProgram(logSize, tables), lanes)
    require(inverse.size * (if profile == ProfileName.F300 then 2 else 1) < 1900)
    require(forward.size * (if profile == ProfileName.F300 then 2 else 1) < 1900)
    def w(index: Int) = s"w$index"
    def setup(op: MicroOp, lane: Int): Vector[String] = Vector(
      s"lane_kind_$lane=4'd${op.kind};",
      s"lane_a_$lane=${w(op.left)};",
      s"lane_b_$lane=${w(op.right)};",
      s"lane_constant_$lane=${lit(op.constant)};",
      s"lane_radix_$lane=2'd${op.radixLog};",
      s"lane_number_$lane=2'd${op.number};"
    )
    def writeback(op: MicroOp, lane: Int): Vector[String] =
      if op.kind == 4 || op.kind == 10 then Vector(s"${w(op.left)}<=lane_out_a_$lane;")
      else Vector(s"${w(op.left)}<=lane_out_a_$lane;", s"${w(op.right)}<=lane_out_b_$lane;")
    def cases(groups: Vector[Vector[MicroOp]], render: (MicroOp, Int) => Vector[String]): String = groups.zipWithIndex.map { case (group, pc) =>
      s"$pc: begin\n${lines(group.zipWithIndex.flatMap((op, lane) => render(op, lane)),12)}\n          end"
    }.mkString("\n")
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++ Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output reg [26:0] io_intt_out_$i") ++ Vector("output reg io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++ Vector.tabulate(lanes)(i => s"output reg [31:0] io_ntt_out_$i") ++ Vector("output reg io_ntt_validout")
    val inputDeclarations = Vector.tabulate(size)(i => s"reg [31:0] intt$i; reg signed [26:0] ntt$i;")
    val workDeclarations = Vector.tabulate(size)(i => s"reg signed [53:0] ${w(i)};")
    val modswitchDeclarations = Vector.tabulate(size)(i => s"wire [31:0] torus$i;")
    val modswitchInstances = Vector.tabulate(size)(i => s"YataModSwitch modswitch_$i(${w(i)},torus$i);")
    val laneDeclarations = Vector.tabulate(lanes)(i => s"reg [3:0] lane_kind_$i; reg signed [53:0] lane_a_$i,lane_b_$i; reg signed [26:0] lane_constant_$i; reg [1:0] lane_radix_$i,lane_number_$i; wire signed [53:0] lane_out_a_$i,lane_out_b_$i;")
    val laneDefaults = Vector.tabulate(lanes)(i => s"lane_kind_$i=0;lane_a_$i=0;lane_b_$i=0;lane_constant_$i=0;lane_radix_$i=0;lane_number_$i=0;")
    val laneInstances = Vector.tabulate(lanes)(i => s"YataMicroLane lane_$i(lane_kind_$i,lane_a_$i,lane_b_$i,lane_constant_$i,lane_radix_$i,lane_number_$i,lane_out_a_$i,lane_out_b_$i);")
    val initializeI = Vector.tabulate(size)(i => s"${w(i)}<={{27{1'b0}},intt$i[26:0]};")
    val initializeN = Vector.tabulate(size)(i => s"${w(i)}<={{27{ntt$i[26]}},ntt$i};")
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
        if isIntt then s"io_intt_out_$lane<=${w(index)}[26:0];" else s"io_ntt_out_$lane<=torus$index;"
      }
      s"$cycle: begin\n${lines(assignments,10)}\n        end"
    }.mkString("\n")
    s"""// Generated by NGen's microcoded YATA radix-8 backend.
       |/* verilator lint_off BLKSEQ */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off WIDTHEXPAND */
       |/* verilator lint_off WIDTHTRUNC */
       |module $top(
       |${ports.map("  " + _).mkString(",\n")}
       |);
       |  localparam integer I_LENGTH=${inverse.size}; localparam integer F_LENGTH=${forward.size}; localparam integer STEP_GAP=${if profile == ProfileName.F300 then 1 else 0};
       |${lines(inputDeclarations ++ workDeclarations ++ modswitchDeclarations ++ laneDeclarations ++ laneInstances ++ modswitchInstances,2)}
       |  integer pc,input_count,output_count,stall_count; reg executing,inverse_operation,finishing,output_intt,output_ntt;
       |  always @(*) begin ${lines(laneDefaults,4)} if(inverse_operation)begin case(pc)
       |${cases(inverse,setup)}
       |  endcase end else begin case(pc)
       |${cases(forward,setup)}
       |  endcase end end
       |  always @(posedge clock) begin
       |    if(reset)begin io_intt_validout<=0;io_ntt_validout<=0;input_count<=0;output_count<=0;pc<=0;stall_count<=0;executing<=0;finishing<=0;output_intt<=0;output_ntt<=0;end else begin io_intt_validout<=0;io_ntt_validout<=0;
       |      if(executing)begin if(stall_count>0)stall_count<=stall_count-1;else begin
       |        if(inverse_operation)begin case(pc)
       |${cases(inverse,writeback)}
       |        endcase end else begin case(pc)
       |${cases(forward,writeback)}
       |        endcase end
       |        if(pc==(inverse_operation?I_LENGTH:F_LENGTH)-1)begin pc<=0;executing<=0;finishing<=1;end else begin pc<=pc+1;stall_count<=STEP_GAP;end end
       |      end else if(finishing)begin finishing<=0;if(inverse_operation)output_intt<=1;else output_ntt<=1;output_count<=0;
       |      end else if(output_intt)begin io_intt_validout<=1;case(output_count) ${outputCases(true)} endcase if(output_count==$cycles-1)begin output_intt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(output_ntt)begin io_ntt_validout<=1;case(output_count) ${outputCases(false)} endcase if(output_count==$cycles-1)begin output_ntt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(io_intt_validin)begin case(input_count) ${inputCases(true)} endcase if(input_count==$cycles-1)begin input_count<=0;${lines(initializeI,8)} inverse_operation<=1;pc<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1;
       |      end else if(io_ntt_validin)begin case(input_count) ${inputCases(false)} endcase if(input_count==$cycles-1)begin input_count<=0;${lines(initializeN,8)} inverse_operation<=0;pc<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |
       |module YataMicroLane(input [3:0] kind,input signed [53:0] a,input signed [53:0] b,input signed [26:0] constant,input [1:0] radix,input [1:0] number,output reg signed [53:0] out_a,output reg signed [53:0] out_b);
       |  localparam signed [53:0] P=54'sd40960001;
       |  function automatic signed [26:0] addmod(input signed [53:0] x,input signed [53:0] y);reg signed [53:0]v;begin v=x+y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;addmod=v;end endfunction
       |  function automatic signed [26:0] submod(input signed [53:0] x,input signed [53:0] y);reg signed [53:0]v;begin v=x-y;if(v>=P)v=v-P;else if(v<=-P)v=v+P;submod=v;end endfunction
       |  function automatic signed [26:0] sredc(input signed [53:0] x);reg[26:0]a0;reg signed[26:0]a1,m,t1;reg signed[53:0]mw,tw;begin a0=x[26:0];a1=x[53:27];mw=-(({27'd0,a0}*625)<<<16)+{27'd0,a0};m=mw[26:0];tw=(($$signed(m)*625)<<<16)+$$signed(m);t1=tw[53:27];sredc=a1-t1;end endfunction
       |  function automatic signed [26:0] mulredc(input signed [26:0] x,input signed [26:0] y);begin mulredc=sredc($$signed(x)*$$signed(y));end endfunction
       |  function automatic signed [53:0] cmul(input signed [53:0] x,input[1:0]r,input[1:0]n);begin if(r==2&&n==1)cmul=(x*25)<<<8;else if(r==3&&n==1)cmul=(x*5)<<<4;else if(r==3&&n==2)cmul=(x*25)<<<8;else if(r==3&&n==3)cmul=(x*125)<<<12;else cmul=x;end endfunction
       |  always @(*) begin out_a=0;out_b=0;case(kind)
       |    1:begin out_a=addmod(a,b);out_b=submod(a,b);end 2:begin out_a=addmod(a,b);out_b=a-b;end 3:begin out_a=sredc(a+b);out_b=sredc(a-b);end
       |    4:out_a=cmul(a,radix,number);5:begin out_a=a+cmul(b,3,2);out_b=a-cmul(b,3,2);end 6:begin out_a=cmul(a,3,1)+cmul(b,3,3);out_b=cmul(a,3,3)+cmul(b,3,1);end
       |    7:begin out_a=a-cmul(b,2,1);out_b=a+cmul(b,2,1);end 8:begin out_a=addmod(a,b);out_b=-cmul(a-b,3,2);end 9:begin out_a=-cmul(a,3,3)-cmul(b,3,1);out_b=-cmul(a,3,1)-cmul(b,3,3);end
       |    10:out_a=mulredc(a[26:0],constant);default:begin end endcase end
       |endmodule
       |
       |module YataModSwitch(input signed [53:0] value,output [31:0] torus);
       |  localparam signed [53:0] P=54'sd40960001; localparam [63:0] SCALE=64'd7036874245;
       |  reg signed [26:0] residue; reg [63:0] positive; reg [95:0] scaled;
       |  always @(*) begin residue=value[26:0];positive=(residue<0)?residue+P:residue;scaled=((positive*SCALE)+96'd33554432)>>26;end
       |  assign torus=scaled[31:0];
       |endmodule
       |/* verilator lint_on WIDTHTRUNC */
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin
