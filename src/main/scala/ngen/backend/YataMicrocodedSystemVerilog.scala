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
    def operation(op: MicroOp): Vector[String] = op.kind match
      case 1 => Vector(s"work[${op.left}]<=yata_add(work[${op.left}],work[${op.right}]);", s"work[${op.right}]<=yata_sub(work[${op.left}],work[${op.right}]);")
      case 2 => Vector(s"work[${op.left}]<=yata_add(work[${op.left}],work[${op.right}]);", s"work[${op.right}]<=work[${op.left}]-work[${op.right}];")
      case 3 => Vector(s"work[${op.left}]<=yata_sredc(work[${op.left}]+work[${op.right}]);", s"work[${op.right}]<=yata_sredc(work[${op.left}]-work[${op.right}]);")
      case 4 => Vector(s"work[${op.left}]<=yata_cmul(work[${op.left}],2'd${op.radixLog},2'd${op.number});")
      case 5 => Vector(s"work[${op.left}]<=work[${op.left}]+yata_cmul(work[${op.right}],3,2);", s"work[${op.right}]<=work[${op.left}]-yata_cmul(work[${op.right}],3,2);")
      case 6 => Vector(s"work[${op.left}]<=yata_cmul(work[${op.left}],3,1)+yata_cmul(work[${op.right}],3,3);", s"work[${op.right}]<=yata_cmul(work[${op.left}],3,3)+yata_cmul(work[${op.right}],3,1);")
      case 7 => Vector(s"work[${op.left}]<=work[${op.left}]-yata_cmul(work[${op.right}],2,1);", s"work[${op.right}]<=work[${op.left}]+yata_cmul(work[${op.right}],2,1);")
      case 8 => Vector(s"work[${op.left}]<=yata_add(work[${op.left}],work[${op.right}]);", s"work[${op.right}]<=-yata_cmul(work[${op.left}]-work[${op.right}],3,2);")
      case 9 => Vector(s"work[${op.left}]<=-yata_cmul(work[${op.left}],3,3)-yata_cmul(work[${op.right}],3,1);", s"work[${op.right}]<=-yata_cmul(work[${op.left}],3,1)-yata_cmul(work[${op.right}],3,3);")
      case 10 => Vector(s"work[${op.left}]<=yata_mul(yata_sword(work[${op.left}]),${lit(op.constant)});")
      case _ => Vector.empty
    def cases(groups: Vector[Vector[MicroOp]]): String = groups.zipWithIndex.map { case (group, pc) =>
      s"$pc: begin\n${lines(group.flatMap(operation),12)}\n          end"
    }.mkString("\n")
    val ports = Vector("input clock", "input reset", "input io_intt_validin") ++ Vector.tabulate(lanes)(i => s"input [31:0] io_intt_in_$i") ++
      Vector.tabulate(lanes)(i => s"output reg [26:0] io_intt_out_$i") ++ Vector("output reg io_intt_validout", "input io_ntt_validin") ++
      Vector.tabulate(lanes)(i => s"input [26:0] io_ntt_in_$i") ++ Vector.tabulate(lanes)(i => s"output reg [31:0] io_ntt_out_$i") ++ Vector("output reg io_ntt_validout")
    val captureI = Vector.tabulate(lanes)(i => s"intt_input[$i*$cycles+input_count]=io_intt_in_$i;")
    val captureN = Vector.tabulate(lanes)(i => s"ntt_input[input_count*$lanes+$i]=io_ntt_in_$i;")
    val initializeI = Vector.tabulate(size)(i => s"work[$i]={{27{1'b0}},intt_input[$i][26:0]};")
    val initializeN = Vector.tabulate(size)(i => s"work[$i]={{27{ntt_input[$i][26]}},ntt_input[$i]};")
    val outputI = Vector.tabulate(lanes)(i => s"io_intt_out_$i<=work[output_count*$lanes+$i][26:0];")
    val outputN = Vector.tabulate(lanes)(i => s"io_ntt_out_$i<=yata_modswitch(work[$i*$cycles+output_count]);")
    s"""// Generated by NGen's microcoded YATA radix-8 backend.
       |/* verilator lint_off BLKSEQ */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off WIDTHEXPAND */
       |/* verilator lint_off WIDTHTRUNC */
       |module $top(
       |${ports.map("  " + _).mkString(",\n")}
       |);
       |  localparam signed [53:0] YATA_P=54'sd40960001; localparam signed [63:0] YATA_SCALE=64'sd7036874245;
       |  localparam integer I_LENGTH=${inverse.size}; localparam integer F_LENGTH=${forward.size}; localparam integer STEP_GAP=${if profile == ProfileName.F300 then 1 else 0};
       |  reg [31:0] intt_input[0:$size-1]; reg signed [26:0] ntt_input[0:$size-1]; reg signed [53:0] work[0:$size-1];
       |  integer i,pc,input_count,output_count,stall_count; reg executing,inverse_operation,finishing,output_intt,output_ntt;
       |  function automatic signed [26:0] yata_sword(input signed [53:0] x); begin yata_sword=x[26:0]; end endfunction
       |  function automatic signed [26:0] yata_add(input signed [53:0] a,input signed [53:0] b); reg signed [53:0] v; begin v=a+b; if(v>=YATA_P)v=v-YATA_P; else if(v<=-YATA_P)v=v+YATA_P; yata_add=v; end endfunction
       |  function automatic signed [26:0] yata_sub(input signed [53:0] a,input signed [53:0] b); reg signed [53:0] v; begin v=a-b; if(v>=YATA_P)v=v-YATA_P; else if(v<=-YATA_P)v=v+YATA_P; yata_sub=v; end endfunction
       |  function automatic signed [26:0] yata_sredc(input signed [53:0] a); reg [26:0] a0; reg signed [26:0] a1,m,t1; reg signed [53:0] mw,tw; begin a0=a[26:0];a1=a[53:27];mw=-(({27'd0,a0}*625)<<<16)+{27'd0,a0};m=mw[26:0];tw=(($$signed(m)*625)<<<16)+$$signed(m);t1=tw[53:27];yata_sredc=a1-t1;end endfunction
       |  function automatic signed [26:0] yata_mul(input signed [26:0] a,input signed [26:0] b); begin yata_mul=yata_sredc($$signed(a)*$$signed(b)); end endfunction
       |  function automatic signed [53:0] yata_cmul(input signed [53:0] a,input [1:0] rb,input [1:0] num); begin if(rb==2&&num==1)yata_cmul=(a*25)<<<8;else if(rb==3&&num==1)yata_cmul=(a*5)<<<4;else if(rb==3&&num==2)yata_cmul=(a*25)<<<8;else if(rb==3&&num==3)yata_cmul=(a*125)<<<12;else yata_cmul=a;end endfunction
       |  function automatic [31:0] yata_modswitch(input signed [53:0] a); reg signed [26:0] r; reg [63:0] p; reg [95:0] s; begin r=yata_sword(a);p=(r<0)?r+YATA_P:r;s=((p*YATA_SCALE)+96'd33554432)>>26;yata_modswitch=s[31:0];end endfunction
       |  always @(posedge clock) begin
       |    if(reset)begin io_intt_validout<=0;io_ntt_validout<=0;input_count<=0;output_count<=0;pc<=0;stall_count<=0;executing<=0;finishing<=0;output_intt<=0;output_ntt<=0;end else begin io_intt_validout<=0;io_ntt_validout<=0;
       |      if(executing)begin if(stall_count>0)stall_count<=stall_count-1;else begin
       |        if(inverse_operation)begin case(pc)
       |${cases(inverse)}
       |        endcase end else begin case(pc)
       |${cases(forward)}
       |        endcase end
       |        if(pc==(inverse_operation?I_LENGTH:F_LENGTH)-1)begin pc<=0;executing<=0;finishing<=1;end else begin pc<=pc+1;stall_count<=STEP_GAP;end end
       |      end else if(finishing)begin finishing<=0;if(inverse_operation)output_intt<=1;else output_ntt<=1;output_count<=0;
       |      end else if(output_intt)begin io_intt_validout<=1;${lines(outputI,8)} if(output_count==$cycles-1)begin output_intt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(output_ntt)begin io_ntt_validout<=1;${lines(outputN,8)} if(output_count==$cycles-1)begin output_ntt<=0;output_count<=0;end else output_count<=output_count+1;
       |      end else if(io_intt_validin)begin ${lines(captureI,8)} if(input_count==$cycles-1)begin input_count<=0;${lines(initializeI,8)} inverse_operation<=1;pc<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1;
       |      end else if(io_ntt_validin)begin ${lines(captureN,8)} if(input_count==$cycles-1)begin input_count<=0;${lines(initializeN,8)} inverse_operation<=0;pc<=0;stall_count<=0;executing<=1;end else input_count<=input_count+1; end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on WIDTHTRUNC */
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin
