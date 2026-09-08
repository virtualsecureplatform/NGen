package ngen.backend

import ngen.algebra.Domains
import ngen.transform.KyberNtt
import ngen.rtl.{IndexedOperation, MicroProgram}

object KyberSystemVerilog:
  val ForwardCycles = 903
  val InverseCycles = 903
  private val domain = Domains.Kyber256
  private val zetas = KyberNtt.zetas(domain).map(_.toInt)
  private def montgomeryConstant(value: Int): Int = ((BigInt(value) * (BigInt(1) << 16)) % 3329).toInt
  private def lines(values: Seq[String], spaces: Int): String = values.map(" " * spaces + _).mkString("\n")

  private final case class MicroOp(kind: Int, left: Int, right: Int, constant: Int) extends IndexedOperation:
    override val indices: Set[Int] = if kind == 3 then Set(left) else Set(left, right)

  private def forwardProgram: Vector[MicroOp] =
    var constantIndex = 1
    var result = Vector.empty[MicroOp]
    var length = 128
    while length >= 2 do
      var start = 0
      while start < 256 do
        val zeta = zetas(constantIndex)
        constantIndex += 1
        for index <- start until start + length do result :+= MicroOp(1, index, index + length, zeta)
        start += 2 * length
      length /= 2
    result

  private def inverseProgram: Vector[MicroOp] =
    var constantIndex = 127
    var result = Vector.empty[MicroOp]
    var length = 2
    while length <= 128 do
      var start = 0
      while start < 256 do
        val zeta = zetas(constantIndex)
        constantIndex -= 1
        for index <- start until start + length do result :+= MicroOp(2, index, index + length, (zeta * 1665) % 3329)
        start += 2 * length
      length *= 2
    // Divide both butterfly outputs by two in every layer: seven layers
    // provide the required 1/128 normalization without a final scaling pass.
    result

  def emit(top: String = "KyberHPM1PE", banked: Boolean = false): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val forward = MicroProgram.schedule(forwardProgram, 1).bundles.flatten
    val inverse = MicroProgram.schedule(inverseProgram, 1).bundles.flatten
    // Fetch/read/preprocess/product/correction/MAC/reduce, then retirement.
    // A synchronous read cannot consume a same-edge nonblocking write.
    for program <- Vector(forward,inverse) do
      val lastWrite = scala.collection.mutable.Map.empty[Int,Int]
      program.zipWithIndex.foreach { (operation,index) =>
        operation.indices.foreach { address =>
          require(lastWrite.get(address).forall(previous => index-previous >= 8),
            s"Kyber pipeline RAW distance is too short at instruction $index, address $address")
        }
        operation.indices.foreach(address => lastWrite(address)=index)
      }
    val forwardRom = forward.zipWithIndex.map { case (op, index) =>
      s"f_kind[$index]=2'd${op.kind}; f_left[$index]=8'd${op.left}; f_right[$index]=8'd${op.right}; f_constant[$index]=12'd${montgomeryConstant(op.constant)};"
    }
    val inverseRom = inverse.zipWithIndex.map { case (op, index) =>
      s"i_kind[$index]=2'd${op.kind}; i_left[$index]=8'd${op.left}; i_right[$index]=8'd${op.right}; i_constant[$index]=12'd${montgomeryConstant(op.constant)};"
    }
    def memoryName(buffer: Int, bank: Int): String = s"coefficient_${buffer}_$bank"
    val compactDeclarations = (for buffer <- 0 until 3; bank <- 0 until 2 yield
      s"(* ram_style = \"block\" *) reg [11:0] ${memoryName(buffer,bank)}[0:127];reg ${memoryName(buffer,bank)}_ae,${memoryName(buffer,bank)}_be,${memoryName(buffer,bank)}_bw;reg [6:0] ${memoryName(buffer,bank)}_aa,${memoryName(buffer,bank)}_ba;reg [11:0] ${memoryName(buffer,bank)}_bd,${memoryName(buffer,bank)}_aq,${memoryName(buffer,bank)}_bq;"
    ).mkString("\n")
    val memoryAlways = (for buffer <- 0 until 3; bank <- 0 until 2 yield
      s"always @(posedge clk)begin if(${memoryName(buffer,bank)}_ae)${memoryName(buffer,bank)}_aq<=${memoryName(buffer,bank)}[${memoryName(buffer,bank)}_aa];if(${memoryName(buffer,bank)}_be)begin if(${memoryName(buffer,bank)}_bw)${memoryName(buffer,bank)}[${memoryName(buffer,bank)}_ba]<=${memoryName(buffer,bank)}_bd;${memoryName(buffer,bank)}_bq<=${memoryName(buffer,bank)}[${memoryName(buffer,bank)}_ba];end end"
    ).mkString("\n")
    def readMux(port: String, selector: String, bank: String): String =
      (0 until 3).map(buffer => s"($selector==2'd$buffer)?($bank?${memoryName(buffer,1)}_${port}q:${memoryName(buffer,0)}_${port}q):").mkString+"12'd0"
    val memoryDefaults = (for buffer <- 0 until 3; bank <- 0 until 2 yield
      s"${memoryName(buffer,bank)}_ae=0;${memoryName(buffer,bank)}_be=0;${memoryName(buffer,bank)}_bw=0;${memoryName(buffer,bank)}_aa=0;${memoryName(buffer,bank)}_ba=0;${memoryName(buffer,bank)}_bd=0;"
    ).mkString
    val memoryRouting = (for buffer <- 0 until 3; bank <- 0 until 2 yield
      s"""if(pipe_valid[0] && source_pointer==2'd$buffer)begin ${memoryName(buffer,bank)}_ae=1;${memoryName(buffer,bank)}_aa=(^pipe_left[0][7:1])==1'b$bank?{pipe_left[0][7:2],pipe_left[0][0]}:{pipe_right[0][7:2],pipe_right[0][0]};end
         |if(executing && pipe_valid[6] && work_pointer==2'd$buffer)begin ${memoryName(buffer,bank)}_be=1;${memoryName(buffer,bank)}_bw=1;${memoryName(buffer,bank)}_ba=(^pipe_left[6][7:1])==1'b$bank?{pipe_left[6][7:2],pipe_left[6][0]}:{pipe_right[6][7:2],pipe_right[6][0]};${memoryName(buffer,bank)}_bd=(^pipe_left[6][7:1])==1'b$bank?write_left:write_right;end
         |if(host_load && load_pointer==2'd$buffer && (^load_address[7:1])==1'b$bank)begin ${memoryName(buffer,bank)}_be=1;${memoryName(buffer,bank)}_bw=1;${memoryName(buffer,bank)}_ba={load_address[7:2],load_address[0]};${memoryName(buffer,bank)}_bd=din;end
         |if(host_prefetch && prefetch_pointer==2'd$buffer && (^prefetch_address[7:1])==1'b$bank)begin ${memoryName(buffer,bank)}_be=1;${memoryName(buffer,bank)}_bw=0;${memoryName(buffer,bank)}_ba={prefetch_address[7:2],prefetch_address[0]};end""".stripMargin
    ).mkString("\n")
    val decodeCases = (0 until 7).map { stage =>
      def body(inverse: Boolean): String =
        val shift=if inverse then stage+1 else 7-stage
        val length=1<<shift
        val prefix=if inverse then (1<<(7-stage))-1 else 1<<stage
        s"decoded_left=((pc[6:0]>>$shift)<<${shift+1})|(pc[6:0]&8'd${length-1});decoded_right=decoded_left|8'd$length;decoded_twiddle=8'd$prefix${if inverse then "-" else "+"}(pc[6:0]>>$shift);"
      s"$stage:begin if(operation_inverse)begin ${body(true)} end else begin ${body(false)} end end"
    }.mkString("\n")
    if banked then
      for (program,isInverse) <- Vector(forward->false,inverse->true); (operation,index) <- program.zipWithIndex do
        val stage=index/128;val ordinal=index%128;val shift=if isInverse then stage+1 else 7-stage
        val length=1<<shift;val block=ordinal>>shift
        val left=(block<<(shift+1))|(ordinal&(length-1));val right=left|length
        val twiddle=if isInverse then (1<<(7-stage))-1-block else (1<<stage)+block
        val coefficient=if isInverse then (zetas(twiddle)*1665)%3329 else zetas(twiddle)
        require(operation.left==left && operation.right==right && operation.constant==coefficient,"compact Kyber decoder differs from microprogram")
        require((Integer.bitCount(left>>1)&1)!=(Integer.bitCount(right>>1)&1),"compact Kyber bank conflict")
    val compactLogic = s"""
       |reg [1:0] bank_a_pointer,bank_b_pointer,work_pointer,operation_pointer,source_pointer,read_pointer_q,host_pointer_q;
       |reg read_bank_q,host_bank_q;
       |reg [7:0] decoded_left,decoded_right,decoded_twiddle;
       |reg [11:0] twiddles[0:127];
       |wire [1:0] load_pointer=load_bank_b?bank_b_pointer:bank_a_pointer;
       |wire host_load=load_active && !(load_a_f||load_a_i||load_b_f||load_b_i);
       |wire [7:0] load_address=load_inverse?{load_count[7:2],load_count[0],load_count[1]}:load_count[7:0];
       |wire [11:0] write_left=pipe_inverse[6]?pipe_pass[6]:kyber_add(pipe_pass[6],reduced_result);
       |wire [11:0] write_right=pipe_inverse[6]?reduced_result:kyber_sub(pipe_pass[6],reduced_result);
       |wire host_prefetch=read_active && ((read_delay>0)||(read_count<254));
       |wire [7:0] prefetch_sample=read_delay==2?8'd0:(read_delay==1?8'd1:read_count[7:0]+8'd2);
       |wire [7:0] prefetch_address=read_inverse?{prefetch_sample[0],prefetch_sample[7:1]}:{prefetch_sample[7:2],prefetch_sample[0],prefetch_sample[1]};
       |wire [1:0] prefetch_pointer=(finishing && read_bank_b==operation_bank_b)?work_pointer:(read_bank_b?bank_b_pointer:bank_a_pointer);
       |wire [11:0] operand_a=${readMux("a","read_pointer_q","read_bank_q")};
       |wire [11:0] operand_b=${readMux("a","read_pointer_q","!read_bank_q")};
       |wire [11:0] host_data=${readMux("b","host_pointer_q","host_bank_q")};
       |always @(*)begin decoded_left=0;decoded_right=0;decoded_twiddle=0;case(pc[9:7])$decodeCases default:begin end endcase end
       |always @(*)begin $memoryDefaults $memoryRouting end
       |$memoryAlways
       |""".stripMargin
    s"""// Generated by NGen from the seven-layer Kyber incomplete NTT plan.
       |/* verilator lint_off BLKSEQ */
       |/* verilator lint_off UNUSEDSIGNAL */
       |/* verilator lint_off WIDTHEXPAND */
       |/* verilator lint_off WIDTHTRUNC */
       |module $top(
       |  input clk,
       |  input reset,
       |  input load_a_f,
       |  input load_a_i,
       |  input load_b_f,
       |  input load_b_i,
       |  input read_a,
       |  input read_b,
       |  input start_ab,
       |  input start_fntt,
       |  input start_pwm2,
       |  input start_intt,
       |  input [11:0] din,
       |  output reg [11:0] dout,
       |  output reg done
       |);
       |  localparam [12:0] KYBER_Q = 13'd3329;
       |  localparam [15:0] KYBER_QINV = 16'd3327;
       |  ${if banked then compactDeclarations else "reg [11:0] bank_a [0:255];reg [11:0] bank_b [0:255];reg [11:0] work [0:255];"}
       |  localparam integer FORWARD_LENGTH = ${forward.size};
       |  localparam integer INVERSE_LENGTH = ${inverse.size};
       |  ${if banked then "" else s"reg [1:0] f_kind[0:FORWARD_LENGTH-1],i_kind[0:INVERSE_LENGTH-1];reg [7:0] f_left[0:FORWARD_LENGTH-1],f_right[0:FORWARD_LENGTH-1],i_left[0:INVERSE_LENGTH-1],i_right[0:INVERSE_LENGTH-1];reg [11:0] f_constant[0:FORWARD_LENGTH-1],i_constant[0:INVERSE_LENGTH-1];"}
       |  reg load_active, load_inverse, load_bank_b;
       |  reg read_active, read_inverse, read_bank_b, last_inverse, executing, operation_inverse, operation_bank_b, finishing;
       |  integer load_count, read_count, read_delay, j, logical_index, pc;
       |  reg issued_all;integer retired_count;
       |  reg [6:0] pipe_valid,pipe_inverse;
       |  reg [7:0] pipe_left[0:6],pipe_right[0:6];
       |  reg [11:0] pipe_constant[0:2],pipe_pass[2:6];
       |  ${if banked then "" else "reg [11:0] operand_a,operand_b;"}reg [11:0] multiply_input,reduced_result;
       |  reg [23:0] product,product_delayed;
       |  ${if banked then "(* use_dsp = \"no\" *)" else ""}reg [15:0] correction;
       |  ${if banked then "(* use_dsp = \"no\" *)" else ""}reg [28:0] reduction_sum;
       |
       |  function automatic [11:0] kyber_add(input [11:0] a,input [11:0] b);
       |    reg [12:0] sum; begin sum={1'b0,a}+{1'b0,b}; if(sum>=KYBER_Q) sum=sum-KYBER_Q; kyber_add=sum[11:0]; end
       |  endfunction
       |  function automatic [11:0] kyber_half(input [11:0] value);
       |    reg [12:0] even_value;begin even_value={1'b0,value}+(value[0]?KYBER_Q:13'd0);kyber_half=even_value[12:1];end
       |  endfunction
       |  function automatic [11:0] kyber_sub(input [11:0] a,input [11:0] b);
       |    reg [12:0] value; begin if(a>=b) value=a-b; else value={1'b0,a}+KYBER_Q-b; kyber_sub=value[11:0]; end
       |  endfunction
       |  function automatic [11:0] kyber_mul(input [11:0] a,input [11:0] b_montgomery);
       |    reg [23:0] product; reg [39:0] qinv_product; reg [15:0] correction; reg [28:0] sum; reg [12:0] reduced;
       |    begin product=a*b_montgomery; qinv_product=product*KYBER_QINV; correction=qinv_product[15:0]; sum={5'd0,product}+correction*KYBER_Q; reduced=sum[28:16]; if(reduced>=KYBER_Q) reduced=reduced-KYBER_Q; kyber_mul=reduced[11:0]; end
       |  endfunction
       |
       |  initial begin
       |${if banked then lines(zetas.zipWithIndex.map((value,index)=>s"twiddles[$index]=12'd${montgomeryConstant(value)};"),4) else lines(forwardRom++inverseRom,4)}
       |  end
       |
       |  ${if banked then compactLogic else ""}
       |  always @(posedge clk) begin
       |    if(reset) begin
       |      dout<=0; done<=0; load_active<=0; read_active<=0; last_inverse<=0; executing<=0; finishing<=0; pc<=0; load_count<=0; read_count<=0; read_delay<=0;pipe_valid<=0;issued_all<=0;retired_count<=0;
       |      ${if banked then "bank_a_pointer<=0;bank_b_pointer<=1;work_pointer<=2;" else Vector.tabulate(256)(j => s"bank_a[$j]<=0; bank_b[$j]<=0; work[$j]<=0;").mkString("\n")}
       |    end else begin
       |      done<=0;
       |      pipe_valid<={pipe_valid[5:0],1'b0};
       |      for(j=1;j<7;j=j+1)begin pipe_left[j]<=pipe_left[j-1];pipe_right[j]<=pipe_right[j-1];pipe_inverse[j]<=pipe_inverse[j-1];end
       |      for(j=3;j<7;j=j+1)pipe_pass[j]<=pipe_pass[j-1];
       |      if(pipe_valid[0])begin ${if banked then "read_pointer_q<=source_pointer;read_bank_q<=^pipe_left[0][7:1];" else "operand_a<=work[pipe_left[0]];operand_b<=work[pipe_right[0]];"}pipe_constant[1]<=pipe_constant[0];end
       |      ${if banked then "if(host_prefetch)begin host_pointer_q<=prefetch_pointer;host_bank_q<=^prefetch_address[7:1];end" else ""}
       |      if(pipe_valid[1])begin
       |        multiply_input<=pipe_inverse[1]?kyber_sub(operand_b,operand_a):operand_b;
       |        pipe_pass[2]<=pipe_inverse[1]?kyber_half(kyber_add(operand_a,operand_b)):operand_a;
       |        pipe_constant[2]<=pipe_constant[1];
       |      end
       |      if(pipe_valid[2])product<=multiply_input*pipe_constant[2];
       |      if(pipe_valid[3])begin correction<=product[15:0]*KYBER_QINV;product_delayed<=product;end
       |      if(pipe_valid[4])reduction_sum<={5'd0,product_delayed}+correction*KYBER_Q;
       |      if(pipe_valid[5])reduced_result<=(reduction_sum[28:16]>=KYBER_Q)?reduction_sum[28:16]-KYBER_Q:reduction_sum[28:16];
       |      if(load_a_f||load_a_i||load_b_f||load_b_i) begin load_active<=1; load_count<=0; load_inverse<=load_a_i||load_b_i; load_bank_b<=load_b_f||load_b_i; end
       |      else if(load_active) begin
       |        if(load_inverse) begin case(load_count[1:0]) 2'd0:logical_index=load_count; 2'd1:logical_index=load_count+1; 2'd2:logical_index=load_count-1; default:logical_index=load_count; endcase end else logical_index=load_count;
       |        ${if banked then "" else "if(load_bank_b) bank_b[logical_index]<=din; else bank_a[logical_index]<=din;"}
       |        if(load_count==255) begin load_active<=0; load_count<=0; end else load_count<=load_count+1;
       |      end
       |      if(start_fntt||start_intt) begin
       |        ${if banked then "operation_pointer<=start_ab?bank_b_pointer:bank_a_pointer;" else Vector.tabulate(256)(j => s"work[$j]<=start_ab?bank_b[$j]:bank_a[$j];").mkString("\n")}
       |        operation_inverse<=start_intt; operation_bank_b<=start_ab; last_inverse<=start_intt; pc<=0; executing<=1;pipe_valid<=0;issued_all<=0;retired_count<=0;
       |      end else if(executing) begin
       |        if(!issued_all)begin
       |          pipe_valid[0]<=1;pipe_inverse[0]<=operation_inverse;
       |          pipe_left[0]<=${if banked then "decoded_left" else "operation_inverse?i_left[pc]:f_left[pc]"};
       |          pipe_right[0]<=${if banked then "decoded_right" else "operation_inverse?i_right[pc]:f_right[pc]"};
       |          pipe_constant[0]<=${if banked then "operation_inverse?kyber_half(twiddles[decoded_twiddle]):twiddles[decoded_twiddle]" else "operation_inverse?i_constant[pc]:f_constant[pc]"};
       |          ${if banked then "source_pointer<=pc<128?operation_pointer:work_pointer;" else ""}
       |          if(pc==(operation_inverse?INVERSE_LENGTH:FORWARD_LENGTH)-1)begin issued_all<=1;pc<=0;end else pc<=pc+1;
       |        end
       |        if(pipe_valid[6])begin
       |          ${if banked then "" else "work[pipe_left[6]]<=pipe_inverse[6]?pipe_pass[6]:kyber_add(pipe_pass[6],reduced_result);work[pipe_right[6]]<=pipe_inverse[6]?reduced_result:kyber_sub(pipe_pass[6],reduced_result);"}
       |          if(retired_count==(operation_inverse?INVERSE_LENGTH:FORWARD_LENGTH)-1)begin executing<=0;finishing<=1;end
       |          else retired_count<=retired_count+1;
       |        end
       |      end else if(finishing) begin ${if banked then "if(operation_bank_b)begin bank_b_pointer<=work_pointer;work_pointer<=bank_b_pointer;end else begin bank_a_pointer<=work_pointer;work_pointer<=bank_a_pointer;end" else Vector.tabulate(256)(j => s"if(operation_bank_b) bank_b[$j]<=work[$j]; else bank_a[$j]<=work[$j];").mkString("\n")} finishing<=0; done<=1; end
       |      if(read_a||read_b) begin read_active<=1; read_count<=0; read_delay<=2; read_bank_b<=read_b; read_inverse<=last_inverse; end
       |      else if(read_active) begin
       |        if(read_delay>0) begin read_delay<=read_delay-1; if(read_delay==1) begin logical_index=read_inverse?(read_count[0]?128+(read_count>>1):(read_count>>1)):((read_count>>2)*4+(read_count[1:0]==1?2:(read_count[1:0]==2?1:read_count[1:0]))); dout<=${if banked then "host_data" else "read_bank_b?bank_b[logical_index]:bank_a[logical_index]"}; end end
       |        else begin if(read_count==255) begin read_active<=0; read_count<=0; end else begin read_count<=read_count+1; logical_index=read_inverse?((read_count+1)&1?128+((read_count+1)>>1):((read_count+1)>>1)):(((read_count+1)>>2)*4+(((read_count+1)&3)==1?2:(((read_count+1)&3)==2?1:((read_count+1)&3)))); dout<=${if banked then "host_data" else "read_bank_b?bank_b[logical_index]:bank_a[logical_index]"}; end end
       |      end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on WIDTHTRUNC */
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on BLKSEQ */
       |""".stripMargin
