package ngen.backend

import ngen.arithmetic.YataField
import ngen.rtl.SwitchTransposeSpec
import scala.collection.mutable

/** Fixed-direction 64-lane YATA512 pipeline. The lane and time transpose
  * factors the 512-point transform into one radix-8 pass and one radix-64
  * pass, as in YATA. Arithmetic registers follow dependencies, not frames.
  * There is no 512-element combinational transform or shared direction mux.
  */
object YataStreamingSystemVerilog:
  final case class Design(source: String, latency: Int)
  private case class Signal(name: String, time: Int, width: Int)
  private def literal(value: Long, width: Int = 27): String =
    if value < 0 then s"(-${width}'sd${-value})" else s"${width}'sd$value"
  private def reverse3(i: Int): Int = ((i & 1) << 2) | (i & 2) | ((i & 4) >> 2)

  // Small latency-aware SSA builder. Payload registers have no reset; only
  // validity is reset, permitting DSP and SRL inference and local placement.
  private class Pipeline:
    private val declarations = mutable.ArrayBuffer.empty[String]
    private val clocked = mutable.ArrayBuffer.empty[String]
    private val combinational = mutable.ArrayBuffer.empty[String]
    private val delays = mutable.Map.empty[(String, Int), Signal]
    private var serial = 0
    val input: Array[Signal] = Array.tabulate(64) { i =>
      declarations += s"wire signed [26:0] input_$i = data_in[$i*27+:27];"
      Signal(s"input_$i",0,27)
    }
    private def fresh(): String =
      serial += 1
      s"s$serial"
    def delay(a: Signal, time: Int): Signal =
      require(time >= a.time)
      if time == a.time then a
      else delays.getOrElseUpdate((a.name, time), {
        val previous = delay(a, time - 1)
        register(previous.time, a.width, previous.name)
      })
    private def register(time: Int, width: Int, expression: String): Signal =
      val name = fresh()
      declarations += s"reg signed [${width-1}:0] $name;"
      clocked += s"$name <= $expression;"
      Signal(name, time + 1, width)
    def operation(width: Int, operands: Signal*)(expression: Seq[String] => String): Signal =
      val time = operands.map(_.time).max
      val names = operands.map(a => delay(a, time).name)
      register(time, width, expression(names))
    def add(a: Signal, b: Signal): Signal = operation(54, a, b)(s => s"${s(0)} + ${s(1)}")
    def sub(a: Signal, b: Signal): Signal = operation(54, a, b)(s => s"${s(0)} - ${s(1)}")
    def negate(a: Signal): Signal = operation(54, a)(s => s"-${s.head}")
    def correction(a: Signal): Signal = operation(27, a)(s =>
      s"(${s.head} >= 54'sd40960001) ? ${s.head}-54'sd40960001 : (${s.head} <= -54'sd40960001) ? ${s.head}+54'sd40960001 : ${s.head}")
    def addMod(a: Signal, b: Signal): Signal = correction(add(a,b))
    def subMod(a: Signal, b: Signal): Signal = correction(sub(a,b))
    def cmul(a: Signal, number: Int): Signal =
      val factor = math.pow(5,number).toLong
      operation(54,a)(s => s"(${s.head} * ${literal(factor,54)}) <<< ${number*4}")
    def sredc(a: Signal): Signal =
      val m = operation(27,a)(s => s"${s.head}[26:0] - ((${s.head}[26:0]*27'd625)<<16)")
      val product = operation(54,m)(s => s"${s.head} * 54'sd625")
      val term = operation(54,product,m)(s => s"(${s(0)} <<< 16) + ${s(1)}")
      operation(27,a,term)(s => s"$$signed(${s(0)}[53:27]) - $$signed(${s(1)}[53:27])")
    def multiply(a: Signal, b: Signal): Signal =
      // Two signed DSP48E2-sized partial products, exact even at -2^26.
      val lo = operation(45,a,b)(s => s"$$signed(${s(0)}[26:0]) * $$signed({1'b0,${s(1)}[16:0]})")
      val hi = operation(37,a,b)(s => s"$$signed(${s(0)}[26:0]) * $$signed(${s(1)}[26:17])")
      val product = operation(54,lo,hi)(s => s"{{9{${s(0)}[44]}},${s(0)}} + {${s(1)},17'd0}")
      sredc(product)
    def factor(values: Seq[Long], group: Int): Signal =
      require(values.size == 8)
      val name = fresh()
      declarations += s"reg signed [26:0] $name;"
      combinational += s"always @(*) begin case(input_cycle_$group) ${values.zipWithIndex.map((v,i) => s"3'd$i: $name=${literal(v)};").mkString(" ")} endcase end"
      Signal(name,0,27)
    def constant(value: Long): Signal =
      val name=fresh()
      declarations += s"wire signed [26:0] $name=${literal(value)};"
      Signal(name,0,27)
    def emit(name: String, result: Array[Signal]): Design =
      val latency = result.map(_.time).max
      val outputs = result.map(delay(_,latency))
      val assigns = outputs.zipWithIndex.map((s,i) => s"assign data_out[$i*27+:27]=${s.name};").mkString("\n")
      Design(s"""module $name(input clock,input reset,input valid_in,input [1727:0] data_in,output valid_out,output [1727:0] data_out);
         |${(0 until 8).map(g=>s"(* DONT_TOUCH=\"TRUE\", SHREG_EXTRACT=\"NO\" *) reg [2:0] input_cycle_$g;").mkString("\n")}
         |reg [${latency-1}:0] valid_pipe;
         |${declarations.mkString("\n")}
         |${combinational.mkString("\n")}
         |$assigns
         |assign valid_out=valid_pipe[${latency-1}];
         |always @(posedge clock) begin
         |  if(reset)begin ${(0 until 8).map(g=>s"input_cycle_$g<=0;").mkString} valid_pipe<=0;end
         |  else begin ${(0 until 8).map(g=>s"input_cycle_$g<=valid_in ? input_cycle_$g+3'd1 : 3'd0;").mkString} valid_pipe<={valid_pipe[${latency-2}:0],valid_in};end
         |  ${clocked.mkString("\n  ")}
         |end
         |endmodule
         |""".stripMargin,latency)

  private def inverseRadix(p: Pipeline, a: Array[Signal], offset: Int, size: Int, radix: Int): Unit =
    def pairs(start: Int, length: Int, kind: Int): Unit =
      for i <- 0 until length/2 do
        val left = start+i; val right = left+length/2
        val x=a(left);val y=a(right)
        a(left)=if kind==2 then p.sredc(p.add(x,y)) else p.addMod(x,y)
        a(right)=if kind==2 then p.sredc(p.sub(x,y)) else if kind==1 then p.sub(x,y) else p.subMod(x,y)
    if radix==1 then pairs(offset,size,0)
    else if radix==2 then
      pairs(offset,size,1)
      pairs(offset,size/2,0)
      for i <- 0 until size/4 do a(offset+3*size/4+i)=p.cmul(a(offset+3*size/4+i),2)
      pairs(offset+size/2,size/2,2)
    else
      pairs(offset,size,1)
      inverseRadix(p,a,offset,size/2,2)
      val block=size/8
      for i <- 0 until block do
        val left=offset+size/2+i;val right=left+2*block
        val x=a(left);val y=p.cmul(a(right),2)
        a(left)=p.add(x,y);a(right)=p.sub(x,y)
        val l=left+block;val r=l+2*block
        val u=a(l);val v=a(r)
        a(l)=p.add(p.cmul(u,1),p.cmul(v,3))
        a(r)=p.add(p.cmul(u,3),p.cmul(v,1))
      pairs(offset+size/2,size/4,2)
      pairs(offset+3*size/4,size/4,2)

  private def forwardRadix(p: Pipeline, a: Array[Signal], offset: Int, size: Int): Unit =
    def pair(left: Int, right: Int): Unit =
      val x=a(left);val y=a(right)
      a(left)=p.addMod(x,y);a(right)=p.subMod(x,y)
    val block=size/8
    for group <- 0 until 4; i <- 0 until block do
      pair(offset+2*group*block+i,offset+(2*group+1)*block+i)
    for i <- 0 until block do pair(offset+i,offset+i+size/4)
    for i <- block until 2*block do
      val left=offset+i;val right=left+size/4
      val x=a(left);val y=p.negate(p.cmul(a(right),2))
      a(left)=p.add(x,y);a(right)=p.sub(x,y)
    for i <- 0 until block do
      val left=offset+size/2+i;val right=left+size/4
      val x=a(left);val y=a(right)
      a(left)=p.addMod(x,y);a(right)=p.negate(p.cmul(p.sub(x,y),2))
      val l=left+block;val r=right+block
      val u=a(l);val v=a(r)
      a(l)=p.negate(p.add(p.cmul(u,3),p.cmul(v,1)))
      a(r)=p.negate(p.add(p.cmul(u,1),p.cmul(v,3)))
    for i <- 0 until block do pair(offset+i,offset+size/2+i)
    for i <- block until size/2 do
      val left=offset+i;val right=left+size/2
      val x=a(left);val y=a(right)
      a(left)=p.sredc(p.add(x,y));a(right)=p.sredc(p.sub(x,y))

  private def radix64(inverse: Boolean, name: String): Design =
    val p=new Pipeline
    val a=p.input.clone()
    val tables=YataField.tables(6)
    if inverse then
      inverseRadix(p,a,0,64,3)
      for j <- 1 until 8; i <- 0 until 8 do
        val table=if j>1 then tables.inttTable1 else tables.inttTable0
        a(j*8+i)=p.multiply(a(j*8+i),p.constant(table(reverse3(j)*i)))
      for i <- 0 until 8 do inverseRadix(p,a,i*8,8,3)
    else
      for i <- 0 until 8 do forwardRadix(p,a,i*8,8)
      for j <- 0 until 8; i <- 0 until 8 do
        val one=(i&3)!=0
        if j!=0 || one then
          val table=if one then tables.nttTable1 else tables.nttTable0
          a(j*8+i)=p.multiply(a(j*8+i),p.constant(if j==0 then YataField.R2 else table(reverse3(j)*i)))
      forwardRadix(p,a,0,64)
    p.emit(name,a)

  private def radix8Pass(inverse: Boolean, name: String): Design =
    val p=new Pipeline
    val a=p.input.clone()
    val tables=YataField.tables(9)
    if inverse then
      for lane <- 0 until 64 do
        a(lane)=p.multiply(a(lane),p.factor(Vector.tabulate(8)(cycle=>tables.inttTwist(lane*8+cycle)),lane/8))
      inverseRadix(p,a,0,64,3)
      for j <- 1 until 8; i <- 0 until 8 do
        val table=if j>1 then tables.inttTable1 else tables.inttTable0
        a(j*8+i)=p.multiply(a(j*8+i),p.factor(Vector.tabulate(8)(cycle=>table(reverse3(j)*(i*8+cycle))),j))
    else
      for j <- 0 until 8; i <- 0 until 8 do
        val one=(i&3)!=0
        if j!=0 || one then
          val table=if one then tables.nttTable1 else tables.nttTable0
          a(j*8+i)=p.multiply(a(j*8+i),p.factor(Vector.tabulate(8)(cycle=>if j==0 then YataField.R2 else table(reverse3(j)*(i*8+cycle))),j))
      forwardRadix(p,a,0,64)
      for lane <- 0 until 64 do
        a(lane)=p.multiply(a(lane),p.factor(Vector.tabulate(8)(cycle=>tables.nttTwist(lane*8+cycle)),lane/8))
    p.emit(name,a)

  def emit(top: String, inverse: Boolean): Design =
    require(top.matches("[A-Za-z_][A-Za-z0-9_]*"))
    val former=if inverse then radix8Pass(true,top+"Former") else radix64(false,top+"Former")
    val later=if inverse then radix64(true,top+"Later") else radix8Pass(false,top+"Later")
    val transpose=SwitchTransposeSystemVerilog.definitions(SwitchTransposeSpec(3,27),top+"T",resetData=false)
    val links=(0 until 8).map { i =>
      val inputs=(0 until 8).map { j =>
        val lane=if inverse then j*8+i else i*8+j
        s"assign transpose_in_$i[$j*27+:27]=former_data[$lane*27+:27];"
      }.mkString("\n")
      val outputs=(0 until 8).map { j =>
        val lane=if inverse then i*8+j else j*8+i
        s"assign later_input[$lane*27+:27]=transpose_out_$i[$j*27+:27];"
      }.mkString("\n")
      s"""wire [215:0] transpose_in_$i,transpose_out_$i;wire transpose_valid_$i;
         |$inputs
         |${top}TNGenSwitchTransposeNetwork_3 transpose_$i(clock,reset,former_valid,transpose_in_$i,transpose_valid_$i,transpose_out_$i);
         |$outputs""".stripMargin
    }.mkString("\n")
    val inputBits=if inverse then 2048 else 1728
    val outputBits=if inverse then 1728 else 2048
    val inputAssignments=(0 until 64).map(i=>s"assign former_input[$i*27+:27]=data_in[$i*${if inverse then 32 else 27}+:27];").mkString("\n")
    val conversion=if inverse then "assign data_out=later_data;assign valid_out=later_valid;"
      else (0 until 64).map(i=>s"wire conversion_valid_$i;${top}ModSwitch conversion_$i(clock,reset,later_valid,{{27{later_data[$i*27+26]}},later_data[$i*27+:27]},conversion_valid_$i,data_out[$i*32+:32]);").mkString("\n")+"\nassign valid_out=conversion_valid_0;"
    val conversionDefinition=if inverse then "" else YataMicroLanePipeline.outputConversionUnreset.replace("YataModSwitchPipeline",top+"ModSwitch")
    Design(s"""// Generated fixed-direction YATA512 pipeline. Eight valid cycles/frame;
       |// frames may be adjacent or separated by gaps, but not contain bubbles.
       |// INTT: input[cycle,lane]=polynomial[lane*8+cycle], output=spectrum[cycle*64+lane].
       |// NTT: input=spectrum[cycle*64+lane], output=torus[lane*8+cycle].
       |${former.source}
       |${later.source}
       |$transpose
       |$conversionDefinition
       |module $top(input clock,input reset,input valid_in,input [${inputBits-1}:0] data_in,output valid_out,output [${outputBits-1}:0] data_out);
       |wire [1727:0] former_input,former_data,later_input,later_data;
       |wire former_valid,later_valid;
       |$inputAssignments
       |${top}Former former(clock,reset,valid_in,former_input,former_valid,former_data);
       |$links
       |${top}Later later(clock,reset,transpose_valid_0,later_input,later_valid,later_data);
       |$conversion
       |endmodule
       |""".stripMargin,former.latency+7+later.latency+(if inverse then 0 else 4))
