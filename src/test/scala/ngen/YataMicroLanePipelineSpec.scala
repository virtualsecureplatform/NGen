package ngen

import ngen.backend.{YataMicrocodedSystemVerilog,YataMicroLanePipeline}
import ngen.rtl.ProfileName
import org.scalatest.funsuite.AnyFunSuite

class YataMicroLanePipelineSpec extends AnyFunSuite:
  test("registered YATA operations preserve original signed truncation and dependent issue spacing"):
    import java.nio.file.Files
    import scala.sys.process.*
    val rtl = YataMicrocodedSystemVerilog.emit(6,3,ProfileName.Baseline,"Core")
    val lane = rtl.substring(rtl.indexOf("module YataMicroLane("),rtl.indexOf("module YataModSwitch("))
    val directory = Files.createTempDirectory("ngen-yata-lane-pipeline")
    val tb = """module test;
      |reg clock=0;always #5 clock=~clock;
      |reg[3:0]kind;reg signed[53:0]a,b;reg signed[26:0]constant;reg[1:0]radix,number;
      |wire signed[53:0]pa,pb,ra,rb;
      |reg signed[53:0]qa[0:6],qb[0:6];integer cycle,j;
      |YataMicroLane reference(kind,a,b,constant,radix,number,ra,rb);
      |YataMicroLanePipeline dut(clock,kind,a,b,constant,radix,number,pa,pb);
      |initial begin
      | for(cycle=0;cycle<1200;cycle=cycle+1)begin
      |  @(negedge clock);kind=cycle%11;a={$random,$random};b={$random,$random};constant=$random;radix=cycle%4;number=(cycle/4)%4;#1;
      |  for(j=6;j>0;j=j-1)begin qa[j]=qa[j-1];qb[j]=qb[j-1];end qa[0]=ra;qb[0]=rb;
      |  @(posedge clock);#1;if(cycle>=6 && (pa!==qa[6] || pb!==qb[6]))$fatal(1,"cycle %d expected %h %h got %h %h",cycle,qa[6],qb[6],pa,pb);
      | end
      | $finish;
      |end
      |endmodule
      |""".stripMargin
    Files.writeString(directory.resolve("test.sv"),lane+YataMicroLanePipeline.definition+tb)
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)
    val old = YataMicrocodedSystemVerilog.scheduleLengths(6,3,ProfileName.Baseline)
    val pipelined = YataMicrocodedSystemVerilog.scheduleLengths(6,3,ProfileName.F300)
    assert(pipelined == (old._1*8,old._2*8))

  test("registered output conversion preserves rounded torus words, bubbles and reset"):
    import java.nio.file.Files
    import scala.sys.process.*
    val random=new scala.util.Random(0x544f5255)
    val boundary=Vector(0L,1L,-1L,40960000L,-40960000L,67108863L,-67108864L)
    var pending=Vector.fill[Option[BigInt]](3)(None)
    val checks=(0 until 300).map { cycle =>
      val value=if cycle<boundary.size then boundary(cycle) else random.nextInt(1<<27).toLong-(1L<<26)
      val positive=if value<0 then value+40960001L else value
      val expectedWord=((BigInt(positive)*BigInt("7036874245")+(BigInt(1)<<25))>>26)&((BigInt(1)<<32)-1)
      val reset=cycle==0 || cycle==78 || cycle==139
      val valid=cycle%7!=3
      val expected=if reset then
        pending=Vector.fill(3)(None);None
      else
        val queue=pending :+ (if valid then Some(expectedWord) else None)
        pending=queue.tail;queue.head
      val assertion=expected match
        case Some(word)=>s"if(!valid_out || torus!==32'h${word.toString(16)}) $$fatal(1,\"torus cycle $cycle\");"
        case None=>s"if(valid_out) $$fatal(1,\"unexpected torus valid cycle $cycle\");"
      val literal=if value<0 then s"-54'sd${-value}" else s"54'sd$value"
      s"@(negedge clock);reset=${if reset then 1 else 0};valid_in=${if valid then 1 else 0};value=$literal;@(posedge clock);#1;$assertion"
    }.mkString("\n")
    val directory=Files.createTempDirectory("ngen-yata-output-pipeline")
    val tb=s"module test;reg clock=0,reset=1,valid_in=0;reg signed[53:0]value=0;wire valid_out;wire[31:0]torus;always #5 clock=~clock;YataModSwitchPipeline dut(clock,reset,valid_in,value,valid_out,torus);initial begin $checks $$finish;end endmodule"
    Files.writeString(directory.resolve("test.sv"),YataMicroLanePipeline.outputConversion+tb)
    assert(Process(Seq("iverilog","-g2012","-s","test","-o","sim","test.sv"),directory.toFile).! == 0)
    assert(Process(Seq("vvp","sim"),directory.toFile).! == 0)
