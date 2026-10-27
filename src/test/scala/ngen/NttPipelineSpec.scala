package ngen

import org.scalatest.funsuite.AnyFunSuite
import ngen.cli.{Cli,Command}
import ngen.rtl.NttPipeline

class NttPipelineSpec extends AnyFunSuite:
  val base=Seq("-n","4","-k","1","-r","1","-q","97","-root","auto","-psi","auto",
    "-architecture","streamed","-reduction","montgomery","-protocol","ready-valid")
  test("native interface owns all arithmetic partitions and input/output registration"):
    Cli.parse(base ++ Seq("-multiplier-pipeline-stages","3","-reduction-pipeline-stages","5","-output-register","-input-register","ntt")) match
      case Command.Generate(c) =>
        assert(c.pipeline == NttPipeline(Some(3),Some(5),true,true))
        assert(c.pipeline.butterflyLatency(7)==10)
      case _ => fail("expected generation")
  test("native bank arbitration remains a distinct NTT interface choice"):
    Cli.parse(base ++ Seq("-bank-write-arbitration","onehot","-input-register","-issue-register","ntt")) match
      case Command.Generate(c) =>
        assert(c.pipeline.bankWriteArbitration=="onehot")
        assert(c.pipeline.inputRegister)
        assert(c.pipeline.issueRegister)
      case _ => fail("expected generation")
  test("default width-aware arithmetic latency is preserved"):
    assert(NttPipeline().butterflyLatency(17)==7)
    assert(NttPipeline().butterflyLatency(64)==9)
    for m<-Seq(1,3); r<-Seq(3,4,5) do
      assert(NttPipeline(Some(m),Some(r)).butterflyLatency(32)==2+m+r)
  test("unsupported combinations fail instead of silently ignoring options"):
    for flag<-Seq(Seq("-multiplier-pipeline-stages","2"),Seq("-reduction-pipeline-stages","2"),
                  Seq("-architecture","stage-parallel","-multiplier-pipeline-stages","3"),
                  Seq("-r","2","-multiplier-pipeline-stages","3"),
                  Seq("-reduction","barrett","-output-register"),
                  Seq("-protocol","next","-output-register"),
                  Seq("-runtime-control","-output-register")) do
      assertThrows[IllegalArgumentException](Cli.parse(base ++ flag ++ Seq("ntt")))
