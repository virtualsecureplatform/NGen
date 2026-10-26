package ngen

import ngen.cli.{Cli,Command}
import ngen.algebra.{Modulus,NttDomain,TransformShape}
import ngen.backend.OverlappedStageNttSystemVerilog
import ngen.rtl.ReductionKind
import ngen.transform.NttPlan
import org.scalatest.funsuite.AnyFunSuite

class OverlappedStageNttSpec extends AnyFunSuite:
  private val base=Seq("-n","4","-k","2","-q","97","-root","auto","-psi","auto",
    "-architecture","stage-parallel","-reduction","montgomery","-protocol","ready-valid")
  test("stage-parallel overlap and full-throughput alias select the same mode"):
    for args<-Seq(base++Seq("-frame-overlap","ntt"),base++Seq("-architecture","full-throughput","ntt")) do
      Cli.parse(args) match
        case Command.Generate(c)=>assert(c.overlappedStages)
        case _=>fail("expected generation")
  test("legacy stage-parallel and preset full-throughput remain distinct existing paths"):
    Cli.parse(base++Seq("-protocol","next","ntt")) match
      case Command.Generate(c)=>assert(!c.overlappedStages)
      case _=>fail("expected generation")
    Cli.parse(Seq("-preset","yata8","-architecture","full-throughput","-protocol","ready-valid","raintt")) match
      case Command.Generate(c)=>assert(!c.overlappedStages)
      case _=>fail("expected preset")
  test("overlap restrictions reject ignored or incompatible options"):
    for extra<-Seq(Seq("-pe","2"),Seq("-stage-groups","2"),Seq("-profile","f300"),
      Seq("-protocol","next"),Seq("-transpose","switch"),Seq("-runtime-control"),
      Seq("-output-register"),Seq("-architecture","streamed")) do
      assertThrows[IllegalArgumentException](Cli.parse(base++extra++Seq("-frame-overlap","ntt")))
  test("each stage owns its frame vector and validity instead of sharing stage-index state"):
    val domain=NttDomain("custom",8,Modulus(17),9,TransformShape.Cyclic)
    val rtl=OverlappedStageNttSystemVerilog.emit(NttPlan.radix2(domain,false),2,"Overlap",ReductionKind.Montgomery)
    assert(rtl.contains("stage_0[0:7]"))
    assert(rtl.contains("stage_2[0:7]"))
    assert(rtl.contains("stage_ready[2]"))
    assert(!rtl.contains("stage_index"))
