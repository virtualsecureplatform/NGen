package ngen

import ngen.algebra.{Modulus,NttDomain,TransformShape}
import ngen.backend.PartitionedNttSystemVerilog
import ngen.cli.{Cli,Command}
import ngen.rtl.PeNttSchedule
import ngen.transform.NttPlan
import org.scalatest.funsuite.AnyFunSuite

class PartitionedNttSpec extends AnyFunSuite:
  val domain=NttDomain("test",16,Modulus(97),8,TransformShape.Negacyclic,Some(28))
  test("partitioned stages preserve forward and inverse arithmetic including twists"):
    for inverse <- Seq(false,true); groups <- 1 to 4; pe <- Seq(1,2,4) do
      val plan=NttPlan.radix2(domain,inverse)
      val input=Vector.tabulate(16)(i=>BigInt((i*13+7)%97))
      val parts=PartitionedNttSystemVerilog.partitions(plan,groups)
      val actual=parts.foldLeft(input)((values,part)=>PeNttSchedule.build(part,1,pe,4).evaluate(values))
      assert(actual==plan.evaluate(input),s"inverse=$inverse groups=$groups pe=$pe")
  test("CLI parses stage groups and concrete plan commands"):
    val args=Seq("-n","4","-q","97","-root","8","-k","2","-stage-groups","3","-architecture","streamed","-protocol","ready-valid","ntt")
    val Command.Generate(config)=Cli.parse(args): @unchecked
    assert(config.stageGroups==3)
    assert(Cli.parse("plan"+:args)==Command.Plan(config))
    assert(Cli.parse(Seq("capabilities"))==Command.Capabilities)
