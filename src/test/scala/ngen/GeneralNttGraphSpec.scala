package ngen

import ngen.algebra.Modulus
import ngen.rtl.{GeneralNttGraph,PipelineProfile}
import ngen.transform.{GeneralNttAlgorithm,GeneralNttDomain,GeneralNttPlan}
import org.scalatest.funsuite.AnyFunSuite

class GeneralNttGraphSpec extends AnyFunSuite:
  test("six-point mixed-radix graph matches the executable plan"):
    val domain=GeneralNttDomain("six",6,Modulus(13),4)
    val plan=GeneralNttPlan(domain,inverse=false)
    val graph=GeneralNttGraph.build(plan,PipelineProfile.Baseline)
    val input=Vector.tabulate(6)(i=>BigInt(i*i-2))
    assert(graph.evaluate((0 until 6).map(i=>s"i$i"->input(i)).toMap)==plan.evaluate(input))

  test("five-point Bluestein graph matches the executable plan"):
    val domain=GeneralNttDomain("five",5,Modulus(241),87,Some(44))
    val plan=GeneralNttPlan(domain,inverse=false)
    val graph=GeneralNttGraph.build(plan,PipelineProfile.Baseline)
    val input=Vector.tabulate(5)(i=>BigInt(i*7-3))
    assert(graph.evaluate((0 until 5).map(i=>s"i$i"->input(i)).toMap)==plan.evaluate(input))

  test("eight-point four-step graph matches the executable plan"):
    val domain=GeneralNttDomain("eight",8,Modulus(17),9)
    val plan=GeneralNttPlan(domain,inverse=false,algorithm=GeneralNttAlgorithm.FourStep,fourStepFactors=Some((2,4)))
    val graph=GeneralNttGraph.build(plan,PipelineProfile.Baseline)
    val input=Vector.tabulate(8)(i=>BigInt(i))
    assert(graph.evaluate((0 until 8).map(i=>s"i$i"->input(i)).toMap)==plan.evaluate(input))
