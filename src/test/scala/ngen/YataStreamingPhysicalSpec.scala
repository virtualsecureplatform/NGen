package ngen

import ngen.backend.YataStreamingSystemVerilog
import org.scalatest.funsuite.AnyFunSuite

class YataStreamingPhysicalSpec extends AnyFunSuite:
  test("fixed-direction YATA keeps eight local phase copies and existing latency"):
    for inverse <- Seq(true,false) do
      val design=YataStreamingSystemVerilog.emit("PhysicalYata",inverse)
      assert(design.latency==(if inverse then 52 else 59))
      for group <- 0 until 8 do
        assert(design.source.contains(s"reg [2:0] input_cycle_$group;"))
        assert(design.source.contains(s"case(input_cycle_$group)"))
      assert(!design.source.contains("case(input_cycle)"))
  test("fixed-direction output conversion resets validity but not payload"):
    val source=YataStreamingSystemVerilog.emit("PhysicalYata",false).source
    assert(source.contains("if(reset) valid_pipe<=0; else valid_pipe<={valid_pipe[2:0],valid_in};"))
    for payload <- Seq("positive1","lo2","hi2","product3","rounded4") do
      assert(!source.contains(s"$payload<=0;"))
