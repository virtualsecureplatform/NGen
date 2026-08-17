package ngen

import ngen.algebra.Domains
import ngen.cli.{Cli, Command, Direction, GeneratorConfig}
import ngen.transform.ReferenceNtt
import ngen.backend.YataSystemVerilog
import ngen.backend.{DesignMetadata, GraphSystemVerilog, TransformDot}
import ngen.rtl.{Architecture, GenericNttGraph, PipelineProfile, Port, PortDirection, ReductionKind, StreamingContract, ValueFormat}

import java.nio.file.{Files, Path}

object Main:
  private def printPlan(config: GeneratorConfig): Unit =
    val direction = config.direction match
      case Direction.Forward => "forward NTT"
      case Direction.Inverse => "inverse NTT"
      case Direction.Both => "combined forward/inverse RAINTT"
    println(s"Transform: $direction")
    println(s"Domain: ${config.domain.name}")
    println(s"Size: ${config.domain.size} (2^${config.domain.logSize})")
    println(s"Modulus: ${config.domain.modulus.q}")
    println(s"Streaming width: ${config.streamingWidth} (2^${config.streamingLog})")
    println(s"Radix: ${config.radix} (2^${config.radixLog})")
    println(s"Shape: ${config.domain.shape}")

  private def check(config: GeneratorConfig): Unit =
    val input = Vector.tabulate(config.domain.size)(BigInt(_))
    val output = ReferenceNtt.inverse(config.domain, ReferenceNtt.forward(config.domain, input))
    require(output == input.map(config.domain.modulus.normalize), s"round trip failed for ${config.domain.name}")
    println("Mathematical NTT/INTT round-trip passed.")

  private def emit(config: GeneratorConfig): Boolean =
    if config.direction == Direction.Both then
      require(config.domain.name == "yata8", "raintt RTL emission currently supports -preset yata8")
      require(config.streamingLog == 3, "yata8 RAINTT requires -k 3")
      require(config.radixLog == 3, "yata8 RAINTT requires -r 3")
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val top = config.top.getOrElse("SmallYata8RainttP27Rtl")
      Files.writeString(output, YataSystemVerilog.emitRadix8(top))
      println(s"Written design in $output.")
      true
    else if config.domain.name == "custom" then
      require(config.streamingLog == config.domain.logSize, "custom-prime RTL in v0.1 requires -k equal to -n")
      require(config.radixLog == 1, "custom-prime RTL in v0.1 requires -r 1")
      val profile = PipelineProfile.named(config.profile)
      val inverse = config.direction == Direction.Inverse
      val graph = GenericNttGraph.build(config.domain, inverse, profile)
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val top = config.top.getOrElse("main")
      Files.writeString(output, GraphSystemVerilog.emit(graph, config.domain, top))
      val architecture = Architecture(
        s"custom-${if inverse then "intt" else "ntt"}",
        Vector(
          Port("clock", PortDirection.Input, ValueFormat.Valid),
          Port("reset", PortDirection.Input, ValueFormat.Valid),
          Port("next", PortDirection.Input, ValueFormat.Valid)
        ),
        Vector(graph),
        Vector.empty,
        Vector.empty,
        StreamingContract(config.domain.size, config.domain.size, 1, 1, graph.latency, 1),
        ReductionKind.Barrett,
        profile
      )
      val metadata = DesignMetadata(Cli.Version, config.domain, architecture, if inverse then "inverse" else "forward", 2, output.toString)
      val base = output.toString.stripSuffix(".sv").stripSuffix(".v")
      Files.writeString(Path.of(base + ".json"), metadata.toJson)
      if config.graph then Files.writeString(Path.of(base + ".graph.gv"), TransformDot.emit(config.domain, inverse))
      if config.rtlGraph then Files.writeString(Path.of(base + ".rtl.gv"), graph.toDot)
      println(s"Written design in $output.")
      println(s"Written metadata in ${base}.json.")
      true
    else false

  def main(args: Array[String]): Unit =
    try
      Cli.parse(args.toSeq) match
        case Command.Help => println(Cli.usage)
        case Command.Version => println(Cli.Version)
        case Command.Presets =>
          Domains.all.foreach(domain => println(s"${domain.name}\tq=${domain.modulus.q}\tN=${domain.size}\t${domain.description}"))
        case Command.Generate(config) =>
          printPlan(config)
          if config.check then check(config)
          if !emit(config) then println("RTL emission for this transform is not implemented yet.")
    catch
      case error: IllegalArgumentException =>
        Console.err.println(s"Error: ${error.getMessage}")
        Console.err.println(Cli.usage)
        sys.exit(2)
