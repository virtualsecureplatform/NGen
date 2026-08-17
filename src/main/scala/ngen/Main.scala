package ngen

import ngen.algebra.Domains
import ngen.cli.{Cli, Command, Direction, GeneratorConfig}
import ngen.transform.ReferenceNtt
import ngen.backend.YataMicrocodedSystemVerilog
import ngen.backend.{DesignMetadata, GraphSystemVerilog, TransformDot}
import ngen.backend.HogeSystemVerilog
import ngen.backend.KyberSystemVerilog
import ngen.rtl.{Architecture, GenericNttGraph, PipelineProfile, Port, PortDirection, ReductionKind, StreamingContract, ValueFormat}

import java.nio.file.{Files, Path}

object Main:
  private def artifactBase(output: Path): String = output.toString.stripSuffix(".sv").stripSuffix(".v")

  private def writePresetArtifacts(
      config: GeneratorConfig,
      output: Path,
      reduction: String,
      inputCycles: Int,
      outputCycles: Int,
      latency: Int,
      initiationInterval: Int
  ): Unit =
    val direction = config.direction.toString.toLowerCase
    val profile = config.profile.toString.toLowerCase
    val base = artifactBase(output)
    val json = s"""{
      |  "schema": "ngen-design-v1",
      |  "generator_version": "${Cli.Version}",
      |  "domain": "${config.domain.name}",
      |  "modulus": "${config.domain.modulus.q}",
      |  "transform_size": ${config.domain.size},
      |  "direction": "$direction",
      |  "streaming_width": ${config.streamingWidth},
      |  "radix": ${config.radix},
      |  "profile": "$profile",
      |  "reduction": "$reduction",
      |  "latency": $latency,
      |  "initiation_interval": $initiationInterval,
      |  "input_cycles": $inputCycles,
      |  "output_cycles": $outputCycles,
      |  "output_file": "${output.toString.replace("\\", "\\\\").replace("\"", "\\\"")}"
      |}
      |""".stripMargin
    Files.writeString(Path.of(base + ".json"), json)
    if config.graph then Files.writeString(Path.of(base + ".graph.gv"), TransformDot.emit(config.domain, config.direction == Direction.Inverse))
    if config.rtlGraph then
      Files.writeString(Path.of(base + ".rtl.gv"), s"digraph rtl { input -> buffer -> ${reduction.toLowerCase} -> output; }\n")
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
      if config.domain.name == "kyber256" then
        require(config.streamingLog == 0 && config.radixLog == 1, "kyberpe requires -k 0 -r 1")
        val output = Path.of(config.output.getOrElse("KyberHPM1PE.v"))
        Option(output.getParent).foreach(Files.createDirectories(_))
        Files.writeString(output, KyberSystemVerilog.emit(config.top.getOrElse("KyberHPM1PE")))
        writePresetArtifacts(config, output, "KyberBarrett", 256, 256, KyberSystemVerilog.InverseCycles + 2, KyberSystemVerilog.InverseCycles)
        println(s"Written design in $output.")
        return true
      require(Set("yata8", "yata64", "yata512")(config.domain.name), "raintt requires a YATA preset")
      require(config.radixLog == 3, "yata8 RAINTT requires -r 3")
      val expectedStreamingLog = if config.domain.name == "yata512" then 6 else 3
      require(config.streamingLog == expectedStreamingLog, s"${config.domain.name} requires -k $expectedStreamingLog")
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val defaultTop = config.domain.name match
        case "yata8" => "SmallYata8RainttP27Rtl"
        case "yata64" => "SmallYata8x8RainttP27Rtl"
        case _ => "YataRainttTop"
      val top = config.top.getOrElse(defaultTop)
      Files.writeString(output, YataMicrocodedSystemVerilog.emit(config.domain.logSize, config.streamingLog, config.profile, top))
      val cycles = config.domain.size / config.streamingWidth
      val schedule = YataMicrocodedSystemVerilog.scheduleLengths(config.domain.logSize, config.streamingLog, config.profile)
      writePresetArtifacts(config, output, "YataSredc", cycles, cycles, schedule._1.max(schedule._2) + 2, schedule._1.max(schedule._2))
      println(s"Written design in $output.")
      true
    else if config.domain.name == "hoge32" then
      require(config.streamingLog == 5 && config.radixLog == 5, "hoge32 requires -k 5 -r 5")
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      Files.writeString(output, HogeSystemVerilog.emitRadix32(config.top.getOrElse("SmallHoge32P64Rtl")))
      writePresetArtifacts(config, output, "Goldilocks", 1, 1, 1, 1)
      println(s"Written design in $output.")
      true
    else if config.domain.name == "hoge1024" then
      require(config.streamingLog == 5 && config.radixLog == 5, "hoge1024 requires -k 5 -r 5")
      val output = Path.of(config.output.getOrElse("design.v"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val inverse = config.direction == Direction.Inverse
      val top = config.top.getOrElse(if inverse then "INTTWrap" else "NTTWrap")
      val rtl = if inverse then HogeSystemVerilog.emitStreamingIntt(top) else HogeSystemVerilog.emitStreamingNtt(top)
      Files.writeString(output, rtl)
      val bundles = HogeSystemVerilog.streamingBundles(inverse)
      writePresetArtifacts(config, output, "Goldilocks", 32, 32, bundles + 2, bundles)
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
