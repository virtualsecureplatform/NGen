package ngen

import ngen.algebra.Domains
import ngen.cli.{Cli, Command, Direction, GeneratorConfig}
import ngen.transform.ReferenceNtt
import ngen.backend.YataMicrocodedSystemVerilog
import ngen.backend.{DesignMetadata, GraphSystemVerilog, TransformDot}
import ngen.backend.HogeSystemVerilog
import ngen.backend.KyberSystemVerilog
import ngen.backend.SwitchTransposeSystemVerilog
import ngen.backend.GenericStreamingNttSystemVerilog
import ngen.rtl.SwitchTransposeSpec
import ngen.rtl.{Architecture, ArchitectureKind, GenericNttGraph, PipelineProfile, Port, PortDirection, ReductionChoice, ReductionKind, StreamingContract, ValueFormat}
import ngen.transform.{DataOrder, IncompleteNttPlan, NttPlan, StreamingNttPlan}

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
    val architecture = config.domain.name match
      case name if name.startsWith("yata") => "yata-microcoded-radix8"
      case "hoge32" => "hoge-radix32"
      case "hoge1024" => "hoge-streamed-radix32"
      case "kyber256" => "kyber-pe1"
      case _ => config.architecture.toString.toLowerCase
    val base = artifactBase(output)
    val json = s"""{
      |  "schema": "ngen-design-v1",
      |  "generator_version": "${Cli.Version}",
      |  "domain": "${config.domain.name}",
      |  "modulus": "${config.domain.modulus.q}",
      |  "transform_size": ${config.domain.size},
      |  "direction": "$direction",
      |  "input_order": "${config.inputOrder.toString.toLowerCase}",
      |  "output_order": "${config.outputOrder.toString.toLowerCase}",
      |  "streaming_width": ${config.streamingWidth},
      |  "radix": ${config.radix},
      |  "profile": "$profile",
      |  "architecture": "$architecture",
      |  "reduction_request": "${config.reduction.toString.toLowerCase}",
      |  "transpose": "${config.transpose.toString.toLowerCase}",
      |  "reduction": "$reduction",
      |  "latency": $latency,
      |  "initiation_interval": $initiationInterval,
      |  "input_cycles": $inputCycles,
      |  "output_cycles": $outputCycles,
      |  "dependencies": [],
      |  "output_file": "${output.toString.replace("\\", "\\\\").replace("\"", "\\\"")}"
      |}
      |""".stripMargin
    Files.writeString(Path.of(base + ".json"), json)
    if config.graph then Files.writeString(Path.of(base + ".graph.gv"), TransformDot.emit(config.domain, config.direction == Direction.Inverse, config.radixLog))
    if config.rtlGraph then
      val transposeNode = if config.transpose == ngen.rtl.TransposeKind.Switch then " -> switch_transpose" else ""
      Files.writeString(Path.of(base + ".rtl.gv"), s"digraph rtl { input$transposeNode -> buffer -> ${reduction.toLowerCase} -> output; }\n")
  private def printPlan(config: GeneratorConfig): Unit =
    val direction = config.direction match
      case Direction.Forward => "forward NTT"
      case Direction.Inverse => "inverse NTT"
      case Direction.Both => if config.domain.name == "kyber256" then "combined Kyber forward/inverse PE" else "combined forward/inverse RAINTT"
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
    if config.domain.name != "custom" then
      require(config.architecture == ArchitectureKind.Auto, "preset backends select their architecture automatically")
      require(config.reduction == ReductionChoice.Auto, "preset backends select their field reduction automatically")
      require(config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural,
        "preset backends currently expose natural-order streams only")
    if config.direction == Direction.Both then
      if config.domain.name == "kyber256" then
        require(config.streamingLog == 0 && config.radixLog == 1, "kyberpe requires -k 0 -r 1")
        val output = Path.of(config.output.getOrElse("KyberHPM1PE.v"))
        Option(output.getParent).foreach(Files.createDirectories(_))
        Files.writeString(output, KyberSystemVerilog.emit(config.top.getOrElse("KyberHPM1PE")))
        writePresetArtifacts(config, output, "KyberMontgomery", 256, 256, KyberSystemVerilog.InverseCycles + 2, KyberSystemVerilog.InverseCycles)
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
      Files.writeString(output, YataMicrocodedSystemVerilog.emit(config.domain.logSize, config.streamingLog, config.profile, top, config.transpose))
      val cycles = config.domain.size / config.streamingWidth
      val schedule = YataMicrocodedSystemVerilog.scheduleLengths(config.domain.logSize, config.streamingLog, config.profile)
      val switchOverhead =
        if config.transpose != ngen.rtl.TransposeKind.Switch || cycles == 1 then 0
        else if config.streamingWidth == cycles then cycles - 1
        else 2 * cycles - 1
      writePresetArtifacts(config, output, "YataSredc", cycles, cycles, schedule._1.max(schedule._2) + 2 + switchOverhead, schedule._1.max(schedule._2))
      println(s"Written design in $output.")
      true
    else if config.domain.name == "hoge32" then
      require(config.transpose == ngen.rtl.TransposeKind.Indexed, "hoge32 has no streaming transpose boundary")
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
      val rtl = if inverse then HogeSystemVerilog.emitStreamingIntt(top, config.profile, config.transpose) else HogeSystemVerilog.emitStreamingNtt(top, config.profile, config.transpose)
      Files.writeString(output, rtl)
      val bundles = HogeSystemVerilog.streamingBundles(inverse, config.profile)
      val switchOverhead = if inverse && config.transpose == ngen.rtl.TransposeKind.Switch then 31 else 0
      writePresetArtifacts(config, output, "Goldilocks", 32, 32, bundles + 2 + switchOverhead, bundles)
      println(s"Written design in $output.")
      true
    else if config.domain.name == "custom" then
      require(config.transpose == ngen.rtl.TransposeKind.Indexed, "custom-prime v0.1 RTL supports indexed transpose only")
      require(config.radixLog == 1, "custom-prime RTL in v0.1 requires -r 1")
      val profile = PipelineProfile.named(config.profile)
      val inverse = config.direction == Direction.Inverse
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val top = config.top.getOrElse("main")
      val fullyParallelCompatible = config.streamingLog == config.domain.logSize &&
        config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural &&
        !config.domain.shape.isInstanceOf[ngen.algebra.TransformShape.IncompleteNegacyclic] &&
        config.reduction != ReductionChoice.Montgomery && config.reduction != ReductionChoice.Shoup
      val reductionKind = config.reduction match
        case ReductionChoice.Auto | ReductionChoice.Barrett => ReductionKind.Barrett
        case ReductionChoice.Montgomery => ReductionKind.Montgomery
        case ReductionChoice.Shoup => ReductionKind.Shoup
      val useFullyParallel = config.architecture match
        case ArchitectureKind.Auto => fullyParallelCompatible
        case ArchitectureKind.FullyParallel =>
          require(fullyParallelCompatible, "fully-parallel custom RTL requires K=N, natural stream order, and a complete transform")
          true
        case ArchitectureKind.Streamed => false
      val architecture =
        if useFullyParallel then
          val graph = GenericNttGraph.build(config.domain, inverse, profile)
          Files.writeString(output, GraphSystemVerilog.emit(graph, config.domain, top))
          Architecture(
            s"custom-${if inverse then "intt" else "ntt"}-fully-parallel",
            Vector(Port("clock", PortDirection.Input, ValueFormat.Valid), Port("reset", PortDirection.Input, ValueFormat.Valid), Port("next", PortDirection.Input, ValueFormat.Valid)),
            Vector(graph), Vector.empty, Vector.empty,
            StreamingContract(config.domain.size, config.domain.size, 1, 1, graph.latency, 1),
            ReductionKind.Barrett, profile
          )
        else
          val plan: StreamingNttPlan = config.domain.shape match
            case ngen.algebra.TransformShape.IncompleteNegacyclic(_) =>
              require(config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural,
                "incomplete transforms currently expose natural-order streams only")
              IncompleteNttPlan(config.domain, inverse)
            case _ => NttPlan.radix2(config.domain, inverse, config.inputOrder, config.outputOrder)
          val schedule = GenericStreamingNttSystemVerilog.schedule(plan, config.streamingWidth, config.profile)
          Files.writeString(output, GenericStreamingNttSystemVerilog.emit(plan, config.streamingWidth, top, config.profile, reductionKind))
          Architecture(
            s"custom-${if inverse then "intt" else "ntt"}-streamed-radix2",
            Vector(Port("clock", PortDirection.Input, ValueFormat.Valid), Port("reset", PortDirection.Input, ValueFormat.Valid), Port("next", PortDirection.Input, ValueFormat.Valid), Port("ready", PortDirection.Output, ValueFormat.Valid)),
            Vector.empty,
            Vector(ngen.rtl.MemorySpec("work", config.domain.size, ValueFormat.unsigned(config.domain.modulus.bitWidth), readLatency = 0)),
            Vector(ngen.rtl.CounterSpec("capture", schedule.inputCycles), ngen.rtl.CounterSpec("bundle", math.max(1, schedule.bundles.size)), ngen.rtl.CounterSpec("output", schedule.outputCycles)),
            StreamingContract(config.domain.size, config.streamingWidth, schedule.inputCycles, schedule.outputCycles, schedule.latency, schedule.initiationInterval),
            reductionKind, profile
          )
      val metadata = DesignMetadata(Cli.Version, config.domain, architecture, if inverse then "inverse" else "forward", 2, output.toString,
        config.inputOrder.toString.toLowerCase, config.outputOrder.toString.toLowerCase)
      val base = output.toString.stripSuffix(".sv").stripSuffix(".v")
      Files.writeString(Path.of(base + ".json"), metadata.toJson)
      if config.graph then Files.writeString(Path.of(base + ".graph.gv"), TransformDot.emit(config.domain, inverse, config.radixLog))
      if config.rtlGraph then
        val graphText = if architecture.datapaths.nonEmpty then architecture.datapaths.head.toDot
        else s"digraph rtl { input -> capture -> radix2_bundles -> output; }\n"
        Files.writeString(Path.of(base + ".rtl.gv"), graphText)
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
        case Command.SwitchTranspose(logSize, dataWidth, outputName, topName) =>
          val output = Path.of(outputName.getOrElse("switch-transpose.sv"))
          Option(output.getParent).foreach(Files.createDirectories(_))
          Files.writeString(output, SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(logSize, dataWidth), topName.getOrElse("SwitchTransposeTop")))
          println(s"Written switch transpose in $output.")
        case Command.Generate(config) =>
          printPlan(config)
          if config.check then check(config)
          if !emit(config) then
            throw new IllegalArgumentException(s"${config.domain.name} does not support ${config.direction.toString.toLowerCase} RTL emission")
    catch
      case error: IllegalArgumentException =>
        Console.err.println(s"Error: ${error.getMessage}")
        Console.err.println(Cli.usage)
        sys.exit(2)
