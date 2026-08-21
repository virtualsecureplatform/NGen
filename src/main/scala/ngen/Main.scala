package ngen

import ngen.algebra.Domains
import ngen.cli.{Cli, Command, Direction, GeneratorConfig}
import ngen.transform.ReferenceNtt
import ngen.backend.YataMicrocodedSystemVerilog
import ngen.backend.YataPipelinedSystemVerilog
import ngen.backend.YataFullThroughputSystemVerilog
import ngen.backend.{DesignMetadata, GraphSystemVerilog, TransformDot}
import ngen.backend.HogeSystemVerilog
import ngen.backend.HogePipelinedSystemVerilog
import ngen.backend.HogeFullThroughputSystemVerilog
import ngen.backend.KyberSystemVerilog
import ngen.backend.SwitchTransposeSystemVerilog
import ngen.backend.PeStreamingNttSystemVerilog
import ngen.backend.StageParallelNttSystemVerilog
import ngen.backend.GenericSwitchTransposeWrapper
import ngen.backend.PipelinedButterflySystemVerilog
import ngen.backend.RnsPolynomialMultiplierSystemVerilog
import ngen.backend.Axi4StreamWrapper
import ngen.backend.{FusedTwiddleButterflySystemVerilog,PrimeReductionSystemVerilog}
import ngen.rtl.GeneralNttGraph
import ngen.rtl.SwitchTransposeSpec
import ngen.rtl.{Architecture, ArchitectureKind, ArithmeticLoweringPlan, GenericNttGraph, InterfaceKind, PeNttSchedule, PipelineProfile, PresetBackend, ProfileName, Port, PortDirection, ReductionChoice, ReductionKind, StreamProtocol, StreamingContract, ValueFormat}
import ngen.transform.{DataOrder, IncompleteNttPlan, NttPlan, StreamingNttPlan, SwitchBoundaryPlan}
import ngen.transform.{NttFriendlyPrimeGenerator,PrimeGenerationRequest}
import ngen.arithmetic.PrimeAnalyzer

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
      initiationInterval: Int,
      architectureOverride: Option[String] = None
  ): Unit =
    val direction = config.direction.toString.toLowerCase
    val profile = config.profile.toString.toLowerCase
    val architecture = architectureOverride.getOrElse(config.domain.name match
      case name if name.startsWith("yata") => "yata-microcoded-radix8"
      case "hoge32" => "hoge-radix32"
      case "hoge1024" => "hoge-streamed-radix32"
      case "kyber256" => "kyber-pe1"
      case _ => config.architecture.toString.toLowerCase)
    val base = artifactBase(output)
    val fullThroughput = architecture.contains("full-throughput")
    val minimumGap = math.max(0, initiationInterval - inputCycles)
    val json = s"""{
      |  "schema": "ngen-design-v1",
      |  "generator_version": "${Cli.Version}",
      |  "domain": "${config.domain.name}",
      |  "modulus": "${config.domain.modulus.q}",
      |  "transform_size": ${config.domain.size},
      |  "direction": "$direction",
      |  "input_order": "${config.inputOrder.toString.toLowerCase}",
      |  "output_order": "${config.outputOrder.toString.toLowerCase}",
      |  "protocol": "${if config.protocol == StreamProtocol.NextPulse then "next" else "ready-valid"}",
      |  "streaming_width": ${config.streamingWidth},
      |  "radix": ${config.radix},
      |  "profile": "$profile",
      |  "architecture": "$architecture",
      |  "reduction_request": "${config.reduction.toString.toLowerCase}",
      |  "transpose": "${config.transpose.toString.toLowerCase}",
      |  "reduction": "$reduction",
      |  "latency": $latency,
      |  "initiation_interval": $initiationInterval,
      |  "minimum_gap": $minimumGap,
      |  "full_throughput": $fullThroughput,
      |  "pipeline_depth": $latency,
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
    val genericDomain = config.domain.name == "custom" || config.domain.name.startsWith("fermat") || config.domain.name.startsWith("generalized-fermat")
    if genericDomain then require(config.presetBackend == PresetBackend.Auto, "-preset-backend is only valid with a built-in preset")
    val architectureBackend = config.architecture match
      case ArchitectureKind.Auto => None
      case ArchitectureKind.FullThroughput => Some(PresetBackend.FullThroughput)
      case ArchitectureKind.Compact | ArchitectureKind.Streamed => Some(PresetBackend.Compact)
      case ArchitectureKind.StageParallel => Some(PresetBackend.StageParallel)
      case ArchitectureKind.FullyParallel => None
    val effectivePresetBackend =
      if genericDomain then PresetBackend.Auto
      else
        val explicit = if config.presetBackend == PresetBackend.Auto then None else Some(config.presetBackend)
        require(explicit.isEmpty || architectureBackend.isEmpty || explicit == architectureBackend,
          "-architecture and -preset-backend select conflicting preset implementations")
        explicit.orElse(architectureBackend).getOrElse(PresetBackend.Auto)
    if !genericDomain then
      require(config.interfaceKind==InterfaceKind.Raw,"AXI4-Stream is currently supported by custom streaming NTTs")
      require(config.architecture != ArchitectureKind.FullyParallel, "preset backends do not use the generic fully-parallel architecture")
      require(config.reduction == ReductionChoice.Auto, "preset backends select their field reduction automatically")
      require(config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural,
        "preset backends currently expose natural-order streams only")
    if config.direction == Direction.Both then
      if config.domain.name == "kyber256" then
        require(config.protocol == StreamProtocol.NextPulse, "Kyber preset currently uses the next-pulse protocol")
        require(!Set(PresetBackend.StageParallel, PresetBackend.FullThroughput)(effectivePresetBackend), "full-throughput/stage-parallel preset backend is not implemented for Kyber PE1")
        require(config.streamingLog == 0 && config.radixLog == 1, "kyberpe requires -k 0 -r 1")
        val output = Path.of(config.output.getOrElse("KyberHPM1PE.v"))
        Option(output.getParent).foreach(Files.createDirectories(_))
        Files.writeString(output, KyberSystemVerilog.emit(config.top.getOrElse("KyberHPM1PE")))
        writePresetArtifacts(config, output, "KyberMontgomery", 256, 256, KyberSystemVerilog.InverseCycles + 2, KyberSystemVerilog.InverseCycles)
        println(s"Written design in $output.")
        return true
      require(Set("yata8", "yata64", "yata512")(config.domain.name), "raintt requires a YATA preset")
      require(config.transpose != ngen.rtl.TransposeKind.Distributed, "distributed transpose is currently a HOGE forward architecture")
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
      val useFullThroughput = effectivePresetBackend == PresetBackend.FullThroughput
      require(config.protocol == StreamProtocol.NextPulse || useFullThroughput,
        "ready-valid YATA requires the full-throughput backend")
      require(!useFullThroughput || config.transpose == ngen.rtl.TransposeKind.Indexed, "YATA full-throughput mode currently uses indexed stream boundaries")
      val usePipelined = config.transpose == ngen.rtl.TransposeKind.Indexed && (effectivePresetBackend match
        case PresetBackend.StageParallel | PresetBackend.FullThroughput => true
        case PresetBackend.Microcoded | PresetBackend.Compact => false
        case PresetBackend.Auto => config.domain.name.startsWith("yata"))
      val rtl =
        if useFullThroughput then YataFullThroughputSystemVerilog.emit(config.domain.logSize,config.streamingLog,config.profile,top,config.protocol)
        else if usePipelined then YataPipelinedSystemVerilog.emit(config.domain.logSize, config.streamingLog, config.profile, top, config.transpose)
        else YataMicrocodedSystemVerilog.emit(config.domain.logSize, config.streamingLog, config.profile, top, config.transpose)
      Files.writeString(output, rtl)
      val cycles = config.domain.size / config.streamingWidth
      val schedule =
        if usePipelined then
          val (inverseStages, forwardStages) = YataPipelinedSystemVerilog.stageCounts(config.domain.logSize)
          val gap = if config.profile == ProfileName.F300 then 1 else 0
          (inverseStages + math.max(0, inverseStages - 1) * gap, forwardStages + math.max(0, forwardStages - 1) * gap)
        else YataMicrocodedSystemVerilog.scheduleLengths(config.domain.logSize, config.streamingLog, config.profile)
      val switchOverhead =
        if config.transpose != ngen.rtl.TransposeKind.Switch || cycles == 1 then 0
        else if config.streamingWidth == cycles then cycles - 1
        else 2 * cycles - 1
      val yataWait = if useFullThroughput then YataFullThroughputSystemVerilog.pipelineDepth(config.domain.logSize) else schedule._1.max(schedule._2) + 2 + switchOverhead
      writePresetArtifacts(config, output, "YataSredc", cycles, cycles, yataWait,
        if useFullThroughput then cycles else schedule._1.max(schedule._2),
        Some(if useFullThroughput then "yata-full-throughput-recursive-radix8" else if usePipelined then "yata-stage-parallel-radix8" else "yata-microcoded-radix8"))
      println(s"Written design in $output.")
      true
    else if config.domain.name == "hoge32" then
      require(config.protocol == StreamProtocol.NextPulse, "HOGE preset currently uses the next-pulse protocol")
      require(!Set(PresetBackend.StageParallel, PresetBackend.FullThroughput)(effectivePresetBackend), "stage-parallel/full-throughput preset backend requires hoge1024")
      require(config.transpose == ngen.rtl.TransposeKind.Indexed, "hoge32 has no streaming transpose boundary")
      require(config.streamingLog == 5 && config.radixLog == 5, "hoge32 requires -k 5 -r 5")
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      Files.writeString(output, HogeSystemVerilog.emitRadix32(config.top.getOrElse("SmallHoge32P64Rtl")))
      writePresetArtifacts(config, output, "Goldilocks", 1, 1, 1, 1)
      println(s"Written design in $output.")
      true
    else if config.domain.name == "hoge1024" then
      require(config.protocol == StreamProtocol.NextPulse, "HOGE preset currently uses the next-pulse protocol")
      require(config.streamingLog == 5 && config.radixLog == 5, "hoge1024 requires -k 5 -r 5")
      val output = Path.of(config.output.getOrElse("design.v"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val inverse = config.direction == Direction.Inverse
      val top = config.top.getOrElse(if inverse then "INTTWrap" else "NTTWrap")
      val useFullThroughput = effectivePresetBackend == PresetBackend.FullThroughput
      require(!useFullThroughput || config.transpose == ngen.rtl.TransposeKind.Switch,
        "HOGE full-throughput requires -transpose switch")
      val usePipelined = config.transpose == ngen.rtl.TransposeKind.Indexed && effectivePresetBackend == PresetBackend.StageParallel
      val rtl =
        if useFullThroughput then HogeFullThroughputSystemVerilog.emit(top, inverse, config.profile, config.transpose)
        else if usePipelined then HogePipelinedSystemVerilog.emit(top, inverse, config.profile, config.transpose)
        else if inverse then HogeSystemVerilog.emitStreamingIntt(top, config.profile, config.transpose)
        else HogeSystemVerilog.emitStreamingNtt(top, config.profile, config.transpose)
      Files.writeString(output, rtl)
      val bundles =
        if useFullThroughput then HogeFullThroughputSystemVerilog.RadixPipelineDepth * 2 + (if inverse then 31 else 62)
        else if usePipelined then
          val (inverseStages, forwardStages) = HogePipelinedSystemVerilog.stageCounts(10, 5)
          val stageCount = if inverse then inverseStages else forwardStages
          stageCount + math.max(0, stageCount - 1) * (if config.profile == ProfileName.F300 then 1 else 0)
        else HogeSystemVerilog.streamingBundles(inverse, config.profile)
      val switchOverhead = config.transpose match
        case ngen.rtl.TransposeKind.Indexed => 0
        case ngen.rtl.TransposeKind.Switch => 31
        case ngen.rtl.TransposeKind.Distributed => 47
      val maxWaitCycles = if useFullThroughput then (if inverse then 9 else 40) else bundles + 2 + switchOverhead
      writePresetArtifacts(config, output, "Goldilocks", 32, 32, maxWaitCycles,
        if useFullThroughput then 32 else bundles,
        Some(if useFullThroughput then "hoge-full-throughput-recursive-radix32"
        else if usePipelined then "hoge-stage-parallel-radix32"
        else if config.transpose == ngen.rtl.TransposeKind.Distributed then "hoge-distributed-transpose-radix32"
        else "hoge-streamed-radix32"))
      println(s"Written design in $output.")
      true
    else if genericDomain then
      require(config.transpose != ngen.rtl.TransposeKind.Distributed, "distributed transpose is currently a HOGE forward architecture")
      require(config.interfaceKind==InterfaceKind.Raw || config.transpose==ngen.rtl.TransposeKind.Indexed,
        "AXI4-Stream currently requires indexed stream boundaries")
      val profile = PipelineProfile.named(config.profile)
      val inverse = config.direction == Direction.Inverse
      val output = Path.of(config.output.getOrElse("design.sv"))
      Option(output.getParent).foreach(Files.createDirectories(_))
      val top = config.top.getOrElse("main")
      val analyzedPrimeForm=PrimeAnalyzer.analyze(config.domain.modulus).form
      val autoNeedsSpecialized=config.reduction==ReductionChoice.Auto&&(analyzedPrimeForm match
        case ngen.arithmetic.PrimeForm.Goldilocks|_:ngen.arithmetic.PrimeForm.Proth|_:ngen.arithmetic.PrimeForm.PseudoMersenne|_:ngen.arithmetic.PrimeForm.SparseSolinas=>true
        case _=>false)
      val fullyParallelCompatible = config.streamingLog == config.domain.logSize &&
        config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural &&
        !config.domain.shape.isInstanceOf[ngen.algebra.TransformShape.IncompleteNegacyclic] &&
        config.reduction != ReductionChoice.Montgomery && config.reduction != ReductionChoice.Shoup && config.reduction != ReductionChoice.FermatShift && !autoNeedsSpecialized && config.radixLog == 1 && config.peCount.isEmpty && config.protocol == StreamProtocol.NextPulse && config.transpose == ngen.rtl.TransposeKind.Indexed && !config.runtimeControl
      val reductionKind = config.reduction match
        case ReductionChoice.Auto if config.domain.name.startsWith("fermat") => ReductionKind.FermatShift
        case ReductionChoice.Auto if config.domain.name.startsWith("generalized-fermat") =>
          PrimeAnalyzer.analyze(config.domain.modulus).form match
            case ngen.arithmetic.PrimeForm.GeneralizedFermat(base,_) if base.isValidInt && Integer.bitCount(base.toInt)==1 => ReductionKind.FermatShift
            case _ => ReductionKind.Shoup
        case ReductionChoice.Auto =>
          PrimeAnalyzer.analyze(config.domain.modulus).form match
            case ngen.arithmetic.PrimeForm.Goldilocks|_:ngen.arithmetic.PrimeForm.PseudoMersenne|_:ngen.arithmetic.PrimeForm.SparseSolinas=>ReductionKind.SparseFold
            case _:ngen.arithmetic.PrimeForm.Proth=>ReductionKind.Montgomery
            case _=>ReductionKind.Barrett
        case ReductionChoice.Barrett => ReductionKind.Barrett
        case ReductionChoice.Montgomery => ReductionKind.Montgomery
        case ReductionChoice.Shoup => ReductionKind.Shoup
        case ReductionChoice.FermatShift =>
          require(config.domain.name.startsWith("fermat") || config.domain.name.startsWith("generalized-fermat"), "fermat-shift reduction requires a Fermat field")
          ReductionKind.FermatShift
      val useFullyParallel = config.architecture match
        case ArchitectureKind.Auto => fullyParallelCompatible
        case ArchitectureKind.FullyParallel =>
          require(fullyParallelCompatible, "fully-parallel custom RTL requires K=N, radix 2, natural stream order, a complete transform, and no -pe override")
          true
        case ArchitectureKind.FullThroughput =>
          require(fullyParallelCompatible,
            "generic full-throughput currently requires K=N, radix 2, natural stream order, a complete transform, and no PE override")
          true
        case ArchitectureKind.Streamed => false
        case ArchitectureKind.StageParallel => false
        case ArchitectureKind.Compact => false
      val useStageParallel = config.architecture == ArchitectureKind.StageParallel
      val primeAnalysis=PrimeAnalyzer.analyze(config.domain.modulus)
      val arithmeticPlan=ArithmeticLoweringPlan.build(config.domain.modulus,config.domain.logSize,config.domain.modulus.bitWidth+2)
      val primeFormCode=primeAnalysis.form match
        case ngen.arithmetic.PrimeForm.Goldilocks=>1
        case _:ngen.arithmetic.PrimeForm.Proth=>2
        case _:ngen.arithmetic.PrimeForm.PseudoMersenne=>3
        case _:ngen.arithmetic.PrimeForm.SparseSolinas=>4
        case _:ngen.arithmetic.PrimeForm.GeneralizedFermat=>5
        case ngen.arithmetic.PrimeForm.Generic=>0
      var architectureParameters = Map(
        "axi4stream"->(if config.interfaceKind==InterfaceKind.Axi4Stream then 1 else 0),
        "dsp_decompose"->(if config.dspDecompose then 1 else 0),
        "prime_form"->primeFormCode,
        "two_adicity"->primeAnalysis.twoAdicity,
        "lazy_butterfly_levels"->primeAnalysis.lazyButterflyLevels,
        "montgomery_signed_digits"->primeAnalysis.montgomery.map(_.signedDigits).min
        ,"dsp_multiplier_tiles"->(if config.dspDecompose then arithmeticPlan.multiplier.dspCount else 0)
        ,"dsp_partial_adder_levels"->(if config.dspDecompose then arithmeticPlan.multiplier.adderLevels else 0)
        ,"lazy_correction_points"->arithmeticPlan.lazySchedule.correctionAfter.size
      )
      val architecture =
        if useStageParallel then
          require(config.domain.shape == ngen.algebra.TransformShape.Cyclic || config.domain.shape == ngen.algebra.TransformShape.Negacyclic,
            "stage-parallel architecture requires a complete cyclic or negacyclic transform")
          require(config.protocol == StreamProtocol.NextPulse, "stage-parallel architecture currently requires the next-pulse protocol")
          require(config.radixLog == 1 && config.peCount.isEmpty && !config.runtimeControl,
            "stage-parallel architecture currently uses the complete radix-2 plan without PE overrides")
          require(config.reduction != ReductionChoice.FermatShift && reductionKind != ReductionKind.FermatShift,
            "stage-parallel architecture currently supports Barrett, Montgomery, Shoup, or sparse-fold reduction")
          val basePlan = NttPlan.radix2(config.domain, inverse, config.inputOrder, config.outputOrder)
          val useSwitchTranspose = config.transpose == ngen.rtl.TransposeKind.Switch
          if useSwitchTranspose then
            require(config.streamingWidth * config.streamingWidth == config.domain.size,
              "switch transpose requires streaming width equal to stream cycle count")
          val coreTop = if useSwitchTranspose then s"${top}Core" else top
          val coreRtl = StageParallelNttSystemVerilog.emit(basePlan, config.streamingWidth, coreTop, config.profile, reductionKind)
          Files.writeString(output,
            if useSwitchTranspose then GenericSwitchTransposeWrapper.emit(coreRtl, top, coreTop, config.streamingWidth, config.domain.modulus.bitWidth)
            else coreRtl)
          val stageCount = StageParallelNttSystemVerilog.stageCount(basePlan)
          val streamCycles = config.domain.size / config.streamingWidth
          val gap = if config.profile == ProfileName.F300 then 1 else 0
          val executionCycles = stageCount + math.max(0, stageCount - 1) * gap
          architectureParameters ++= Map(
            "stage_count" -> stageCount,
            "stage_gap" -> gap,
            "switch_transpose" -> (if useSwitchTranspose then 1 else 0),
            "coefficient_buffers" -> 0
          )
          Architecture(
            s"custom-${if inverse then "intt" else "ntt"}-stage-parallel",
            Vector(Port("clock", PortDirection.Input, ValueFormat.Valid), Port("reset", PortDirection.Input, ValueFormat.Valid),
              Port("next", PortDirection.Input, ValueFormat.Valid), Port("ready", PortDirection.Output, ValueFormat.Valid), Port("next_out", PortDirection.Output, ValueFormat.Valid)),
            Vector.empty, Vector.empty,
            Vector(ngen.rtl.CounterSpec("capture", streamCycles), ngen.rtl.CounterSpec("stage", math.max(1, stageCount)), ngen.rtl.CounterSpec("output", streamCycles)),
            StreamingContract(config.domain.size, config.streamingWidth, streamCycles, streamCycles,
              streamCycles + executionCycles + streamCycles - 1 + (if useSwitchTranspose then 2 * (config.streamingWidth - 1) else 0),
              streamCycles + executionCycles + streamCycles - 1),
            reductionKind, profile
          )
        else if useFullyParallel then
          val graph = GenericNttGraph.build(config.domain, inverse, profile)
          Files.writeString(output, GraphSystemVerilog.emit(graph, config.domain, top))
          Architecture(
            s"custom-${if inverse then "intt" else "ntt"}-${if config.architecture == ArchitectureKind.FullThroughput then "full-throughput" else "fully-parallel"}",
            Vector(Port("clock", PortDirection.Input, ValueFormat.Valid), Port("reset", PortDirection.Input, ValueFormat.Valid), Port("next", PortDirection.Input, ValueFormat.Valid)),
            Vector(graph), Vector.empty, Vector.empty,
            StreamingContract(config.domain.size, config.domain.size, 1, 1, graph.latency, 1),
            ReductionKind.Barrett, profile
          )
        else
          val basePlan: StreamingNttPlan = config.domain.shape match
            case ngen.algebra.TransformShape.IncompleteNegacyclic(_) =>
              require(config.inputOrder == DataOrder.Natural && config.outputOrder == DataOrder.Natural,
                "incomplete transforms currently expose natural-order streams only")
              IncompleteNttPlan(config.domain, inverse)
            case _ => NttPlan.radix2(config.domain, inverse, config.inputOrder, config.outputOrder)
          val useSwitchTranspose = config.transpose == ngen.rtl.TransposeKind.Switch
          if useSwitchTranspose then
            require(config.protocol == StreamProtocol.NextPulse, "switch transpose currently requires the uninterrupted next-pulse protocol")
            require(config.streamingWidth * config.streamingWidth == config.domain.size,
              "switch transpose requires streaming width equal to stream cycle count")
          val plan = if useSwitchTranspose then SwitchBoundaryPlan(basePlan, config.streamingWidth) else basePlan
          val requestedPeCount = config.peCount.getOrElse(math.max(1, config.streamingWidth / 2))
          val schedule = PeNttSchedule.build(plan, config.radixLog, requestedPeCount, config.streamingWidth)
          val metrics = PeStreamingNttSystemVerilog.metrics(schedule, config.streamingWidth, config.profile)
          architectureParameters ++= Map(
            "pe_count" -> metrics.peCount,
            "radix" -> metrics.radix,
            "bank_count_per_buffer" -> metrics.bankCount,
            "bank_depth" -> metrics.bankDepth,
            "coefficient_buffers" -> 2,
            "operation_bundles" -> metrics.bundleCount,
            "switch_transpose" -> (if useSwitchTranspose then 1 else 0),
            "runtime_control" -> (if config.runtimeControl then 1 else 0),
            "control_record_width" -> PeStreamingNttSystemVerilog.packedControlWidth(schedule)
          )
          val useAxi=config.interfaceKind==InterfaceKind.Axi4Stream
          val coreTop = if useSwitchTranspose || useAxi then s"${top}Core" else top
          val coreRtl = PeStreamingNttSystemVerilog.emit(schedule, config.streamingWidth, coreTop, config.profile, reductionKind, config.protocol, config.runtimeControl)
          Files.writeString(output,
            if useSwitchTranspose then GenericSwitchTransposeWrapper.emit(coreRtl, top, coreTop, config.streamingWidth, config.domain.modulus.bitWidth)
            else if useAxi then Axi4StreamWrapper.emit(coreRtl,top,coreTop,config.streamingWidth,config.domain.modulus.bitWidth,metrics.inputCycles)
            else coreRtl)
          val controlPorts = config.protocol match
            case StreamProtocol.NextPulse => Vector(Port("next", PortDirection.Input, ValueFormat.Valid), Port("ready", PortDirection.Output, ValueFormat.Valid), Port("next_out", PortDirection.Output, ValueFormat.Valid))
            case StreamProtocol.ReadyValid => Vector(Port("in_valid", PortDirection.Input, ValueFormat.Valid), Port("in_ready", PortDirection.Output, ValueFormat.Valid), Port("out_valid", PortDirection.Output, ValueFormat.Valid), Port("out_ready", PortDirection.Input, ValueFormat.Valid))
          Architecture(
            s"custom-${if inverse then "intt" else "ntt"}-banked-pe-radix${config.radix}",
            Vector(Port("clock", PortDirection.Input, ValueFormat.Valid), Port("reset", PortDirection.Input, ValueFormat.Valid)) ++ controlPorts,
            Vector.empty,
            Vector(ngen.rtl.MemorySpec("coefficient_buffers", metrics.bankDepth, ValueFormat.unsigned(config.domain.modulus.bitWidth), banks = 2 * metrics.bankCount, readLatency = 1)),
            Vector(ngen.rtl.CounterSpec("capture", metrics.inputCycles), ngen.rtl.CounterSpec("bundle", math.max(1, metrics.bundleCount)), ngen.rtl.CounterSpec("output", metrics.outputCycles)),
            StreamingContract(config.domain.size, config.streamingWidth, metrics.inputCycles, metrics.outputCycles,
              metrics.latency + (if useSwitchTranspose then 2 * (config.streamingWidth - 1) else 0), metrics.initiationInterval),
            reductionKind, profile
          )
      val metadata = DesignMetadata(Cli.Version, config.domain, architecture, if inverse then "inverse" else "forward", config.radix, output.toString,
        config.inputOrder.toString.toLowerCase, config.outputOrder.toString.toLowerCase,
        if config.protocol == StreamProtocol.NextPulse then "next" else "ready-valid", architectureParameters)
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
        case Command.PrimeInfo(modulus) =>
          val analysis=PrimeAnalyzer.analyze(modulus)
          println(s"modulus=${modulus.q}")
          println(s"form=${analysis.form}")
          println(s"two_adicity=${analysis.twoAdicity}")
          println(s"maximum_cyclic_log_size=${analysis.maximumCyclicLogSize}")
          println(s"maximum_negacyclic_log_size=${analysis.maximumNegacyclicLogSize}")
          println(s"recommended_multiplier=${analysis.recommendedMultiplier}")
          println(s"lazy_butterfly_levels=${analysis.lazyButterflyLevels}")
          analysis.montgomery.foreach(cost=>println(s"montgomery_${cost.wordBits}=qinv:${cost.qInverse},bits:${cost.nonzeroBits},signed_digits:${cost.signedDigits}"))
        case Command.PrimeGenerate(transformLog,bitWidth) =>
          val domain=NttFriendlyPrimeGenerator.generate(PrimeGenerationRequest(transformLog,bitWidth)).head
          println(s"q=${domain.modulus.q}")
          println(s"root=${domain.root}")
          println(s"psi=${domain.twist.get}")
          println(s"reduction=${PrimeAnalyzer.analyze(domain.modulus).recommendedMultiplier}")
        case Command.PrimeReducer(modulus,fused,dspDecompose,outputName,topName) =>
          val output=Path.of(outputName.getOrElse(if fused then "fused-butterfly.sv" else "prime-reducer.sv"))
          Option(output.getParent).foreach(Files.createDirectories(_))
          val top=topName.getOrElse(if fused then "NGenFusedTwiddleButterfly" else "NGenPrimeReducer")
          Files.writeString(output,if fused then FusedTwiddleButterflySystemVerilog.emit(modulus,top,dspDecompose) else PrimeReductionSystemVerilog.emit(modulus,top))
          println(s"Written specialized arithmetic in $output.")
        case Command.SwitchTranspose(inputCycleLog, inputLaneLog, dataWidth, fixedRate, outputName, topName) =>
          val output = Path.of(outputName.getOrElse("switch-transpose.sv"))
          Option(output.getParent).foreach(Files.createDirectories(_))
          Files.writeString(output, SwitchTransposeSystemVerilog.emit(SwitchTransposeSpec(inputCycleLog,inputLaneLog,dataWidth), topName.getOrElse("SwitchTransposeTop"),fixedRate))
          println(s"Written switch transpose in $output.")
        case Command.ButterflyPipeline(modulus, reduction, runtimeField, outputName, topName) =>
          val output = Path.of(outputName.getOrElse("butterfly-pipeline.sv"))
          Option(output.getParent).foreach(Files.createDirectories(_))
          val kind = reduction match
            case ReductionChoice.Barrett => ReductionKind.Barrett
            case ReductionChoice.Montgomery => ReductionKind.Montgomery
            case ReductionChoice.Shoup => ReductionKind.Shoup
            case ReductionChoice.FermatShift => ReductionKind.FermatShift
            case _ => throw new IllegalArgumentException("butterfly pipeline reduction must be explicit")
          Files.writeString(output, PipelinedButterflySystemVerilog.emit(modulus, kind, topName.getOrElse("NGenPipelinedButterfly"),runtimeField))
          println(s"Written pipelined butterfly in $output.")
        case Command.RnsPolynomial(basis, emitCrt, outputName, topName) =>
          val output = Path.of(outputName.getOrElse("rns-polymul.sv"))
          Option(output.getParent).foreach(Files.createDirectories(_))
          Files.writeString(output,RnsPolynomialMultiplierSystemVerilog.emit(basis,topName.getOrElse("RnsPolynomialMultiplier"),emitCrt))
          println(s"Written RNS polynomial multiplier in $output.")
        case Command.GeneralNtt(plan, outputName, topName) =>
          val output=Path.of(outputName.getOrElse("general-ntt.sv"));Option(output.getParent).foreach(Files.createDirectories(_))
          val graph=GeneralNttGraph.build(plan,PipelineProfile.Baseline)
          Files.writeString(output,GraphSystemVerilog.emit(graph,plan.domain.modulus,plan.domain.size,topName.getOrElse("GeneralNtt")))
          println(s"Written ${plan.algorithm.toString.toLowerCase} NTT in $output.")
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
