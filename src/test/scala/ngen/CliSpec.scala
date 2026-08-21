package ngen

import ngen.algebra.TransformShape
import ngen.cli.{Cli, Command, Direction}
import ngen.transform.GeneralNttAlgorithm
import ngen.rtl.{InterfaceKind, ProfileName, ReductionChoice, StreamProtocol, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class CliSpec extends AnyFunSuite:
  test("prime analysis and generation commands expose hardware-friendly fields"):
    assert(Cli.parse(Seq("-q","40960001","primeinfo"))==Command.PrimeInfo(ngen.algebra.Modulus(40960001)))
    assert(Cli.parse(Seq("-n","8","-data-width","32","primegen"))==Command.PrimeGenerate(8,32))
    Cli.parse(Seq("-n","1","-fermat-base","10","-fermat-index","1","ntt")) match
      case Command.Generate(config)=>assert(config.domain.modulus.q==101)
      case other=>fail(s"expected generalized Fermat generation, got $other")

  test("AXI4-Stream selects ready-valid while raw remains the default"):
    Cli.parse(Seq("-n","3","-k","1","-q","17","-root","9","-interface","axi4stream","ntt")) match
      case Command.Generate(config)=>
        assert(config.interfaceKind==InterfaceKind.Axi4Stream)
        assert(config.protocol==StreamProtocol.ReadyValid)
      case other=>fail(s"expected AXI generation, got $other")
    Cli.parse(Seq("-n","3","-q","17","-root","9","ntt")) match
      case Command.Generate(config)=>assert(!config.dspDecompose)
      case other=>fail(s"expected raw generation, got $other")
    assertThrows[IllegalArgumentException](Cli.parse(Seq("-n","3","-q","17","-root","9","-dsp-decompose","ntt")))
  test("SGen-style preset options precede the transform"):
    val command = Cli.parse(Seq("-preset", "yata512", "-k", "6", "-r", "3", "-check", "ntt"))
    val config = command match
      case Command.Generate(value) => value
      case other => fail(s"expected generation command, got $other")
    assert(config.domain.name == "yata512")
    assert(config.streamingWidth == 64)
    assert(config.radix == 8)
    assert(config.direction == Direction.Forward)
    assert(config.check)

  test("explicit custom field options create a cyclic domain"):
    val command = Cli.parse(Seq("-n", "3", "-k", "2", "-q", "0x11", "-root", "9", "intt"))
    val config = command match
      case Command.Generate(value) => value
      case other => fail(s"expected generation command, got $other")
    assert(config.domain.size == 8)
    assert(config.domain.modulus.q == 17)
    assert(config.domain.shape == TransformShape.Cyclic)
    assert(config.direction == Direction.Inverse)

  test("Kyber preset preserves incomplete-transform semantics"):
    val command = Cli.parse(Seq("-preset", "kyber256", "-k", "0", "-r", "1", "ntt"))
    val config = command match
      case Command.Generate(value) => value
      case other => fail(s"expected generation command, got $other")
    assert(config.domain.shape == TransformShape.IncompleteNegacyclic(2))

  test("a custom domain requires size, modulus, and root"):
    assertThrows[IllegalArgumentException](Cli.parse(Seq("-n", "3", "ntt")))

  test("RAINTT accepts SGen-style output and top options"):
    val command = Cli.parse(Seq("-preset", "yata8", "-k", "3", "-r", "3", "-o", "out.sv", "-top", "Candidate", "raintt"))
    val config = command match
      case Command.Generate(value) => value
      case other => fail(s"expected generation command, got $other")
    assert(config.direction == Direction.Both)
    assert(config.output.contains("out.sv"))
    assert(config.top.contains("Candidate"))

  test("pipeline profile and graph flags are parsed before the transform"):
    val command = Cli.parse(Seq("-n", "3", "-q", "17", "-root", "9", "-r", "1", "-profile", "f300", "-transpose", "switch", "-graph", "-rtlgraph", "ntt"))
    val config = command match
      case Command.Generate(value) => value
      case other => fail(s"expected generation command, got $other")
    assert(config.profile == ProfileName.F300)
    assert(config.transpose == TransposeKind.Switch)
    assert(config.graph && config.rtlGraph)

  test("preset backend selects the stage-parallel lowering"):
    val command = Cli.parse(Seq("-preset", "yata64", "-k", "3", "-r", "3", "-preset-backend", "stage-parallel", "raintt"))
    command match
      case Command.Generate(config) => assert(config.presetBackend == ngen.rtl.PresetBackend.StageParallel)
      case other => fail(s"expected generation command, got $other")

  test("generic architecture accepts stage-parallel lowering"):
    val command = Cli.parse(Seq("-n", "3", "-k", "2", "-q", "17", "-root", "9", "-architecture", "stage-parallel", "ntt"))
    command match
      case Command.Generate(config) => assert(config.architecture == ngen.rtl.ArchitectureKind.StageParallel)
      case other => fail(s"expected generation command, got $other")

  test("HOGE accepts the distributed transpose selector"):
    val command = Cli.parse(Seq("-preset", "hoge1024", "-k", "5", "-r", "5", "-transpose", "distributed", "ntt"))
    command match
      case Command.Generate(config) => assert(config.transpose == ngen.rtl.TransposeKind.Distributed)
      case other => fail(s"expected generation command, got $other")

  test("SGen-style full-throughput and compact architecture names parse"):
    assert(ngen.rtl.ArchitectureKind.parse("full-throughput") == ngen.rtl.ArchitectureKind.FullThroughput)
    assert(ngen.rtl.ArchitectureKind.parse("compact") == ngen.rtl.ArchitectureKind.Compact)
    assert(ngen.rtl.PresetBackend.parse("full-throughput") == ngen.rtl.PresetBackend.FullThroughput)
    assert(ngen.rtl.PresetBackend.parse("interleaved") == ngen.rtl.PresetBackend.Compact)

  test("switch transpose has an SGen-style terminal command"):
    val command = Cli.parse(Seq("-n", "3", "-data-width", "16", "-o", "transpose.sv", "switchtranspose"))
    assert(command == Command.SwitchTranspose(3, 3, 16, false, false, Some("transpose.sv"), None))
    assert(Cli.parse(Seq("-n","2","-k","3","switchtranspose")) == Command.SwitchTranspose(2,3,64,false,false,None,None))
    assert(Cli.parse(Seq("-n","2","-k","3","-fixed-rate","switchtranspose")) == Command.SwitchTranspose(2,3,64,true,false,None,None))
    assert(Cli.parse(Seq("-n","2","-k","3","-rate-preserving","switchtranspose")) == Command.SwitchTranspose(2,3,64,false,true,None,None))

  test("custom incomplete domains expose a configurable base case"):
    val command = Cli.parse(Seq("-n", "3", "-k", "1", "-r", "1", "-q", "17", "-root", "9", "-base-case", "2", "ntt"))
    command match
      case Command.Generate(config) => assert(config.domain.shape == TransformShape.IncompleteNegacyclic(2))
      case other => fail(s"expected generation command, got $other")

  test("custom domains can discover cyclic and negacyclic roots"):
    val cyclic = Cli.parse(Seq("-n", "3", "-q", "17", "-root", "auto", "ntt")) match
      case Command.Generate(config) => config.domain
      case other => fail(s"expected generation command, got $other")
    assert(cyclic.modulus.hasExactPowerOfTwoOrder(cyclic.root, 8))
    val negacyclic = Cli.parse(Seq("-n", "3", "-q", "97", "-root", "auto", "-psi", "auto", "ntt")) match
      case Command.Generate(config) => config.domain
      case other => fail(s"expected generation command, got $other")
    assert(negacyclic.shape == TransformShape.Negacyclic)
    assert(negacyclic.modulus.pow(negacyclic.twist.get, 8) == 96)

  test("custom streamed domains accept Shoup reduction"):
    val command = Cli.parse(Seq("-n", "3", "-k", "1", "-q", "17", "-root", "9", "-reduction", "shoup", "ntt"))
    command match
      case Command.Generate(config) => assert(config.reduction == ReductionChoice.Shoup)
      case other => fail(s"expected generation command, got $other")

  test("custom streamed domains expose PE count and fused radix"):
    val command = Cli.parse(Seq("-n", "4", "-k", "2", "-r", "2", "-pe", "1", "-q", "97", "-root", "8", "ntt"))
    command match
      case Command.Generate(config) =>
        assert(config.radix == 4)
        assert(config.peCount.contains(1))
      case other => fail(s"expected generation command, got $other")

  test("custom streamed domains accept ready-valid control"):
    val command = Cli.parse(Seq("-n", "3", "-k", "1", "-q", "17", "-root", "9", "-protocol", "ready-valid", "ntt"))
    command match
      case Command.Generate(config) => assert(config.protocol == StreamProtocol.ReadyValid)
      case other => fail(s"expected generation command, got $other")

  test("pipelined butterfly command requires an explicit reduction"):
    val command = Cli.parse(Seq("-q", "12289", "-reduction", "shoup", "-o", "pipe.sv", "butterflypipeline"))
    command match
      case Command.ButterflyPipeline(modulus, reduction, _, output, _) =>
        assert(modulus.q == 12289)
        assert(reduction == ReductionChoice.Shoup)
        assert(output.contains("pipe.sv"))
      case other => fail(s"expected butterfly pipeline command, got $other")

  test("classical and generalized Fermat domains derive their fields"):
    val classical = Cli.parse(Seq("-fermat", "4", "-n", "5", "-k", "3", "ntt")).asInstanceOf[Command.Generate].config
    assert(classical.domain.modulus.q == 65537)
    val generalized = Cli.parse(Seq("-fermat-base", "4", "-fermat-index", "1", "-n", "2", "-k", "1", "ntt")).asInstanceOf[Command.Generate].config
    assert(generalized.domain.modulus.q == 17)

  test("RNS polynomial multiplication accepts matched parameter vectors"):
    val command=Cli.parse(Seq("-n","3","-rns-q","17,97","-rns-root","9,64","-rns-psi","3,8","rnspolymul"))
    command match
      case Command.RnsPolynomial(basis,_,_,_) => assert(basis.combinedModulus==1649)
      case other => fail(s"expected RNS polynomial command, got $other")

  test("generalntt can request four-step decomposition"):
    val command = Cli.parse(Seq("-size", "8", "-q", "17", "-root", "9", "-four-step", "-four-step-factor", "2", "generalntt"))
    command match
      case Command.GeneralNtt(plan, _, _) =>
        assert(plan.algorithm == GeneralNttAlgorithm.FourStep)
        assert(plan.fourStepFactorsOrDefault == (2, 4))
      case other => fail(s"expected generalntt command, got $other")

  test("custom core accepts writable runtime control records"):
    val config=Cli.parse(Seq("-n","3","-k","1","-q","17","-root","9","-runtime-control","ntt")).asInstanceOf[Command.Generate].config
    assert(config.runtimeControl)
