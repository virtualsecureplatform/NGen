package ngen

import ngen.algebra.TransformShape
import ngen.cli.{Cli, Command, Direction}
import ngen.rtl.{ProfileName, ReductionChoice, StreamProtocol, TransposeKind}
import org.scalatest.funsuite.AnyFunSuite

class CliSpec extends AnyFunSuite:
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

  test("switch transpose has an SGen-style terminal command"):
    val command = Cli.parse(Seq("-n", "3", "-data-width", "16", "-o", "transpose.sv", "switchtranspose"))
    assert(command == Command.SwitchTranspose(3, 16, Some("transpose.sv"), None))

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
