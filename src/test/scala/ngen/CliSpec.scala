package ngen

import ngen.algebra.TransformShape
import ngen.cli.{Cli, Command, Direction}
import ngen.rtl.{ProfileName, TransposeKind}
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
