package ngen

import ngen.algebra.TransformShape
import ngen.cli.{Cli, Command, Direction}
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
