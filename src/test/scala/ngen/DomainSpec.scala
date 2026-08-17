package ngen

import ngen.algebra.{Domains, TransformShape}
import org.scalatest.funsuite.AnyFunSuite
import ngen.backend.TransformDot

class DomainSpec extends AnyFunSuite:
  test("all built-in domains validate"):
    Domains.validateAll()

  test("Kyber is represented as an incomplete negacyclic transform"):
    assert(Domains.Kyber256.modulus.q == 3329)
    assert(Domains.Kyber256.normalizedRoot == 17)
    assert(Domains.Kyber256.shape == TransformShape.IncompleteNegacyclic(2))
    assert(Domains.Kyber256.modulus.pow(17, 256) == 1)
    assert(Domains.Kyber256.modulus.pow(17, 128) == 3328)

  test("Kyber has no primitive 512-th root in its base field"):
    assert((Domains.Kyber256.modulus.q - 1) % 512 != 0)

  test("Kyber transform graph contains exactly seven incomplete layers"):
    val dot = TransformDot.emit(Domains.Kyber256, inverse = false, radixLog = 1)
    assert(dot.contains("stage7"))
    assert(!dot.contains("stage8"))
