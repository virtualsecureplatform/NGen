package ngen

import ngen.algebra.{Modulus, NttDomain, TransformShape}
import ngen.transform.{RnsBasis, RnsPolynomialMultiplier}
import org.scalatest.funsuite.AnyFunSuite

class RnsPolynomialMultiplierSpec extends AnyFunSuite:
  private val basis = RnsBasis(Vector(
    NttDomain("q17-negacyclic", 8, Modulus(17), 9, TransformShape.Negacyclic, Some(3)),
    NttDomain("q97-negacyclic", 8, Modulus(97), 64, TransformShape.Negacyclic, Some(8))
  ))

  test("multi-prime NTT multiplication agrees with direct negacyclic convolution"):
    val lhs = Vector.tabulate(8)(i => BigInt(i * 3 - 4))
    val rhs = Vector.tabulate(8)(i => BigInt(i * i - 5))
    val expected = Vector.tabulate(8) { output =>
      (for left <- lhs.indices; right <- rhs.indices if (left + right) % 8 == output yield
        val sign = if left + right >= 8 then -1 else 1
        BigInt(sign) * lhs(left) * rhs(right)
      ).sum.mod(basis.combinedModulus)
    }
    assert(RnsPolynomialMultiplier.multiply(basis, lhs, rhs) == expected)

  test("CRT reconstruction returns the unique combined residue"):
    for value <- Vector(BigInt(0), BigInt(1), BigInt(1234), basis.combinedModulus - 1) do
      assert(basis.reconstruct(basis.domains.map(domain => value.mod(domain.modulus.q))) == value)
