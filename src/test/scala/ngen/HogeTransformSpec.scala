package ngen

import ngen.arithmetic.HogeField
import ngen.transform.HogeTransform
import org.scalatest.funsuite.AnyFunSuite

class HogeTransformSpec extends AnyFunSuite:
  test("radix-32 forward and inverse butterflies compose to scaling by 32"):
    val input = Vector.tabulate(32)(i => BigInt(i * i + 1))
    val transformed = HogeTransform.forwardRadix32(HogeTransform.inverseRadix32(input))
    assert(transformed == input.map(value => HogeField.multiply(value, 32)))

  test("1024-point HOGE transform returns the original 32-bit torus words"):
    val input = Vector.tabulate(1024)(i => BigInt((i * 2654435761L) & 0xffffffffL))
    val inverse = HogeTransform.inverse(input, 10, 5)
    val output = HogeTransform.forwardResidues(inverse, 10, 5)
    assert(output.map(_ & 0xffffffffL) == input)
