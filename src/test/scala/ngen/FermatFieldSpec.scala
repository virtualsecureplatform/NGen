package ngen

import ngen.arithmetic.FermatField
import ngen.transform.ReferenceNtt
import org.scalatest.funsuite.AnyFunSuite

class FermatFieldSpec extends AnyFunSuite:
  test("F0 through F4 derive exact power-of-two roots"):
    for index <- 0 to 4 do
      val field = FermatField(index)
      field.domain(Integer.numberOfTrailingZeros(field.rootOrder)).validate()

  test("shift multiplication agrees with exact modular powers of two"):
    val field = FermatField(4)
    for value <- Vector(BigInt(0),BigInt(1),BigInt(12345),field.modulus.q-1); shift <- 0 until 40 do
      assert(field.shiftMultiply(value,shift)==field.modulus.multiply(value,field.modulus.pow(2,shift)))

  test("a streamed-size Fermat domain round trips"):
    val domain = FermatField(4).domain(5)
    val input = Vector.tabulate(domain.size)(i => BigInt(i*i-7))
    assert(ReferenceNtt.inverse(domain,ReferenceNtt.forward(domain,input))==input.map(domain.modulus.normalize))
