package ngen

import ngen.arithmetic.YataField
import ngen.transform.YataRadix8
import org.scalatest.funsuite.AnyFunSuite

class YataFieldSpec extends AnyFunSuite:
  test("YATA constants match the 27-bit field representation"):
    assert(YataField.R == 11337725)
    assert(YataField.R2 == 15277344)

  test("signed reduction represents multiplication by R inverse"):
    val field = BigInt(YataField.Modulus)
    val inverseR = BigInt(YataField.R).modInverse(field)
    val values = Vector(-40000000L, -17L, 0L, 1L, 31L, 1234567L, 40000000L)
    for lhs <- values; rhs <- values do
      val expected = (BigInt(lhs) * rhs * inverseR).mod(field)
      assert(BigInt(YataField.multiplySigned(lhs, rhs)).mod(field) == expected)

  test("radix-8 twist tables match the established YATA convention"):
    val tables = YataField.tables(3)
    assert(tables.inttTwist == Vector(11337725L, 36471443L, 5897978L, 9555369L, 21278229L, 27149502L, 22898279L, 1080107L))
    assert(
      tables.nttTwist == Vector(
        16777216L,
        7520300L,
        6167871L,
        10334004L,
        22940222L,
        39041176L,
        11066564L,
        12776015L
      ).map(YataField.signedWord)
    )

  test("radix-8 executable plan matches established vectors"):
    val inverse = YataRadix8.inverse(Vector.tabulate(8)(_.toLong))
    assert(inverse == Vector(5425149, 770371, 7591597, 8347876, -16028223, 14138555, 12705675, -10784757))
    assert(
      YataRadix8.forwardTorus(inverse) == Vector(
        2974891933L,
        3396857465L,
        3678680817L,
        4065610805L,
        1229563543L,
        4031974689L,
        1801199139L,
        1197829964L
      )
    )
