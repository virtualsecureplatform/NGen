package ngen

import ngen.algebra.{Domains, Modulus, NttDomain, TransformShape}
import ngen.transform.{KyberNtt, ReferenceNtt}
import org.scalatest.funsuite.AnyFunSuite

class ReferenceNttSpec extends AnyFunSuite:
  private val cyclic8 = NttDomain(
    name = "test8",
    size = 8,
    modulus = Modulus(17),
    root = 9,
    shape = TransformShape.Cyclic
  )

  private val negacyclic8 = NttDomain(
    name = "negacyclic8",
    size = 8,
    modulus = Modulus(97),
    root = 64,
    shape = TransformShape.Negacyclic,
    twist = Some(8)
  )

  private def directCyclic(domain: NttDomain, input: Vector[BigInt]): Vector[BigInt] =
    Vector.tabulate(domain.size) { k =>
      input.indices.foldLeft(BigInt(0)) { (sum, j) =>
        domain.modulus.add(
          sum,
          domain.modulus.multiply(input(j), domain.modulus.pow(domain.root, j * k))
        )
      }
    }

  test("radix-2 plan agrees with the direct cyclic definition"):
    val input = Vector.tabulate(8)(i => BigInt(i + 1))
    assert(ReferenceNtt.forward(cyclic8, input) == directCyclic(cyclic8, input))

  test("cyclic transform round trips"):
    val input = Vector(-1, 0, 1, 2, 16, 17, 18, 35).map(BigInt(_))
    assert(ReferenceNtt.inverse(cyclic8, ReferenceNtt.forward(cyclic8, input)) == input.map(cyclic8.modulus.normalize))

  test("negacyclic twist transform round trips"):
    negacyclic8.validate()
    val input = Vector.tabulate(8)(i => BigInt(i * i - 3))
    assert(ReferenceNtt.inverse(negacyclic8, ReferenceNtt.forward(negacyclic8, input)) == input.map(negacyclic8.modulus.normalize))

  test("Kyber zetas use seven-bit bit-reversed powers of root 17"):
    assert(KyberNtt.zetas(Domains.Kyber256).take(8) == Vector(1, 1729, 2580, 3289, 2642, 630, 1897, 848))

  test("Kyber incomplete NTT round trips"):
    val field = Domains.Kyber256.modulus
    val inputs = Vector(
      Vector.tabulate(256)(BigInt(_)),
      Vector.tabulate(256)(i => BigInt(i * i - 400)),
      Vector.tabulate(256)(i => if i == 0 then BigInt(1) else BigInt(0))
    )
    inputs.foreach { input =>
      val transformed = ReferenceNtt.forward(Domains.Kyber256, input)
      assert(ReferenceNtt.inverse(Domains.Kyber256, transformed) == input.map(field.normalize))
    }
