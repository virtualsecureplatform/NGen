package ngen.algebra

object Domains:
  val Yata512: NttDomain = NttDomain(
    name = "yata512",
    size = 512,
    modulus = Modulus(BigInt("40960001")),
    root = BigInt("2249398"),
    shape = TransformShape.Negacyclic,
    twist = Some(BigInt("37528371")),
    description = "YATA compressed RAINTT field, q = 5^4 * 2^16 + 1"
  )

  val Hoge1024: NttDomain = NttDomain(
    name = "hoge1024",
    size = 1024,
    modulus = Modulus(BigInt("18446744069414584321")),
    root = BigInt("11353340290879379826"),
    shape = TransformShape.Negacyclic,
    twist = Some(BigInt("455906449640507599")),
    description = "HOGE/Goldilocks field, q = 2^64 - 2^32 + 1"
  )

  val Kyber256: NttDomain = NttDomain(
    name = "kyber256",
    size = 256,
    modulus = Modulus(BigInt(3329)),
    root = BigInt(17),
    shape = TransformShape.IncompleteNegacyclic(baseCaseSize = 2),
    description = "CRYSTALS-Kyber field; seven-layer incomplete negacyclic NTT"
  )

  val all: Vector[NttDomain] = Vector(Yata512, Hoge1024, Kyber256)

  def named(name: String): Option[NttDomain] = all.find(_.name == name.toLowerCase)

  def validateAll(): Unit = all.foreach(_.validate())
