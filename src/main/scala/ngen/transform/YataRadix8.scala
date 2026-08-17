package ngen.transform

/** Executable specification of YATA's compressed radix-8 RAINTT block. */
object YataRadix8:
  val Size = 8

  def inverse(input: Seq[Long]): Vector[Long] =
    YataTransform.inverse(input, 3)

  def forwardResidues(input: Seq[Long]): Vector[Long] =
    YataTransform.forwardResidues(input, 3)

  def forwardTorus(input: Seq[Long]): Vector[Long] =
    YataTransform.forwardTorus(input, 3)
