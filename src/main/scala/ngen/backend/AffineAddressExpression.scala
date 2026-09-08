package ngen.backend

/** Recognize a complete binary-indexed table as an affine map over GF(2).
  * Every table entry is checked before replacing a case table with XOR logic.
  */
object AffineAddressExpression:
  def emit(values: Vector[Int], index: String, width: Int): Option[String] =
    require(values.nonEmpty && Integer.bitCount(values.size) == 1)
    require(width > 0 && width <= 31)
    require(values.forall(value => value >= 0 && BigInt(value) < (BigInt(1) << width)))
    val bits = Integer.numberOfTrailingZeros(values.size)
    val offset = values.head
    val columns = Vector.tabulate(bits)(bit => values(1 << bit) ^ offset)
    val equivalent = values.indices.forall { address =>
      val computed = columns.indices.foldLeft(offset) { (value, bit) =>
        if (address & (1 << bit)) != 0 then value ^ columns(bit) else value
      }
      computed == values(address)
    }
    if !equivalent then None
    else
      val expressions = (0 until width).reverse.map { bit =>
        val mask = columns.indices.filter(column => (columns(column) & (1 << bit)) != 0).foldLeft(0)((mask, column) => mask | (1 << column))
        val constant = (offset >> bit) & 1
        if mask == 0 then s"1'b$constant"
        else s"((^(32'($index) & 32'h${mask.toHexString})) ^ 1'b$constant)"
      }
      Some(expressions.mkString("{", ",", "}"))
