package ngen.rtl

trait IndexedOperation:
  def indices: Set[Int]

final case class MicroProgram[+T <: IndexedOperation](width: Int, bundles: Vector[Vector[T]]):
  require(width > 0)
  require(bundles.forall(bundle => bundle.nonEmpty && bundle.size <= width))
  require(bundles.forall(bundle => bundle.flatMap(_.indices).distinct.size == bundle.flatMap(_.indices).size),
    "operations in one bundle must not access the same state element")
  val length: Int = bundles.size

object MicroProgram:
  /** Preserve program order while packing adjacent independent operations. */
  def schedule[T <: IndexedOperation](operations: Vector[T], width: Int): MicroProgram[T] =
    require(operations.nonEmpty)
    val result = scala.collection.mutable.ArrayBuffer.empty[Vector[T]]
    var current = Vector.empty[T]
    var used = Set.empty[Int]
    operations.foreach { operation =>
      if current.size == width || operation.indices.exists(used) then
        result += current
        current = Vector.empty
        used = Set.empty
      current :+= operation
      used ++= operation.indices
    }
    if current.nonEmpty then result += current
    MicroProgram(width, result.toVector)
