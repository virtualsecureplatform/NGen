package ngen

import ngen.rtl.{IndexedOperation, MicroProgram}
import org.scalatest.funsuite.AnyFunSuite

class MicroProgramSpec extends AnyFunSuite:
  private final case class Op(indices: Set[Int]) extends IndexedOperation

  test("scheduler packs adjacent independent operations without reordering"):
    val operations = Vector(Op(Set(0, 1)), Op(Set(2)), Op(Set(1, 3)), Op(Set(4)))
    val program = MicroProgram.schedule(operations, width = 2)
    assert(program.bundles == Vector(operations.take(2), operations.slice(2, 4)))

  test("scheduler splits conflicting state accesses"):
    val operations = Vector(Op(Set(0)), Op(Set(0)), Op(Set(1)))
    val program = MicroProgram.schedule(operations, width = 4)
    assert(program.bundles == Vector(Vector(operations(0)), operations.drop(1)))
