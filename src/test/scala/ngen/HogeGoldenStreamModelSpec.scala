package ngen

import ngen.arithmetic.HogeField
import ngen.transform.HogeTransform
import org.scalatest.funsuite.AnyFunSuite

class HogeGoldenStreamModelSpec extends AnyFunSuite:
  test("golden two-pass radix-32 streaming decomposition"):
    val input=Vector.tabulate(1024)(i=>BigInt(i));val inv=HogeField.inversePowerOfTwo(10)
    val root=HogeField.power(HogeField.Generator,1<<(32-10))
    val nttTable=Vector(inv)++Vector.tabulate(1023)(i=>HogeField.multiply(root.modPow(1023-i,HogeField.Modulus),inv))
    val twistRoot=HogeField.power(HogeField.Generator,1<<(32-10-1));val invTwist=twistRoot.modInverse(HogeField.Modulus)
    val nttTwist=Vector.tabulate(1024)(i=>invTwist.modPow(i,HogeField.Modulus))
    def transpose(v:Vector[BigInt])=Vector.tabulate(1024)(i=>v((i%32)*32+i/32))
    val first=input.grouped(32).toVector.flatMap(HogeTransform.forwardRadix32)
    val multiplied=first.zipWithIndex.map{case(v,i)=>val c=i/32;val l=i%32;HogeField.multiply(v,nttTable(HogeField.reverse(c,5)*l))}
    val between=transpose(multiplied)
    val second=between.grouped(32).toVector.flatMap(HogeTransform.forwardRadix32)
    // The golden stream presents cycle-major vectors, while the final twist is
    // indexed in the transposed (lane-major) tensor coordinate.
    val twisted=second.zipWithIndex.map{case(v,i)=>
      val cycle=i/32;val lane=i%32
      HogeField.multiply(v,nttTwist(lane*32+cycle))
    }
    val output=transpose(twisted)
    val expected=HogeTransform.forwardResidues(input,10,5)
    val mismatch=output.zip(expected).indexWhere((a,b)=>a!=b)
    assert(mismatch<0,if mismatch < 0 then "" else s"mismatch=$mismatch got=${output(mismatch)} expected=${expected(mismatch)}")

  test("golden inverse two-pass radix-32 streaming decomposition"):
    val input=Vector.tabulate(1024)(i=>BigInt(i))
    // The inverse wrapper receives each external cycle as one matrix column.
    val physical=Vector.tabulate(1024)(i=>input((i%32)*32+i/32))
    val tables=HogeField.tables(10)
    def former(values: Seq[BigInt]): Vector[BigInt] =
      def recurse(values: Vector[BigInt], depth: Int): Vector[BigInt] =
        if depth == 0 then values
        else
          val half=values.size/2
          val stage=Vector.tabulate(values.size){i=>
            if i < half then HogeField.add(values(i),values(i+half))
            else HogeField.shift(HogeField.subtract(values(i-half),values(i)),3*((i-half)<<(6-depth)))
          }
          recurse(stage.take(half),depth-1)++recurse(stage.drop(half),depth-1)
      val first=values.toArray
      for i <- 0 until 16 do
        val right=HogeField.shift(first(i+16),48)
        val left=first(i)
        first(i)=HogeField.shift(HogeField.add(left,right),3*i)
        first(i+16)=HogeField.shift(HogeField.subtract(left,right),9*i)
      recurse(first.take(16).toVector,4) ++ recurse(first.drop(16).toVector,4)
    val first=physical.grouped(32).toVector.flatMap(former)
    val multiplied=first.zipWithIndex.map{case(v,i)=>
      val cycle=i/32;val lane=i%32
      HogeField.multiply(v,HogeField.multiply(tables.inverse(HogeField.reverse(lane,5)*cycle),tables.inverseTwist(cycle)))
    }
    val between=Vector.tabulate(1024)(i=>multiplied((i%32)*32+i/32))
    val output=between.grouped(32).toVector.flatMap(HogeTransform.inverseRadix32)
    assert(output==HogeTransform.inverse(input,10,5))
