package ngen.rtl

import ngen.transform.GeneralNttPlan

object GeneralNttGraph:
  def build(plan: GeneralNttPlan, profile: PipelineProfile): TimedGraph =
    val domain=plan.domain;val field=domain.modulus;val builder=TimedGraphBuilder()
    val multiplyLatency=profile.multiplierLatency+profile.reductionLatency
    val inputs=Vector.tabulate(domain.size)(i=>builder.input(s"i$i",ValueFormat.unsigned(field.bitWidth)))
    def multiply(signal: Signal,constant: BigInt): Signal = if field.normalize(constant)==1 then signal else builder(BarrettMultiplyConstant(field,constant,multiplyLatency),signal)
    def add(values: Vector[Signal]): Signal = values.reduce((left,right)=>builder(ModularAdd(field,profile.addLatency),left,right))
    def direct(values: Vector[Signal],root: BigInt): Vector[Signal] = Vector.tabulate(values.size)(output=>add(values.zipWithIndex.map((signal,index)=>multiply(signal,field.pow(root,index*output)))))
    def radix2(values: Vector[Signal],root: BigInt): Vector[Signal] =
      var current=Vector.tabulate(values.size)(i=>values(ngen.transform.ReferenceNtt.bitReverse(i,Integer.numberOfTrailingZeros(values.size))))
      var span=2
      while span<=values.size do
        val half=span/2;val next=current.toArray
        for block<-0 until values.size by span; index<-0 until half do
          val even=current(block+index);val odd=multiply(current(block+index+half),field.pow(root,index*values.size/span))
          next(block+index)=builder(ModularAdd(field,profile.addLatency),even,odd);next(block+index+half)=builder(ModularSubtract(field,profile.addLatency),even,odd)
        current=next.toVector;span*=2
      current
    def recurse(values: Vector[Signal],root: BigInt,factors: Vector[Int]): Vector[Signal] =
      if values.size==1 then values
      else
        val radix=factors.head;val remaining=values.size/radix
        val first=Vector.tabulate(remaining)(offset=>direct(Vector.tabulate(radix)(lane=>values(offset+remaining*lane)),field.pow(root,remaining)))
        val second=Vector.tabulate(radix) { k1 =>
          val sequence=Vector.tabulate(remaining)(offset=>multiply(first(offset)(k1),field.pow(root,offset*k1)))
          recurse(sequence,field.pow(root,radix),factors.tail)
        }
        Vector.tabulate(values.size)(index=>second(index%radix)(index/radix))
    val root=if plan.inverse then field.inverse(domain.normalizedRoot) else domain.normalizedRoot
    var outputs = if plan.algorithm == ngen.transform.GeneralNttAlgorithm.MixedRadix then recurse(inputs,root,domain.factors) else
      val convolutionRoot=domain.convolutionRoot.getOrElse(throw new IllegalArgumentException("Bluestein RTL requires a convolution root"))
      val m=domain.convolutionSize;val inverseTwo=BigInt(2).modInverse(domain.size)
      val zero=multiply(inputs.head,0)
      val a=Vector.tabulate(m)(index=>if index<domain.size then multiply(inputs(index),field.pow(root,(BigInt(index)*index*inverseTwo).mod(domain.size))) else zero)
      val kernel=Vector.tabulate(m) { index =>
        val distance=if index<domain.size then index else if index>m-domain.size then m-index else 0
        if index<domain.size || index>m-domain.size then field.pow(root,(-BigInt(distance)*distance*inverseTwo).mod(domain.size)) else BigInt(0)
      }
      val kernelSpectrum=Vector.tabulate(m)(k=>kernel.indices.foldLeft(BigInt(0))((sum,n)=>field.add(sum,field.multiply(kernel(n),field.pow(convolutionRoot,n*k)))))
      val spectrum=radix2(a,convolutionRoot).zip(kernelSpectrum).map(multiply)
      val convolution=radix2(spectrum,field.inverse(convolutionRoot)).map(multiply(_,field.inverse(m)))
      Vector.tabulate(domain.size)(index=>multiply(convolution(index),field.pow(root,(BigInt(index)*index*inverseTwo).mod(domain.size))))
    if plan.inverse then outputs=outputs.map(multiply(_,field.inverse(domain.size)))
    builder.result(outputs*)
