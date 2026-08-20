package ngen.backend

import ngen.arithmetic.{BarrettField,PrimeAnalyzer,PrimeForm}
import ngen.rtl.{GenericNttGraph, PipelineProfile,ReductionKind}
import ngen.transform.RnsBasis

object RnsPolynomialMultiplierSystemVerilog:
  def emit(basis: RnsBasis, top: String = "RnsPolynomialMultiplier", emitCrt: Boolean = false): String =
    val size = basis.size
    val profile = PipelineProfile.Baseline
    val cores = basis.domains.zipWithIndex.flatMap { case (domain,prime) =>
      val reduction=PrimeAnalyzer.analyze(domain.modulus).form match
        case PrimeForm.Goldilocks|_:PrimeForm.PseudoMersenne|_:PrimeForm.SparseSolinas=>ReductionKind.SparseFold
        case _=>ReductionKind.Barrett
      Vector(
        GraphSystemVerilog.emit(GenericNttGraph.build(domain,inverse=false,profile),domain,s"${top}ForwardA$prime",reduction),
        GraphSystemVerilog.emit(GenericNttGraph.build(domain,inverse=false,profile),domain,s"${top}ForwardB$prime",reduction),
        GraphSystemVerilog.emit(GenericNttGraph.build(domain,inverse=true,profile),domain,s"${top}Inverse$prime",reduction)
      )
    }
    val residuePorts = basis.domains.zipWithIndex.flatMap { case (domain,prime) =>
      val width=domain.modulus.bitWidth
      Vector.tabulate(size)(i=>s"input [${width-1}:0] a_${prime}_$i") ++ Vector.tabulate(size)(i=>s"input [${width-1}:0] b_${prime}_$i") ++ Vector.tabulate(size)(i=>s"output [${width-1}:0] o_${prime}_$i")
    }
    val combinedWidth=basis.combinedModulus.bitLength
    val crtPorts=if emitCrt then Vector.tabulate(size)(i=>s"output [${combinedWidth-1}:0] crt_$i") else Vector.empty
    val ports = Vector("input clock","input reset","input next","output next_out") ++ residuePorts ++ crtPorts
    val bodies = basis.domains.zipWithIndex.map { case (domain,prime) =>
      val field=domain.modulus;val width=field.bitWidth;val barrett=BarrettField(field)
      val sparse=PrimeAnalyzer.analyze(field).form match
        case PrimeForm.Goldilocks|_:PrimeForm.PseudoMersenne|_:PrimeForm.SparseSolinas=>true
        case _=>false
      val wires=(0 until size).map(i=>s"wire [${width-1}:0] fa_${prime}_$i,fb_${prime}_$i,p_${prime}_$i;").mkString
      val forwardA=(Vector(".clock(clock)",".reset(reset)",".next(next)")++Vector.tabulate(size)(i=>s".i$i(a_${prime}_$i)")++Vector(".next_out(fa_valid_"+prime+")")++Vector.tabulate(size)(i=>s".o$i(fa_${prime}_$i)")).mkString(",")
      val forwardB=(Vector(".clock(clock)",".reset(reset)",".next(next)")++Vector.tabulate(size)(i=>s".i$i(b_${prime}_$i)")++Vector(".next_out(fb_valid_"+prime+")")++Vector.tabulate(size)(i=>s".o$i(fb_${prime}_$i)")).mkString(",")
      val inverse=(Vector(".clock(clock)",".reset(reset)",s".next(fa_valid_$prime&&fb_valid_$prime)")++Vector.tabulate(size)(i=>s".i$i(p_${prime}_$i)")++Vector(s".next_out(inv_valid_$prime)")++Vector.tabulate(size)(i=>s".o$i(o_${prime}_$i)")).mkString(",")
      val products=Vector.tabulate(size)(i=>s"assign p_${prime}_$i=rns_mul_$prime(fa_${prime}_$i,fb_${prime}_$i);").mkString
      val pointMultiply=if sparse then SparseFoldFunction.emit(field,s"rns_mul_$prime") else s"""function automatic [${width-1}:0] rns_mul_$prime(input [${width-1}:0] a,input [${width-1}:0] b);
         |reg [${2*width-1}:0] product;reg [${4*width-1}:0] scaled;reg [${2*width-1}:0] quotient;reg [${3*width-1}:0] qp;reg signed [${3*width}:0] remainder;
         |begin product={{$width{1'b0}},a}*{{$width{1'b0}},b};scaled={{${2*width}{1'b0}},product}*{{${2*width}{1'b0}},${2*width}'d${barrett.mu}};quotient=scaled[${4*width-1}:${2*width}];qp={{$width{1'b0}},quotient}*{{${2*width}{1'b0}},${width}'d${field.q}};remainder=$$signed({${width+1}'d0,product})-$$signed({1'b0,qp});if(remainder<0)remainder=remainder+${field.q};if(remainder>=${field.q})remainder=remainder-${field.q};if(remainder>=${field.q})remainder=remainder-${field.q};rns_mul_$prime=remainder[${width-1}:0];end endfunction""".stripMargin
      s"""wire fa_valid_$prime,fb_valid_$prime,inv_valid_$prime;$wires
         |$pointMultiply
         |${top}ForwardA$prime f_a_$prime($forwardA);${top}ForwardB$prime f_b_$prime($forwardB);$products${top}Inverse$prime inv_$prime($inverse);""".stripMargin
    }
    val crtLogic=if emitCrt then Vector.tabulate(size) { coefficient =>
      val terms=basis.domains.indices.map { prime =>
        val modulus=basis.domains(prime).modulus.q;val partial=basis.combinedModulus/modulus;val factor=partial*partial.modInverse(modulus)
        s"o_${prime}_$coefficient*${combinedWidth}'d$factor"
      }
      s"wire [${2*combinedWidth-1}:0] crt_sum_$coefficient=${terms.mkString("+")};assign crt_$coefficient=crt_sum_$coefficient%${combinedWidth}'d${basis.combinedModulus};"
    }.mkString("\n") else ""
    s"""${cores.mkString("\n")}
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |${bodies.mkString("\n")}
       |$crtLogic
       |assign next_out=${basis.domains.indices.map(i=>s"inv_valid_$i").mkString("&")};
       |endmodule
       |""".stripMargin
