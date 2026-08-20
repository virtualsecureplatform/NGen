package ngen.backend

import ngen.algebra.Modulus
import ngen.arithmetic.{PrimeAnalyzer,PrimeForm}

object SparseFoldFunction:
  def emit(field:Modulus,name:String="field_mul"):String=
    val width=field.bitWidth
    val(bits,constant)=PrimeAnalyzer.analyze(field).form match
      case PrimeForm.Goldilocks=>(64,(BigInt(1)<<32)-1)
      case PrimeForm.PseudoMersenne(b,c)=>(b,c)
      case PrimeForm.SparseSolinas(b,_)=>(b,(BigInt(1)<<b)-field.q)
      case other=>throw new IllegalArgumentException(s"$other is not sparse-fold compatible")
    val workWidth=3*width+bits+4
    val boundary=BigInt(1)<<bits;val mask=boundary-1
    s"""function automatic [$width-1:0] $name(input [$width-1:0] a,input [$width-1:0] b);
       |  reg [${2*width-1}:0] product;reg signed [$workWidth-1:0] value;integer fold_index;
       |  begin product=a*b;value={{${workWidth-2*width}{1'b0}},product};for(fold_index=0;fold_index<${2*width+2};fold_index=fold_index+1)begin if(value>=$workWidth'sd$boundary||value<0)value=(value&$workWidth'sd$mask)+(value>>>$bits)*$workWidth'sd$constant;end for(fold_index=0;fold_index<${2*width+2};fold_index=fold_index+1)begin if(value<0)value=value+$workWidth'sd${field.q};else if(value>=$workWidth'sd${field.q})value=value-$workWidth'sd${field.q};end $name=value[$width-1:0];end
       |endfunction""".stripMargin
