package ngen.backend

import ngen.algebra.Modulus
import ngen.arithmetic.{PrimeAnalyzer,PrimeForm}

object PrimeReductionSystemVerilog:
  def emit(modulus:Modulus,top:String="NGenPrimeReducer"):String=
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"))
    val width=modulus.bitWidth
    PrimeAnalyzer.analyze(modulus).form match
      case PrimeForm.Proth(k,radixBits)=>
        val radixMask=(BigInt(1)<<radixBits)-1
        s"""module $top(input [${2*width-1}:0] product,output reg [$width-1:0] residue);
           |  localparam [$width-1:0] Q=$width'd${modulus.q};localparam [${2*width}:0] Q_EXT=${2*width+1}'d${modulus.q};localparam integer RADIX_BITS=$radixBits,K=$k;
           |  reg [${2*width}:0] m,sum,u;integer j;
           |  always @(*)begin m=(-product)&${2*width+1}'d$radixMask;sum=product+m*Q_EXT;u=sum>>RADIX_BITS;for(j=0;j<K+2;j=j+1)if(u>=Q_EXT)u=u-Q_EXT;residue=u[$width-1:0];end
           |endmodule
           |""".stripMargin
      case PrimeForm.PseudoMersenne(bits,constant)=>emitSparse(modulus,bits,constant,top)
      case PrimeForm.Goldilocks=>emitSparse(modulus,64,(BigInt(1)<<32)-1,top)
      case PrimeForm.SparseSolinas(bits,_)=>
        val constant=(BigInt(1)<<bits)-modulus.q
        emitSparse(modulus,bits,constant,top)
      case other=>throw new IllegalArgumentException(s"$other has no sparse/Proth reducer")

  private def emitSparse(modulus:Modulus,bits:Int,constant:BigInt,top:String):String=
    val width=modulus.bitWidth
    val workWidth=2*width+bits+4
    val boundary=BigInt(1)<<bits
    val mask=boundary-1
    s"""module $top(input [${2*width-1}:0] product,output reg [$width-1:0] residue);
       |  localparam signed [$workWidth-1:0] Q=$workWidth'sd${modulus.q},C=$workWidth'sd$constant;reg signed [$workWidth-1:0] value;integer j;
       |  always @(*)begin value={{${workWidth-2*width}{1'b0}},product};for(j=0;j<${2*width+2};j=j+1)begin if(value>=$workWidth'sd$boundary||value<0)value=(value&$workWidth'sd$mask)+(value>>>$bits)*C;end for(j=0;j<${2*width+2};j=j+1)begin if(value<0)value=value+Q;else if(value>=Q)value=value-Q;end residue=value[$width-1:0];end
       |endmodule
       |""".stripMargin
