package ngen.backend

/** Seven registered arithmetic stages; the microcoded scheduler writes back on
  * cycle eight, before issuing a dependent bundle. No multicycle timing waiver.
  */
object YataMicroLanePipeline:
  val IssueCycles = 8
  val definition: String =
    """module YataMicroLanePipeline(input clock,input [3:0] kind,input signed [53:0] a,b,input signed [26:0] constant,input [1:0] radix,number,output reg signed [53:0] out_a,out_b);
      |  localparam signed [53:0] P=54'sd40960001;
      |  reg [3:0] k1,k2,k3,k4,k5,k6;
      |  reg [1:0] r1,n1;
      |  reg signed [26:0] c1,ma4,mb4;
      |  reg signed [53:0] a1,b1,x2,y2,u2,v2,a3,b3,ra4,rb4,ra5,rb5,ra6,rb6;
      |  reg signed [53:0] ma5,mb5,ta6,tb6;
      |  reg signed [26:0] ma5low,mb5low;
      |  function automatic signed [26:0] correction(input signed [53:0] x);reg signed[53:0]v;begin v=x;if(v>=P)v=v-P;else if(v<=-P)v=v+P;correction=v[26:0];end endfunction
      |  function automatic signed [26:0] finish_reduction(input signed[53:0]x,t);begin finish_reduction=$signed(x[53:27])-$signed(t[53:27]);end endfunction
      |  always @(posedge clock) begin
      |    k1<=kind;a1<=a;b1<=b;c1<=constant;r1<=radix;n1<=number;
      |    k2<=k1;u2<=0;v2<=0;
      |    case(k1)
      |      4:begin x2<=(r1==2&&n1==1)||(r1==3&&n1==2)?(a1*25)<<<8:(r1==3&&n1==1)?(a1*5)<<<4:(r1==3&&n1==3)?(a1*125)<<<12:a1;y2<=0;end
      |      5,7:begin x2<=a1;y2<=(b1*25)<<<8;end
      |      6:begin x2<=(a1*5)<<<4;y2<=(b1*125)<<<12;u2<=(a1*125)<<<12;v2<=(b1*5)<<<4;end
      |      9:begin x2<=(a1*125)<<<12;y2<=(b1*5)<<<4;u2<=(a1*5)<<<4;v2<=(b1*125)<<<12;end
      |      10:begin x2<=$signed(a1[26:0])*c1;y2<=0;end
      |      default:begin x2<=a1;y2<=b1;end
      |    endcase
      |    k3<=k2;
      |    case(k2)
      |      4,10:begin a3<=x2;b3<=0;end
      |      6:begin a3<=x2+y2;b3<=u2+v2;end
      |      7:begin a3<=x2-y2;b3<=x2+y2;end
      |      9:begin a3<=-x2-y2;b3<=-u2-v2;end
      |      default:begin a3<=x2+y2;b3<=x2-y2;end
      |    endcase
      |    k4<=k3;ra4<=a3;rb4<=k3==8?-((b3*25)<<<8):b3;
      |    ma4<=a3[26:0]-((a3[26:0]*625)<<16);mb4<=b3[26:0]-((b3[26:0]*625)<<16);
      |    k5<=k4;ra5<=ra4;rb5<=rb4;ma5<=ma4*625;mb5<=mb4*625;ma5low<=ma4;mb5low<=mb4;
      |    k6<=k5;ra6<=ra5;rb6<=rb5;ta6<=(ma5<<<16)+ma5low;tb6<=(mb5<<<16)+mb5low;
      |    case(k6)
      |      1:begin out_a<=correction(ra6);out_b<=correction(rb6);end
      |      2,8:begin out_a<=correction(ra6);out_b<=rb6;end
      |      3:begin out_a<=finish_reduction(ra6,ta6);out_b<=finish_reduction(rb6,tb6);end
      |      10:begin out_a<=finish_reduction(ra6,ta6);out_b<=0;end
      |      4,5,6,7,9:begin out_a<=ra6;out_b<=rb6;end
      |      default:begin out_a<=0;out_b<=0;end
      |    endcase
      |  end
      |endmodule
      |""".stripMargin
