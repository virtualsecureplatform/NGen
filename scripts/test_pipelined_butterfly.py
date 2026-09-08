#!/usr/bin/env python3
"""Independent arithmetic, exact latency, tag, bubble, and reset checks for PE pipelines."""
from __future__ import annotations
import random
import subprocess
import tempfile
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]


def testbench(q: int,reduction: str) -> str:
    width=q.bit_length();radix=1<<width;latency=7 if reduction=='montgomery' else 3
    rng=random.Random(4291)
    operations=[(kind,a,b,c) for kind in (1,2,3) for a,b,c in ((0,0,0),(q-1,q-1,q-1),(q-1,1,1),(1,q-1,0),(q-1,q-2,1))]
    operations += [(rng.randrange(1,4),rng.randrange(q),rng.randrange(q),rng.randrange(q)) for _ in range(96)]
    drives=[];checks=[]
    for tag,(kind,a,b,c) in enumerate(operations):
        encoded=c*radix%q if reduction=='montgomery' else c
        precon=c*radix//q if reduction=='shoup' else 0
        drives.append(f'@(negedge clock);valid_in=1;kind_in={kind};a_in={width}\'d{a};b_in={width}\'d{b};constant_in={width}\'d{encoded};precon_in={width}\'d{precon};tag_in={tag};')
        if kind==1:out0,out1=a*c%q,0
        elif kind==2:out0,out1=(a+b*c)%q,(a-b*c)%q
        else:out0,out1=(a+b)%q,((b-a)*c)%q
        checks.append(f'{tag}:if(out0!=={width}\'d{out0}||out1!=={width}\'d{out1})$fatal(1,"arithmetic tag={tag}");')
        if tag==3:drives.append('@(negedge clock);reset=1;valid_in=0;repeat(2)@(negedge clock);reset=0;')
        if tag%11==7:drives.append('@(negedge clock);valid_in=0;repeat(3)@(negedge clock);')
    return f'''module test;
reg clock=0,reset=1,valid_in=0;reg [1:0] kind_in;
reg [{width-1}:0] a_in,b_in,constant_in,precon_in;reg [15:0] tag_in;
wire valid_out;wire [{width-1}:0] out0,out1;wire [15:0] tag_out;
integer head=0,tail=0,cycle=0,received=0;integer tags[0:255],due[0:255];
always #5 clock=~clock;
NGenPipelinedButterfly #(.TAG_WIDTH(16)) dut(clock,reset,valid_in,kind_in,a_in,b_in,constant_in,precon_in,tag_in,valid_out,out0,out1,tag_out);
always @(posedge clock)begin
 cycle=cycle+1;
 if(reset)begin head=0;tail=0;end
 else if(valid_in)begin tags[tail]=tag_in;due[tail]=cycle+{latency-1};tail=tail+1;end
 #1;
 if(valid_out)begin
  if(reset||head==tail||tag_out!=tags[head]||cycle!=due[head])$fatal(1,"tag/latency/reset mismatch cycle=%0d",cycle);
  case(tag_out) {' '.join(checks)} default:$fatal(1,"unknown tag");endcase
  head=head+1;received=received+1;
 end
 else if(head<tail&&due[head]==cycle)$fatal(1,"missing output");
end
initial begin
 repeat(2)@(negedge clock);reset=0;
{chr(10).join(drives)}
 @(negedge clock);valid_in=0;repeat({latency+3})@(negedge clock);
 if(head!=tail||received<100)$fatal(1,"incomplete test");
 $display("PASS {reduction} q={q}: arithmetic, latency, tags, bubbles, reset");$finish;
end
initial begin repeat(1000)@(posedge clock);$fatal(1,"timeout");end
endmodule
'''


def main():
    with tempfile.TemporaryDirectory(prefix='ngen-pipeline-test.') as temporary:
        d=Path(temporary)
        for q,reduction in [(17,r) for r in ('barrett','montgomery','shoup')]+[(q,'montgomery') for q in (2147484161,9007199255560193,18446744069414584321)]:
            name=f'{reduction}-{q}';rtl=d/f'{name}.sv';tb=d/f'{name}_tb.sv';sim=d/name
            subprocess.run(['bash',str(ROOT/'ngen.bat'),'-q',str(q),'-reduction',reduction,'-o',str(rtl),'butterflypipeline'],cwd=ROOT,check=True,stdout=subprocess.DEVNULL)
            tb.write_text(testbench(q,reduction))
            subprocess.run(['iverilog','-g2012','-s','test','-o',str(sim),str(rtl),str(tb)],check=True)
            subprocess.run(['vvp',str(sim)],check=True)

if __name__=='__main__':main()
