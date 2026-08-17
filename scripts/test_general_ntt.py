#!/usr/bin/env python3
import subprocess,tempfile
from pathlib import Path
ROOT=Path(__file__).resolve().parents[1]

def expected(values,q,root):return [sum(value*pow(root,i*k,q) for i,value in enumerate(values))%q for k in range(len(values))]
def run(directory,name,n,q,root,conv=None):
 d=Path(directory);rtl=d/f"{name}.sv";args=["bash",str(ROOT/"ngen.bat"),"-size",str(n),"-q",str(q),"-root",str(root)]
 if conv is not None:args += ["-convolution-root",str(conv)]
 args += ["-o",str(rtl),"generalntt"];subprocess.run(args,cwd=ROOT,check=True,stdout=subprocess.DEVNULL)
 values=[(i*i+3*i-4)%q for i in range(n)];want=expected(values,q,root);w=q.bit_length()
 decl=''.join(f"reg [{w-1}:0] i{i};wire [{w-1}:0] o{i};" for i in range(n));conn=','.join([".clock(clock)",".reset(reset)",".next(next)",".next_out(next_out)"]+[f".i{i}(i{i})" for i in range(n)]+[f".o{i}(o{i})" for i in range(n)])
 assign=''.join(f"i{i}={w}'d{values[i]};" for i in range(n));checks=''.join(f"if(o{i}!=={w}'d{want[i]})$fatal(1,\"{name} mismatch {i}\");" for i in range(n))
 tb=d/f"{name}_tb.sv";tb.write_text(f"module test;reg clock=0,reset=1,next=0;wire next_out;{decl}always #5 clock=~clock;GeneralNtt dut({conn});initial begin repeat(2)@(posedge clock);@(negedge clock);reset=0;{assign}next=1;@(posedge clock);@(negedge clock);next=0;while(!next_out)@(posedge clock);#1;{checks}$display(\"PASS {name}\");$finish;end endmodule")
 sim=d/name;subprocess.run(["iverilog","-g2012","-s","test","-o",str(sim),str(rtl),str(tb)],check=True);subprocess.run(["vvp",str(sim)],check=True)
def main():
 with tempfile.TemporaryDirectory(prefix="ngen-general-test.") as d:run(d,"mixed_radix_6",6,13,4);run(d,"bluestein_5",5,241,87,44)
if __name__=="__main__":main()
