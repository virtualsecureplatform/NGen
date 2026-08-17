#!/usr/bin/env python3
import subprocess,tempfile
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
N=8
PRIMES=(17,97)
LHS=[i*3-4 for i in range(N)]
RHS=[i*i-5 for i in range(N)]

def convolution(q):
    return [sum(((-1 if i+j>=N else 1)*LHS[i]*RHS[j]) for i in range(N) for j in range(N) if (i+j)%N==k)%q for k in range(N)]

def main():
    with tempfile.TemporaryDirectory(prefix="ngen-rns-test.") as directory:
        d=Path(directory);rtl=d/"rns.sv"
        subprocess.run(["bash",str(ROOT/"ngen.bat"),"-n","3","-rns-q","17,97","-rns-root","9,64","-rns-psi","3,8","-o",str(rtl),"rnspolymul"],cwd=ROOT,check=True,stdout=subprocess.DEVNULL)
        decl=[];connect=[".clock(clock)",".reset(reset)",".next(next)",".next_out(next_out)"];assign=[];checks=[]
        for p,q in enumerate(PRIMES):
            w=q.bit_length();expected=convolution(q)
            for i in range(N):
                decl.append(f"reg [{w-1}:0] a_{p}_{i},b_{p}_{i};wire [{w-1}:0] o_{p}_{i};")
                connect += [f".a_{p}_{i}(a_{p}_{i})",f".b_{p}_{i}(b_{p}_{i})",f".o_{p}_{i}(o_{p}_{i})"]
                assign.append(f"a_{p}_{i}={w}'d{LHS[i]%q};b_{p}_{i}={w}'d{RHS[i]%q};")
                checks.append(f"if(o_{p}_{i}!=={w}'d{expected[i]})$fatal(1,\"RNS mismatch p={p} i={i}\");")
        tb=d/"tb.sv";tb.write_text(f"""module test;reg clock=0,reset=1,next=0;wire next_out;{' '.join(decl)}always #5 clock=~clock;RnsPolynomialMultiplier dut({','.join(connect)});initial begin repeat(2)@(posedge clock);@(negedge clock);reset=0;{' '.join(assign)}next=1;@(posedge clock);@(negedge clock);next=0;while(!next_out)@(posedge clock);#1;{' '.join(checks)}$display("PASS RNS polynomial multiplier");$finish;end initial begin repeat(1000)@(posedge clock);$fatal(1,"timeout");end endmodule""")
        sim=d/"sim";subprocess.run(["iverilog","-g2012","-s","test","-o",str(sim),str(rtl),str(tb)],check=True);subprocess.run(["vvp",str(sim)],check=True)

if __name__=="__main__":main()
