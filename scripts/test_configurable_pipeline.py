#!/usr/bin/env python3
"""Independent NTT oracle, stalls/reset, and measured pipeline-latency checks."""
import itertools
import json
import random
import re
import subprocess
import tempfile
from pathlib import Path

from test_streamed_ntt import Domain,transform

ROOT=Path(__file__).resolve().parents[1]


def bench(inputs,outputs,lanes,width,stalls,abort):
    flat_in=[v for frame in inputs for v in frame]
    flat_out=[v for frame in outputs for v in frame]
    beats=len(inputs[0])//lanes
    declarations='\n'.join(f'reg [{width-1}:0] i{l};wire [{width-1}:0] o{l};' for l in range(lanes))
    ports=','.join(f'.i{l}(i{l}),.o{l}(o{l})' for l in range(lanes))
    init='\n'.join(f'source[{i}]={width}\'d{v};expected[{i}]={width}\'d{flat_out[i]};' for i,v in enumerate(flat_in))
    drive=''.join(f'i{l}=source[sent*{lanes}+{l}];' for l in range(lanes))
    check=''.join(f'if(o{l}!==expected[received*{lanes}+{l}])$fatal(1,"oracle/stall mismatch beat=%0d lane={l}",received);' for l in range(lanes))
    return f'''module test;
reg clock=0,reset=1,in_valid=0,out_ready=0;wire in_ready,out_valid;
{declarations}
reg [{width-1}:0] source[0:{len(flat_in)-1}],expected[0:{len(flat_out)-1}];
integer sent=0,received=0,cycle=0,first_input=-1,first_output=-1,second_output=-1;
reg epoch={0 if abort else 1};reg source_stalled=0;
main dut(.clock(clock),.reset(reset),.in_valid(in_valid),.in_ready(in_ready),.out_valid(out_valid),.out_ready(out_ready),{ports});
always #5 clock=~clock;
always @(negedge clock)begin
 if(reset)begin in_valid=0;out_ready=0;end
 else begin
  out_ready={"cycle%5!=0 && cycle%5!=1" if stalls else "1"};
  if(!source_stalled)begin
   in_valid=sent<((epoch)?{len(inputs)*beats}:{beats}) && {"cycle%7!=2" if stalls else "1"};
   if(sent<{len(inputs)*beats})begin {drive} end
  end
 end
end
always @(posedge clock)begin
 if(reset)begin sent=0;received=0;cycle=0;first_input=-1;first_output=-1;second_output=-1;source_stalled=0;end
 else begin
  cycle=cycle+1;source_stalled=in_valid && !in_ready;
  if(in_valid&&in_ready)begin if(sent==0)first_input=cycle;sent=sent+1;end
  if(out_valid)begin
   if(received>={len(inputs)*beats})$fatal(1,"extra output");
   {check}
   if(out_ready)begin
    if(received==0)first_output=cycle;
    if(received=={beats})second_output=cycle;
    received=received+1;
   end
  end
  if(epoch&&received=={len(inputs)*beats})begin
   $display("MEASURE latency=%0d interval=%0d",first_output-first_input,second_output-first_output);$finish;
  end
 end
end
initial begin
{init}
repeat(2)@(negedge clock);reset=0;
{f'wait(sent=={beats});repeat(3)@(negedge clock);reset=1;repeat(2)@(negedge clock);epoch=1;reset=0;' if abort else ''}
end
initial begin repeat(20000)@(posedge clock);$fatal(1,"timeout");end
endmodule
'''


def main():
    count=0
    with tempfile.TemporaryDirectory(prefix='ngen-configurable-') as scratch:
        folder=Path(scratch)
        domain=Domain('q97-negacyclic',16,97,8,28)  # 28 has order 32; 28^2 = 8 mod 97.
        rng=random.Random(42)
        frames=[[rng.randrange(97) for _ in range(16)] for _ in range(3)]
        timings={}
        for m,r,registered,inverse,groups,input_registered in itertools.product((1,3),(3,4,5),(False,True),(False,True),(1,2),(False,True)):
            args=['bash',str(ROOT/'ngen.bat'),'-n','4','-k','2','-r','1','-q','97','-root','8','-psi','28',
                  '-architecture','streamed','-pe','2','-stage-groups',str(groups),'-protocol','ready-valid',
                  '-reduction','montgomery','-multiplier-pipeline-stages',str(m),'-reduction-pipeline-stages',str(r)]
            if registered:args+=['-output-register']
            if input_registered:args+=['-input-register']
            rtl=folder/'core.sv';tb=folder/'test.sv';sim=folder/'sim'
            subprocess.run([*args,'-o',str(rtl),'intt' if inverse else 'ntt'],check=True,stdout=subprocess.DEVNULL)
            metadata=json.loads(rtl.with_suffix('.json').read_text())
            params=metadata['architecture_parameters']
            assert params['butterfly_pipeline_latency']==2+m+r
            assert params['input_register']==int(input_registered)
            expected=[transform(domain,frame,inverse) for frame in frames]
            for stalls in (False,True):
                tb.write_text(bench(frames,expected,4,7,stalls,stalls))
                subprocess.run(['iverilog','-g2012','-s','test','-o',str(sim),str(rtl),str(tb)],check=True)
                result=subprocess.run(['vvp',str(sim)],check=True,capture_output=True,text=True)
                if not stalls:
                    match=re.search(r'latency=(\d+) interval=(\d+)',result.stdout)
                    latency,interval=map(int,match.groups())
                    timings[m,r,registered,inverse,groups,input_registered]=(latency,metadata['latency'])
                count+=1
        # Independently measured changes must match declared changes, including the elastic register.
        for (m,r,registered,inverse,groups,input_registered),(measured,declared) in timings.items():
            ref_measured,ref_declared=timings[1,4,False,inverse,groups,False]
            assert measured-ref_measured==declared-ref_declared,((m,r,registered,inverse,groups),measured,declared,ref_measured,ref_declared)
        print(f'PASS {count} core simulations: forward/inverse, groups, arithmetic partitions, stalls, reset, metadata latency deltas')


if __name__=='__main__':main()
