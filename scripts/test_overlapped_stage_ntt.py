#!/usr/bin/env python3
"""Check spatial frame overlap with independent arithmetic, stalls/reset, and exact II."""
import itertools
import json
import random
import re
import subprocess
import tempfile
from pathlib import Path

from test_streamed_ntt import Domain,transform,bit_reverse
from test_configurable_pipeline import bench

ROOT=Path(__file__).resolve().parents[1]


def run_case(folder,n,q,lanes,inverse,reduction,ring='negacyclic',orders='natural'):
    order=n*(2 if ring=='negacyclic' else 1)
    base=2
    while True:
        primitive=pow(base,(q-1)//order,q)
        if pow(primitive,order//2,q)==q-1:break
        base+=1
    root=primitive*primitive%q if ring=='negacyclic' else primitive
    psi=primitive if ring=='negacyclic' else None
    domain=Domain('independent',n,q,root,psi)
    rng=random.Random(891)
    # More frames than pipeline slots, so output stalls must exercise upstream pressure.
    frames=[[rng.randrange(q) for _ in range(n)] for _ in range(n.bit_length()+4)]
    frames[0]=[0]*n;frames[1]=[q-1]*n
    expected=[transform(domain,frame,inverse) for frame in frames]
    bits=n.bit_length()-1
    if orders=='bitreversed':
        frames=[[frame[bit_reverse(i,bits)] for i in range(n)] for frame in frames]
        expected=[[frame[bit_reverse(i,bits)] for i in range(n)] for frame in expected]
    args=['bash',str(ROOT/'ngen.bat'),'-n',str(bits),'-k',str(lanes.bit_length()-1),
          '-q',str(q),'-root',str(root),'-architecture','stage-parallel','-frame-overlap',
          '-protocol','ready-valid','-reduction',reduction,'-input-order',orders,'-output-order',orders]
    if psi is not None:args+=['-psi',str(psi)]
    rtl=folder/'core.sv';tb=folder/'test.sv';sim=folder/'sim'
    subprocess.run([*args,'-o',str(rtl),'intt' if inverse else 'ntt'],check=True,stdout=subprocess.DEVNULL)
    metadata=json.loads(rtl.with_suffix('.json').read_text())
    beats=n//lanes
    assert metadata['initiation_interval']==beats and metadata['minimum_gap']==0 and metadata['full_throughput']
    for stalls in (False,True):
        text=bench(frames,expected,lanes,q.bit_length(),stalls,stalls)
        if stalls:
            text=text.replace('out_ready=cycle%5!=0 && cycle%5!=1;',
                              f'out_ready=cycle>{2*n} && cycle%5!=0 && cycle%5!=1;')
        if not stalls:
            # Check every frame start, not merely the first pair.
            text=text.replace('main dut(', 'integer previous_input=-1,previous_output=-1;\nmain dut(',1)
            text=text.replace('if(sent==0)first_input=cycle;',f'''if(sent%{beats}==0)begin
if(previous_input>=0 && cycle-previous_input!={beats})$fatal(1,"input frame gap");previous_input=cycle;end
if(sent==0)first_input=cycle;''',1)
            text=text.replace('if(received==0)first_output=cycle;',f'''if(received%{beats}==0)begin
if(previous_output>=0 && cycle-previous_output!={beats})$fatal(1,"output frame gap");previous_output=cycle;end
if(received==0)first_output=cycle;''',1)
        tb.write_text(text)
        subprocess.run(['iverilog','-g2012','-s','test','-o',str(sim),str(rtl),str(tb)],check=True)
        p=subprocess.run(['vvp',str(sim)],capture_output=True,text=True)
        if p.returncode:raise AssertionError(((n,q,lanes,inverse,reduction,ring,orders,stalls),p.stdout,p.stderr))
        if not stalls:
            match=re.search(r'latency=(\d+) interval=(\d+)',p.stdout)
            measured,interval=map(int,match.groups())
            assert measured==metadata['latency'],(measured,metadata['latency'])
            assert interval==beats
    return 2


def main():
    count=0
    with tempfile.TemporaryDirectory(prefix='ngen-overlapped-') as scratch:
        folder=Path(scratch)
        for lanes,inverse,reduction,ring in itertools.product((1,2,4,8,16),(False,True),
                ('barrett','montgomery','shoup'),('cyclic','negacyclic')):
            count+=run_case(folder,16,97,lanes,inverse,reduction,ring)
        for inverse in (False,True):
            count+=run_case(folder,16,97,4,inverse,'montgomery',orders='bitreversed')
            count+=run_case(folder,2,17,2,inverse,'montgomery')
        for reduction in ('barrett','montgomery','shoup'):
            count+=run_case(folder,8,18446744069414584321,4,True,reduction)
        for inverse in (False,True):
            count+=run_case(folder,512,65537,8,inverse,'montgomery')
    print(f'PASS {count} overlapped-stage simulations: arithmetic, frame II=N/L, exact latency, stalls and reset')


if __name__=='__main__':main()
