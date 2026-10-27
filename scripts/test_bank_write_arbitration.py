#!/usr/bin/env python3
"""Check parallel bank writes against an independent NTT oracle under backpressure."""
import itertools
import random
import subprocess
import tempfile
from pathlib import Path
from test_configurable_pipeline import bench
from test_streamed_ntt import Domain, transform
ROOT=Path(__file__).resolve().parents[1]
def main():
    domain=Domain('q193-negacyclic',32,193,pow(5,6,193),pow(5,3,193))
    assert pow(domain.twist,32,193)==192
    rng=random.Random(37)
    frames=[[rng.randrange(193) for _ in range(32)] for _ in range(3)]
    count=0
    with tempfile.TemporaryDirectory() as scratch:
        folder=Path(scratch)
        for (lanes,pe),inverse,groups in itertools.product(((2,1),(4,2),(8,4),(16,8)),(False,True),(1,2)):
            rtl=folder/'core.sv'; tb=folder/'test.sv'; sim=folder/'sim'
            args=['bash',str(ROOT/'ngen.bat'),'-n','5','-k',str(lanes.bit_length()-1),'-r','1','-q','193','-root',str(domain.root),'-psi',str(domain.twist),'-architecture','streamed','-pe',str(pe),'-stage-groups',str(groups),'-protocol','ready-valid','-reduction','montgomery','-bank-write-arbitration','onehot','-input-register','-issue-register','-output-register','-o',str(rtl),'intt' if inverse else 'ntt']
            subprocess.run(args,check=True,stdout=subprocess.DEVNULL)
            expected=[transform(domain,frame,inverse) for frame in frames]
            for stalls in (False,True):
                tb.write_text(bench(frames,expected,lanes,8,stalls,stalls))
                subprocess.run(['iverilog','-g2012','-s','test','-o',str(sim),str(rtl),str(tb)],check=True)
                subprocess.run(['vvp',str(sim)],check=True,stdout=subprocess.DEVNULL)
                count+=1
    print(f'PASS {count} registered-issue/onehot bank-write simulations')
if __name__=='__main__':main()
