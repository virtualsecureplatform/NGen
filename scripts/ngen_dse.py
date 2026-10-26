#!/usr/bin/env python3
"""Parameter-based JSON interface to NGen; independent of benchmark task IDs."""
from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCHEMA = 'ngen-dse-design-v1'
RESULT_SCHEMA = 'ngen-dse-result-v1'
DEFAULTS = dict(direction='forward', ring='negacyclic', root='auto', psi='auto',
                architecture='streamed', reduction='montgomery', profile='baseline',
                protocol='ready-valid', transpose='indexed', interface='raw',
                input_order='natural', output_order='natural', radix=2, stage_groups=1)
CHOICES = dict(direction=('forward','inverse'), ring=('cyclic','negacyclic'),
               architecture=('streamed','compact','stage-parallel','fully-parallel','full-throughput'),
               reduction=('auto','barrett','montgomery','shoup'),
               profile=('baseline','f300','split-barrett'), protocol=('next','ready-valid'),
               transpose=('indexed','switch','distributed'), interface=('raw','axi4stream'),
               input_order=('natural','bitreversed'), output_order=('natural','bitreversed'))


def normalize(spec):
    """Use actual sizes/counts; concrete backend legality is decided by NGen."""
    required = {'schema','transform_size','streaming_width','modulus'}
    allowed = required | set(DEFAULTS) | {'pe_count'}
    if not isinstance(spec,dict) or set(spec)-allowed or required-set(spec):
        raise ValueError('expected schema, transform_size, streaming_width, modulus and documented optional fields')
    if spec['schema'] != SCHEMA:
        raise ValueError('unsupported design schema')
    c = {**DEFAULTS, **spec}
    for name in ('transform_size','streaming_width','radix'):
        value=c[name]
        if type(value) is not int or value<1 or value & (value-1):
            raise ValueError(name+' must be a positive power-of-two integer')
    if c['transform_size']<2 or c['radix']<2 or c['streaming_width']>c['transform_size']:
        raise ValueError('require N >= 2, radix >= 2 and streaming_width <= transform_size')
    for name in ('stage_groups','pe_count'):
        if name in c and (type(c[name]) is not int or c[name]<1):
            raise ValueError(name+' must be a positive integer')
    for name,values in CHOICES.items():
        if c[name] not in values:
            raise ValueError('invalid '+name)
    for name in ('modulus','root','psi'):
        value=c[name]
        if name!='modulus' and value=='auto':continue
        if isinstance(value,str) and value.isascii() and value.isdecimal():value=int(value)
        if type(value) is not int or value<1:
            raise ValueError(name+' must be a positive integer or decimal string'+(' or auto' if name!='modulus' else ''))
        c[name]=str(value)  # Preserve large field values for non-Python JSON clients.
    if int(c['modulus'])<3:raise ValueError('modulus must be at least 3')
    if c['ring']=='cyclic':
        if 'psi' in spec:raise ValueError('psi is only used for negacyclic transforms')
        c.pop('psi')
    return c


def arguments(c):
    result=[]
    for name,flag in [('transform_size','-n'),('streaming_width','-k'),('radix','-r')]:
        result += [flag,str(c[name].bit_length()-1)]
    for name,flag in [('modulus','-q'),('root','-root'),('psi','-psi'),
                      ('architecture','-architecture'),('reduction','-reduction'),
                      ('profile','-profile'),('protocol','-protocol'),('transpose','-transpose'),
                      ('input_order','-input-order'),('output_order','-output-order'),
                      ('pe_count','-pe'),('stage_groups','-stage-groups')]:
        if name in c:result += [flag,str(c[name])]
    if c['interface']!='raw':result += ['-interface',c['interface']]
    return result


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def invoke(command, executable, timeout):
    return subprocess.run(['bash',str(executable),*command],cwd=ROOT,
                          capture_output=True,text=True,timeout=timeout)


def evaluate(spec, action='plan', output_dir=None, executable=None, timeout=120):
    """Return structured generation evidence; does not simulate or synthesize."""
    if action not in ('plan','generate'):raise ValueError('invalid action')
    if timeout<=0:raise ValueError('timeout must be positive')
    c=normalize(spec)
    executable=Path(executable or ROOT/'ngen.bat').resolve()
    binary_sha=sha(executable)
    identity=dict(configuration=c,generator_sha256=binary_sha,adapter_sha256=sha(Path(__file__)))
    candidate_id=hashlib.sha256(json.dumps(identity,sort_keys=True,separators=(',',':')).encode()).hexdigest()
    result=dict(schema=RESULT_SCHEMA,action=action,candidate_id=candidate_id,**identity,
                qualified=False,evidence_scope='generation-only; no simulation, synthesis, or physical signoff')
    folder=None
    terminal='ntt' if c['direction']=='forward' else 'intt'
    args=arguments(c)
    if action=='generate':
        if output_dir is None:raise ValueError('generate requires --output-dir')
        folder=Path(output_dir).resolve()
        folder.mkdir(parents=True,exist_ok=False)
        args += ['-top','Candidate','-o',str(folder/'candidate.sv')]
    else:args=['plan',*args]
    result['command']=['bash',str(executable),*args,terminal]
    try:
        process=invoke([*args,terminal],executable,timeout)
        result.update(returncode=process.returncode,stdout=process.stdout,stderr=process.stderr)
        if process.returncode:
            result['status']='lowering-failed'
        elif sha(executable)!=binary_sha:
            result['status']='generator-changed'
        else:
            metadata=json.loads(process.stdout if action=='plan' else (folder/'candidate.json').read_text())
            if metadata.get('schema')!='ngen-design-v1':raise ValueError('unexpected NGen metadata schema')
            if metadata['transform_size']!=c['transform_size'] or metadata['streaming_width']!=c['streaming_width']:
                raise ValueError('emitted metadata does not match requested N/K')
            if int(metadata['modulus'])!=int(c['modulus']) or metadata['direction']!=c['direction']:
                raise ValueError('emitted metadata does not match field/direction')
            result.update(metadata=metadata,status='planned' if action=='plan' else 'generated')
            if folder:
                result['artifacts']={name:sha(folder/name) for name in ('candidate.sv','candidate.json')}
    except subprocess.TimeoutExpired:
        result['status']='timeout'
    except (OSError,ValueError,KeyError) as error:
        result.update(status='artifact-error',error=str(error))
    if folder:
        (folder/'result.json').write_text(json.dumps(result,indent=2,sort_keys=True)+'\n')
    return result


def main(argv=None):
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('action',choices=('capabilities','plan','generate'))
    parser.add_argument('--spec',type=Path)
    parser.add_argument('--output-dir',type=Path)
    parser.add_argument('--ngen',type=Path,default=ROOT/'ngen.bat')
    parser.add_argument('--timeout',type=int,default=120)
    args=parser.parse_args(argv)
    try:
        if args.timeout<=0:raise ValueError('timeout must be positive')
        if args.action=='capabilities':
            if args.spec or args.output_dir:raise ValueError('capabilities takes no spec or output directory')
            p=invoke(['capabilities'],args.ngen.resolve(),args.timeout)
            if p.returncode:raise ValueError('NGen capabilities failed: '+p.stderr)
            result=json.loads(p.stdout)
            if result.get('schema')!='ngen-capabilities-v1':raise ValueError('unexpected capability schema')
        else:
            if not args.spec:raise ValueError('--spec is required')
            if args.action=='plan' and args.output_dir:raise ValueError('plan takes no output directory')
            result=evaluate(json.loads(args.spec.read_text()),args.action,args.output_dir,args.ngen,args.timeout)
        print(json.dumps(result,indent=2,sort_keys=True))
        return 0 if result.get('status','planned') in ('planned','generated') else 1
    except (ValueError,OSError,subprocess.TimeoutExpired) as error:
        print(json.dumps(dict(schema=RESULT_SCHEMA,status='request-error',error=str(error))))
        return 2


if __name__=='__main__':
    raise SystemExit(main())
