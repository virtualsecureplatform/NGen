# Building your own design space exploration framework

NGen is a hardware generator that an external grid, random, Bayesian, or LLM
search controller can call. You do not need to modify Scala to select existing
architectures. Use the CLI to discover controls, validate candidates, emit RTL,
and collect metadata; let your framework own simulation, synthesis, physical
implementation, objectives, budgets, and results.

## Build once, then call from your framework

From the NGen repository root, with Java 17+ and sbt available:

```bash
sbt test assembly
bash ngen.bat version
bash ngen.bat capabilities
```

`ngen.bat` is the assembled executable, despite its Windows-style extension;
`bash ngen.bat ...` works on Linux. Rebuild after generator source changes.
Keep the source revision and executable SHA-256 in the experiment manifest so
an older assembly cannot silently stand in for edited sources.

The native machine-readable commands are:

| Command | Success output | What it establishes |
| --- | --- | --- |
| `bash ngen.bat capabilities` | JSON with schema `ngen-capabilities-v1` | Available axes and broad restrictions. Their Cartesian product is not a promise that every combination works. |
| `bash ngen.bat plan <options> ntt` | JSON with schema `ngen-design-v1` | The same generation lowering succeeded in temporary storage. No RTL remains at the requested output path. |
| `bash ngen.bat <options> -o candidate.sv ntt` | Human-readable messages, plus `candidate.sv` and `candidate.json` | Emitted design and declared metadata. Read the JSON file rather than parsing progress messages. |

`plan` is not an inexpensive analytical estimator: it performs real lowering
and then deletes temporary files. Give it a timeout, cache its result, and do
not call it repeatedly for an unchanged candidate. Successful lowering is not
functional qualification or timing closure. Treat a nonzero exit as failure;
retain stdout, stderr, and the return code for diagnosis.

## Recommended parameter-based JSON interface

Use [scripts/ngen_dse.py](../scripts/ngen_dse.py) as a standalone adapter for your
framework. It accepts a concrete design, not a task ID, and does not import the
LLM-NTT benchmark catalog or require a sibling evaluator checkout. Grid and LLM
policies use the same request format.

Start with [examples/dse/ntt512.json](../examples/dse/ntt512.json):

```bash
python3 scripts/ngen_dse.py capabilities
python3 scripts/ngen_dse.py plan --spec examples/dse/ntt512.json
python3 scripts/ngen_dse.py generate --spec examples/dse/ntt512.json \
  --output-dir build/my-ntt-candidate
```

All command results are JSON on stdout. `capabilities` returns NGen's native
`ngen-capabilities-v1` contract. `plan` and `generate` return the adapter's
`ngen-dse-result-v1` result. Human-readable generator stdout/stderr are captured
inside that result rather than mixed with JSON. `--ngen` selects another built
executable and `--timeout` sets a positive command deadline in seconds.
Generation creates a new directory and refuses to overwrite an existing one.

| Request field | Meaning / default |
| --- | --- |
| `schema` | Required: `ngen-dse-design-v1`. Unknown fields are rejected. |
| `transform_size`, `streaming_width`, `modulus` | Required actual N, actual lane count, and field prime. Sizes must be powers of two; width cannot exceed N. Use a decimal string for large primes to avoid floating-point conversion in JSON clients. |
| `direction`, `ring` | `forward` / `inverse`; `negacyclic` / `cyclic`. Defaults: forward negacyclic. |
| `root`, `psi` | Positive integers/decimal strings or `auto`. Defaults: auto; psi is allowed only for negacyclic requests. |
| `architecture` | `streamed`, `compact`, `stage-parallel`, `fully-parallel`, or `full-throughput`. Default: streamed. |
| `pe_count`, `radix`, `stage_groups` | Actual counts/values. PE count is omitted by default so the generator chooses it; radix defaults to 2, stage groups to 1. |
| `multiplier_pipeline_stages`, `reduction_pipeline_stages`, `output_register` | Optional native NTT controls: 1/3 main-product stages, 3/4/5 reduction stages, and an elastic output register. See [pipeline controls](pipeline-controls.md) for restrictions, defaults, and real arithmetic partitions. |
| `reduction`, `profile` | Defaults: `montgomery`, `baseline`; legal alternatives come from `capabilities` and concrete lowering. |
| `protocol`, `transpose`, `interface` | Defaults: `ready-valid`, `indexed`, `raw`. AXI wrapping is `axi4stream`. |
| `input_order`, `output_order` | `natural` or `bitreversed`; default: natural. |

The adapter normalizes types and actual sizes. It deliberately delegates
backend-specific legality, field/root validation, and lowering to NGen. For
example, a stage-parallel request needs `protocol: next`, radix 2, and no
`pe_count`. Do not populate every optional field using defaults from another
architecture. This interface covers custom complete power-of-two NTT/INTT
transforms; presets and incomplete transforms remain accessible through the
native CLI.

| Result field | Meaning |
| --- | --- |
| `status` | `planned`, `generated`, `lowering-failed`, `timeout`, `generator-changed`, `artifact-error`, or `request-error`. |
| `candidate_id` | SHA-256 of the normalized requested configuration, generator executable hash, and adapter hash. Plan/generate share the identity for the same inputs. |
| `configuration` | Normalized request, retaining actual N/L/R values and field values as decimal strings. |
| `metadata` | Native declared design metadata when lowering succeeds, including resolved architecture and cycle contracts. |
| `generator_sha256`, `adapter_sha256` | Executable and adapter identity; retain source revision/build provenance separately in your experiment manifest. |
| `command`, `returncode`, `stdout`, `stderr` | The executed command and available process diagnostics. Timeout/request errors may not have all process fields. |
| `artifacts` | Generated RTL and native metadata SHA-256 values, for successful generation. |
| `qualified`, `evidence_scope` | Always generation-only / unqualified. The adapter never asserts functional or physical signoff. |

Exit codes are 0 for success, 1 for a failed lowering/generation attempt, and 2
for invalid requests or unavailable prerequisites. Generation saves
`candidate.sv`, `candidate.json`, and `result.json` in the requested directory.
The final file contains the same result object as stdout. Planning leaves no
candidate files. There is no automatic result cache or resumed evaluation;
your framework owns those policies and must include its evaluator/context in
its cache identity.

A Python controller can call the same interface without launching a Python
subprocess. From the repository root:

```python
import json
from scripts.ngen_dse import evaluate

request = json.load(open('examples/dse/ntt512.json'))
request.update(streaming_width=16, pe_count=8)
result = evaluate(request, action='plan', timeout=120)
if result['status'] == 'planned':
    print(result['candidate_id'], result['metadata']['initiation_interval'])
```

Treat this as a generator boundary: attach your own independent simulation and
physical evaluation before feeding achieved objectives back to a policy. The
older `ngen_llm_ntt.py --task ...` path is only a compatibility adapter for its
fixed benchmark evaluator, not the custom-DSE API.

Run the adapter's Python validation/integration tests after assembly:

```bash
python3 -m unittest discover -s tests/python
```

## Fix the workload before choosing search axes

Choose transform size, prime modulus, cyclic/negacyclic semantics, direction,
input/output ordering, and interface. Keep those fixed when comparing resource
and performance tradeoffs for the same task.

The CLI uses logarithms for sizes, unlike most result-table labels:

| Framework value | CLI argument | Example |
| --- | --- | --- |
| Transform size `N` | `-n log2(N)` | N=512 means `-n 9`. |
| Streaming width `K` / lanes | `-k log2(K)` | Four lanes means `-k 2`. |
| Butterfly radix `R` | `-r log2(R)` | Radix 8 means `-r 3`. |
| PEs per engine | `-pe P` | Two PEs means `-pe 2`, not `-pe 1`. |
| Independently buffered stage groups | `-stage-groups U` | Two groups means `-stage-groups 2`; not two stages per group. |
| Field modulus | `-q q` | Prime 97 means `-q 97`. |
| Complete cyclic transform | `-root auto` | Requires an appropriate exact-order Nth root. |
| Complete negacyclic transform | `-root auto -psi auto` | Requires an appropriate 2Nth root/twist; normally `2*N` divides `q-1`. |
| Direction | Final terminal `ntt` or `intt` | Options precede the terminal. |

For example, validate a complete N=16 negacyclic forward NTT over q=97:

```bash
bash ngen.bat plan -n 4 -k 1 -r 1 -q 97 -root auto -psi auto \
  -architecture streamed -pe 1 -stage-groups 2 \
  -protocol ready-valid -transpose indexed -profile baseline \
  -reduction montgomery ntt
```

A single transform is not a complete polynomial multiplier. A product framework
must also specify operand bounds, coefficient representation, forward/inverse
composition, pointwise multiplication, scaling/twists, and reconstruction.
For products modulo `2^32`, using one convenient NTT prime is not generally
sufficient: choose and verify an RNS dynamic range/CRT reconstruction or another
correct lifting scheme. See the README's `rnspolymul` example for NGen's
matched-prime composition path. Complete-product integration controls used by
an external project, such as `product_profile: registered`, are not NGen CLI
`-profile` values.

## Choose a legal architecture space

These rows describe custom complete power-of-two transforms. Presets have
separate constraints; query `capabilities` and use `plan` for every concrete
candidate rather than transferring constraints between families.

| Backend | Architecture meaning | Useful controls and important restrictions |
| --- | --- | --- |
| `streamed` / `compact` | Reusable banked butterfly PEs; these names select the hardware-reusing custom path. | Sweep lanes, PEs, radix, reduction, and profile subject to lowering constraints. Radix logarithm must divide `log2(N)`. |
| `stage-parallel` | Unrolled radix-2 stage datapaths with registered stage boundaries. | Complete cyclic/negacyclic transforms, `-r 1`, `-protocol next`, no `-pe` override and no runtime-control override. Do not pass the streamed PE axis to this backend. |
| `fully-parallel` / generic `full-throughput` | Acyclic parallel custom datapath. | Requires K=N, radix 2, natural stream order, complete transform, and no PE override. Preset full-throughput implementations have their own rules. |
| `auto` | Chooses a lowering from the request. | Useful for interactive use; prefer an explicit backend when studying architecture effects. Record the resolved metadata architecture. |

For grouped streamed designs (`U > 1`), use a complete radix-2 transform,
`ready-valid`, raw interface, and indexed transpose. Each of U groups has P
PEs: do not confuse per-group `pe_count` with `pe_count_total`. U must be between
one and `log2(N)`. More groups add both coefficient storage and opportunities
for overlap; this is not an SDF/MDC architecture.

Reduction choices include `barrett`, `montgomery`, `shoup`, and `auto`. `auto`
can select a specialized implementation based on the prime, so record both
requested and resolved settings. Profiles include `baseline`, `f300`, and
`split-barrett`; the latter requires custom fully-parallel lowering. `f300`
is a scheduling/pipeline intent, not a promise of 300 MHz after routing.
Transpose and protocol choices also constrain legal combinations. The tables
are an orientation; the installed build's `capabilities` and `plan` are the
executable authority.

## A runnable small grid driver

Save the following as `sweep.py` in the NGen repository root and run
`python3 sweep.py`. Python uses only its standard library; Java and the assembled
NGen executable must be available. It validates and emits twelve small designs:
two PE counts × two stage-group counts × three reductions. It does not run
simulation, synthesis, or routing, and therefore records **generated**, not
**qualified**, designs. Each invocation creates a new output directory.

```python
import hashlib
import itertools
import json
from pathlib import Path
import subprocess
import tempfile

root = Path.cwd().resolve()
exe = root / 'ngen.bat'
output = Path(tempfile.mkdtemp(prefix='ngen-dse-', dir=root))

def call(args):
    return subprocess.run(['bash', str(exe), *args], cwd=root,
                          capture_output=True, text=True, timeout=120)

cap = call(['capabilities'])
cap.check_returncode()
capabilities = json.loads(cap.stdout)
assert capabilities['schema'] == 'ngen-capabilities-v1'
manifest = {
    'capabilities': capabilities,
    'executable_sha256': hashlib.sha256(exe.read_bytes()).hexdigest(),
    'workload': {'n': 16, 'q': 97, 'ring': 'negacyclic', 'direction': 'forward'},
    'trials': [],
}
for pe, groups, reduction in itertools.product(
        [1, 2], [1, 2], ['barrett', 'montgomery', 'shoup']):
    config = dict(pe=pe, stage_groups=groups, reduction=reduction)
    identity = json.dumps(config, sort_keys=True, separators=(',', ':'))
    trial = output / hashlib.sha256(identity.encode()).hexdigest()[:16]
    trial.mkdir()
    args = ['-n', '4', '-k', '1', '-r', '1', '-q', '97',
            '-root', 'auto', '-psi', 'auto', '-architecture', 'streamed',
            '-pe', str(pe), '-stage-groups', str(groups),
            '-reduction', reduction, '-profile', 'baseline',
            '-protocol', 'ready-valid', '-transpose', 'indexed']
    record = dict(configuration=config, directory=trial.name, status='pending')
    try:
        for stage, argv in [('plan', ['plan', *args, 'ntt']),
                            ('generate', [*args, '-o', str(trial / 'core.sv'), 'ntt'])]:
            result = call(argv)
            (trial / (stage + '.stdout')).write_text(result.stdout)
            (trial / (stage + '.stderr')).write_text(result.stderr)
            record[stage + '_returncode'] = result.returncode
            if result.returncode:
                record['status'] = stage + '-failed'
                break
            if stage == 'plan':
                metadata = json.loads(result.stdout)
                assert metadata['schema'] == 'ngen-design-v1'
        else:
            record['metadata'] = json.loads((trial / 'core.json').read_text())
            record['rtl_sha256'] = hashlib.sha256((trial / 'core.sv').read_bytes()).hexdigest()
            record['status'] = 'generated'
    except subprocess.TimeoutExpired:
        record['status'] = 'timeout'
    manifest['trials'].append(record)
    (output / 'results.json').write_text(json.dumps(manifest, indent=2) + '\n')
print(output / 'results.json')
```

The short IDs above are unique only within this fixed-workload demonstration.
A production cache identity must also include the entire workload, all effective
options, source/executable identity, top-level/wrapper RTL, evaluator and corpus,
and tool/target settings. Save the selected candidate before evaluation, write
results atomically, and lock shared caches when running multiple workers. Do not
reuse a previous success merely because its readable name matches.

To include stage-parallel candidates, create a separate family with
`-architecture stage-parallel -r 1 -protocol next`, omit `-pe`, and keep stage
groups at one. Do not blindly append these candidates to the streamed argument
list: the interface and allowed controls differ.

## Consume metadata without confusing estimates and measurements

| Metadata field | Framework use |
| --- | --- |
| `schema`, `generator_version` | Check compatibility; retain executable/source hashes as well. |
| `transform_size`, `modulus`, `direction`, orders, `protocol` | Verify the emitted design matches the fixed task. The modulus is a JSON string, so use arbitrary-precision integer parsing. |
| `architecture`, `profile`, `reduction`, `streaming_width`, `radix` | Record resolved settings alongside requested options; architecture names may be more specific than the CLI backend name. |
| `latency`, `initiation_interval`, `input_cycles`, `output_cycles`, `minimum_gap` | Declared core cycle contract for screening. Independently simulate the generated RTL to establish the timing definition and sustained frame interval. |
| `architecture_parameters` | Backend-specific details such as `pe_count`, `pe_count_total`, `stage_groups`, banks and butterfly pipeline latency. Fields can be absent for other backends. |
| `full_throughput` | Descriptive backend metadata; do not use this Boolean alone as a throughput benchmark. |
| `dependencies`, `output_file` | Artifact bookkeeping. A plan's `<planned-output>` is not a retained RTL path. Preserve the actual emitted design and its metadata. |

Do not label declared cycles as achieved MHz or board throughput. A modeled
rate `clock_hz / measured_frame_interval` requires a qualified implementation
at that clock and a matching interface/workload definition. Physical timing,
resources, and constraints come from the target tools, not `plan`.

## Attach evaluation and a search policy

A useful trial lifecycle is:

1. Propose a candidate from the legal space, give it a stable identity, and save it.
2. Validate/lower with `plan`, then generate into its own directory. Retain failures.
3. Check RTL against an independent modular-transform or polynomial-product
   oracle, including reset, consecutive frames, and the actual handshake/stall
   contract. Use a protocol-specific driver; `next` and `ready-valid` differ.
4. Measure first-result latency and sustained product/frame interval separately.
   A forward/inverse round trip alone is not an independent correctness oracle.
5. Synthesize qualified RTL for comparable resource metrics. Fix tool versions,
   memory handling, pass sequence, and wrappers across candidates.
6. Promote selected designs to FPGA mapping/routing with fixed part, clock,
   constraints, and context. Record failed timing, not just tool exit success.
7. Update the feasible frontier, store evidence, and return acquired feedback to
   the search policy.

NGen's [verification scripts](../scripts/) provide transform-level examples;
see `test_streamed_ntt.py` for randomized simulation and protocols. They are
starting points for a testbench, not a universal product evaluator.

Keep a common evaluator for grid, random, and LLM search. An LLM can return a
structured choice of existing parameters without editing generated RTL. Show
it only configuration information and measurements already acquired under its
budget. Use the same seed evidence, failure accounting, promotion scheduler,
and logical candidate/mapping/routing budgets for every policy. Count cache
hits as acquisitions and report new physical work and wall time separately.

For a completed reference grid, compute nondominance only among qualified
points with the same workload/context. Report error, resource, latency, and
throughput objectives explicitly. A two-axis projection is not the whole
multi-objective frontier. To claim fewer trials, compare against an appropriate
reference and repeated matched-budget baselines; a small replay fixture only
checks the integration.

## Common integration mistakes

| Symptom | Likely cause / next step |
| --- | --- |
| Unexpected N or lane width | Passing actual values to logarithmic `-n`, `-k`, or `-r`. |
| Legal-looking candidate fails `plan` | Capability axes were crossed without their constraints; record the rejection and inspect stderr. |
| Stage-parallel candidate rejects a PE option | Remove `-pe`; that axis belongs to the reusable-PE family. Use the required next-pulse protocol. |
| A plan runs slowly | It performs actual lowering. Use timeouts, small initial cases, and cached validation results. |
| Emitted JSON differs from requested labels | Auto lowering/reduction or a backend-specific name resolved the request. Save requested and resolved settings. |
| A lower-interval candidate misses the clock | Cycle count and achievable frequency are different metrics; evaluate routed feasibility. |
| NTT passes but a polynomial product fails | Check range lifting, basis size, pointwise operations, scaling, twists, CRT centering, and output reduction. |
| Results change after a generator edit | Rebuild the assembly and invalidate source/executable-dependent caches. |
