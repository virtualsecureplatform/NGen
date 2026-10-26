# Configurable NTT pipelines

NTT pipeline choices belong to NGen. Both the native CLI and the JSON DSE
adapter expose arithmetic partitioning and core output registration. An external
search framework selects these options; it does not insert registers into the
emitted RTL itself.

## Supported controls

| JSON field | Native CLI | Values | Meaning |
| --- | --- | --- | --- |
| `multiplier_pipeline_stages` | `-multiplier-pipeline-stages` | 1, 3 | Stages in the main twiddle-product multiplier: one direct product, or four partial products followed by two registered accumulation stages. |
| `reduction_pipeline_stages` | `-reduction-pipeline-stages` | 3, 4, 5 | Montgomery correction/reduction partition described below. |
| `output_register` | `-output-register` to enable | false, true | One elastic beat register at the NTT core output, holding validity and data under stalls. |

The initial supported scope is **custom complete radix-2 streamed/compact
Montgomery transforms, ready-valid protocol, raw interface, indexed transpose,
and no runtime control**. Forward/inverse transforms and buffered stage groups
are supported. Unsupported combinations are rejected by `plan` and generation;
they do not silently ignore requested stages. Barrett/Shoup, fused radix-4/8,
stage-parallel, presets, AXI/switch wrappers, and `rnspolymul` do not yet accept
these overrides.

These are actual arithmetic partitions, not an arbitrary latency target or
padding registers. The two multiplications by Montgomery constants are part of
the reduction pipeline, not controlled by the main-product multiplier setting.
The reduction choices are:

| Reduction stages | Arithmetic partition |
| --- | --- |
| 3 | Correction product; correction × modulus; combined carry addition and final conditional subtraction. |
| 4 | Correction product; correction × modulus; carry addition; final conditional subtraction. |
| 5 | Correction product; correction × modulus; low-word sum and registered upper operands; high-word sum with carry; final conditional subtraction. |

More stages change register cost, logic per stage, and transform-stage draining.
They do not guarantee improved routed frequency or end-to-end throughput.

## Defaults and latency

Omitting all three options preserves existing generated RTL. Defaults are:

- Main-product stages: 1 for field widths up to 32 bits, 3 for wider fields.
- Reduction stages: 4.
- Core output register: disabled.

The radix-2 butterfly latency is `2 + multiplier stages + reduction stages`:
one input/preparation stage, the selected arithmetic stages, and one butterfly
retirement stage. It is independent of the optional **core** output register.

| Main-product stages | Reduction stages | Butterfly latency |
| ---: | ---: | ---: |
| 1 | 3 | 6 cycles |
| 1 | 4 | 7 cycles |
| 1 | 5 | 8 cycles |
| 3 | 3 | 8 cycles |
| 3 | 4 | 9 cycles |
| 3 | 5 | 10 cycles |

An enabled core output register adds one cycle to the declared no-stall core
latency. It does not add one cycle per butterfly or per stage group. The register
can accept the next beat while its previous beat transfers; stalls are reflected
through the ready-valid handshake. It also allows the core to release its output
storage before that final buffered beat is accepted downstream. The reported
core initiation interval remains the underlying conservative declared contract,
not a measured interval after buffering.

NGen propagates butterfly latency into the PE schedule's declared execution
cost and stage draining; operands, operation kinds, transaction tags, and valid
signals are aligned through the selected arithmetic. Grouped engines receive
the same arithmetic choices, with output registration at the outer core boundary.

## Use the native CLI

```bash
bash ngen.bat plan -n 4 -k 2 -r 1 -q 97 -root auto -psi auto \
  -architecture streamed -pe 2 -reduction montgomery -protocol ready-valid \
  -multiplier-pipeline-stages 3 -reduction-pipeline-stages 5 \
  -output-register ntt

bash ngen.bat -n 4 -k 2 -r 1 -q 97 -root auto -psi auto \
  -architecture streamed -pe 2 -reduction montgomery -protocol ready-valid \
  -multiplier-pipeline-stages 3 -reduction-pipeline-stages 5 \
  -output-register -o build/pipelined-ntt.sv ntt
```

`butterflypipeline` also accepts the two arithmetic-stage options with explicit
Montgomery reduction, for standalone testing. It does not accept the core output
register or runtime-field option with pipeline overrides.

## Use the DSE JSON interface

The [example request](../examples/dse/ntt512-pipelined.json) selects N=512, L8,
PE4, main-product stages 3, reduction stages 5, and an elastic output register:

```bash
python3 scripts/ngen_dse.py plan --spec examples/dse/ntt512-pipelined.json
python3 scripts/ngen_dse.py generate --spec examples/dse/ntt512-pipelined.json \
  --output-dir build/my-pipelined-candidate
```

Sweep the three fields independently within their supported values. Request
normalization, candidate identity, and artifact recording include these fields.
`capabilities` advertises their values, constraints, and defaults under
`generic.pipeline_controls`.

Native `ngen-design-v1` metadata reports the resolved values under
`architecture_parameters`:

- `multiplier_pipeline_stages`
- `reduction_pipeline_stages`
- `output_register` (0 or 1)
- `butterfly_pipeline_latency`

The top-level `latency` and `initiation_interval` remain declared core contracts.
Measure generated RTL independently and qualify the target implementation before
using them as achieved performance. These controls are not the external
polynomial-product `pipelined` or `registered` integration profiles; those also
change pointwise/CRT logic and frame adapters outside the NTT core.

## Validation

After building the assembly, with Icarus Verilog on PATH:

```bash
sbt test assembly
python3 -m unittest discover -s tests/python
python3 scripts/test_pipelined_butterfly.py
python3 scripts/test_configurable_pipeline.py
```

The butterfly tests check independent modular arithmetic, consecutive tagged
operations, exact latency, bubbles, and reset across all arithmetic partitions
and fields up to 64 bits. Core tests check forward/inverse negacyclic transforms,
one/two stage groups, all arithmetic partitions, output registration, input
bubbles, output backpressure, reset during an active frame, and independently
measured latency changes versus declared metadata. Existing default emitters
remain in use for the original arithmetic configuration.
