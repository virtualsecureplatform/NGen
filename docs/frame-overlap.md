# Stage-parallel frame overlap and full-throughput

Full-throughput is the maximum-overlap case of the spatial stage-parallel
architecture. It describes a frame initiation interval, not a requirement to
expose every coefficient on the interface in one cycle. At N=512 and L8,
back-to-back frames occupy 64 cycles each; a full-throughput core can start
another frame every 64 cycles even though previous frames remain in the pipeline.

## Select the mode

These custom-domain requests select the same implementation:

```bash
bash ngen.bat plan -n 9 -k 3 -r 1 -q 65537 -root auto -psi auto \
  -architecture full-throughput -protocol ready-valid -reduction montgomery ntt

bash ngen.bat plan -n 9 -k 3 -r 1 -q 65537 -root auto -psi auto \
  -architecture stage-parallel -frame-overlap \
  -protocol ready-valid -reduction montgomery ntt
```

For actual RTL generation, omit `plan` and add `-o build/core.sv` before `ntt`.
For the JSON interface, use [the example](../examples/dse/ntt512-overlapped.json):

```bash
python3 scripts/ngen_dse.py generate --spec examples/dse/ntt512-overlapped.json \
  --output-dir build/my-overlapped-core
```

The example uses `architecture: full-throughput` and `protocol: ready-valid`.
Alternatively set `architecture: stage-parallel` and `frame_overlap: true`.
These are aliases for one implementation. The JSON adapter canonicalizes the
ready-valid full-throughput alias to stage-parallel with `frame_overlap: true`
before assigning a candidate identity, so both forms have the same ID for the
same remaining settings. Explicitly disabling overlap with this alias is rejected.
Controllers using the raw native CLI should likewise deduplicate these forms.

The initial implementation supports custom complete cyclic/negacyclic radix-2
NTT/INTT, baseline profile, raw ready-valid interface, indexed transpose,
Barrett/Montgomery/Shoup arithmetic, and one stage group. Lane counts are
power-of-two divisors of N, including L1 and L=N. Do not pass a PE override:
each stage spatially instantiates all its butterflies. Runtime-control,
AXI/switch wrappers, sparse-fold reduction, configurable PE arithmetic pipelines,
and extra output-register options are rejected rather than ignored.

## What changes in the hardware

The historical `stage-parallel` controller loads a frame, updates shared work
registers one selected stage at a time, then drains that frame. Its parallel
arithmetic does not imply simultaneous processing of different frames.

With frame overlap enabled:

1. A capture vector assembles input beats and applies input factors/order.
2. Every radix-2 stage has its own full coefficient vector and frame-valid bit.
3. A frame advances when the next stage can accept it. Each stage can process a
   different frame during the same clock cycle.
4. The final stage vector is serialized into output beats, applying output
   factors/order and Montgomery conversion when selected.

Capture, stage execution, and output draining overlap. The final beat of one
output frame can transfer while the following frame replaces the final stage
vector. Similarly, the capture vector can transfer a completed frame while
accepting the first beat of its successor. Both operations use registered
frame boundaries; no complete NTT engines are replicated to hide frame gaps.

Output backpressure holds the current output beat and its validity stable.
Pressure propagates through the occupied frame slots; input readiness eventually
falls when they fill. Bubbles and frame gaps are supported. Reset flushes frame
validity and partial input/output counts; coefficient registers need not be reset.

The storage cost is one capture vector plus one vector per transform stage:
roughly `(log2(N)+1) * N * field_width` coefficient-register bits, before
control and arithmetic. This is a spatial, frame-buffered implementation,
not an SDF/MDC streaming network or a minimum-memory FFT/NTT architecture.

## Timing contract

With continuously available input and a ready output, let `T=N/L` and
`S=log2(N)`:

| Quantity | Contract |
| --- | --- |
| Input/output beats per frame | T |
| Sustained frame initiation interval | T cycles |
| Required inter-frame gap | 0 cycles |
| First accepted input beat to first accepted output beat | T + S cycles |
| First input beat to final output beat | T + S + T - 1 cycles |

The first-output latency assumes input beats are consecutive and the sink is
ready. Stalls increase elapsed time. `ngen-design-v1` reports the first-output
contract as `latency`, T as `initiation_interval`, zero `minimum_gap`, and
`full_throughput: true`. The resolved architecture name identifies overlapped
stage-parallel full-throughput. `architecture_parameters` records
`frame_overlap: 1`, `stage_count`, and `frame_register_slots`.

These are core transform contracts, not complete polynomial-product throughput
or a routed frequency claim. Each spatial butterfly still has combinational
modular arithmetic between stage registers. FPGA mapping/routing must establish
whether the desired clock is feasible. Different historical backends can use
different latency references; measure first and final output timing explicitly
when comparing archived metadata.

## Compatibility and verification

Omitting `-frame-overlap` retains the original next-pulse stage-parallel RTL.
Generic `full-throughput` with `next` retains its historical K=N acyclic path.
Preset YATA/HOGE implementations retain their existing behavior. Existing
published grid results are not replaced or relabeled as overlapped measurements.

After building the generator, run:

```bash
sbt test assembly
python3 -m unittest discover -s tests/python
python3 scripts/test_overlapped_stage_ntt.py
```

The independent RTL scoreboard covers cyclic/negacyclic transforms, both
directions, Barrett/Montgomery/Shoup, multiple widths through L=N, bit-reversed
ordering, fields up to 64 bits, and N=512 examples. Dense streams check every
input/output frame interval against N/L and verify first-output latency.
Long output stalls fill the frame pipeline; bubble/reset cases verify correct
resumption and flushing. Arithmetic expectations come from a separate Python
transform implementation.
