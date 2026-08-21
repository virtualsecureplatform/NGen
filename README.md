# NGen 0.2 development

NGen generates exact streaming Number Theoretic Transform hardware. Its
SGen-style command line lowers finite-field transform specifications through a
latency-aware RTL representation and emits synthesizable SystemVerilog,
metadata, and optional graphs.

Acyclic datapaths use `TimedGraph`; stateful streaming designs use the shared
`MicroProgram` IR, which preserves dependency order while packing independent
operations into the configured hardware width.

## Supported designs

| Preset | Configuration | Generated interface |
|---|---:|---|
| `yata8` | N=8, K=8, radix 8 | dual YATA RAINTT |
| `yata64` | N=64, K=8, radix 8 | dual YATA RAINTT |
| `yata512` | N=512, K=64, radix 8 | dual YATA RAINTT |
| `hoge32` | N=32, K=32, radix 32 | dual HOGE butterfly |
| `hoge1024` | N=1024, K=32, radix 32 | packed `NTTWrap`/`INTTWrap` |
| `kyber256` | N=256, one PE | Kyber PE1 load/start/read protocol |

Custom NTT-friendly primes support fully-parallel or banked streaming
generation at any power-of-two streaming width. Complete cyclic and
negacyclic transforms support fused radix 2, 4, or 8; parameterized incomplete
negacyclic transforms currently use radix 2.

## Build and use

Java 17 or newer is required.

```bash
sbt test assembly
./ngen.bat version
./ngen.bat presets

./ngen.bat -preset yata512 -k 6 -r 3 -profile baseline \
  -o YataRainttTop.v raintt

./ngen.bat -preset hoge1024 -k 5 -r 5 -o INTTWrap.v intt
./ngen.bat -preset hoge1024 -k 5 -r 5 -o NTTWrap.v ntt

./ngen.bat -preset kyber256 -k 0 -r 1 -o KyberHPM1PE.v kyberpe

./ngen.bat -n 3 -k 3 -r 1 -q 17 -root 9 \
  -graph -rtlgraph -o design.sv ntt

./ngen.bat -n 8 -k 3 -r 1 -q 3329 -root auto -base-case 2 \
  -architecture streamed -reduction montgomery -o incomplete.sv ntt

./ngen.bat -n 4 -k 2 -r 1 -q 12289 -root auto \
  -input-order natural -output-order bitreversed -o streamed.sv intt

./ngen.bat -n 8 -k 3 -r 2 -pe 2 -q 12289 -root auto \
  -architecture streamed -reduction shoup -o radix4.sv ntt

./ngen.bat -n 4 -k 4 -r 1 -q 12289 -root 4134 \
  -architecture stage-parallel -reduction montgomery -o staged.sv ntt

./ngen.bat -fermat 4 -n 5 -k 3 -r 1 \
  -architecture streamed -o fnt65537.sv ntt
```

Options precede the terminal transform. `-n`, `-k`, and `-r` are base-2
logarithms. Pipeline profiles are `baseline` and `f300`; the latter adds a
scheduling gap between microcoded bundles or uses the deeper generic graph
latencies. `f300` is a pipeline intent, not a vendor timing guarantee.

For custom domains, `-architecture auto|full-throughput|compact|fully-parallel|streamed|stage-parallel` controls the
lowering. `auto` preserves the fully-parallel v0.1 implementation when `K=N`
and otherwise selects the streamed state machine. Streamed designs expose
`ready`, accept `next` with the first input chunk, and can overlap a new first
chunk with the previous final output chunk. `-reduction barrett|montgomery|shoup`
selects constant modular multiplication. Shoup emits a precomputed reciprocal
beside each fixed twiddle and performs one final correction. `-root auto` discovers an exact-order
root; use `-root auto -psi auto` for complete negacyclic transforms.

The streamed backend uses `-pe <count>` reusable PEs, synchronous
conflict-free banks, packed per-PE address/twiddle control ROMs, and two coefficient
buffers. One buffer can capture while the other executes, and output can drain
one buffer while the next transform executes. The default PE count is
`max(1, K/2)`. Multipliers therefore scale with PE count rather than with the
number of scheduled butterflies.

`-architecture stage-parallel` is also available for arbitrary complete cyclic
or negacyclic domains. It emits one registered boundary per radix-2 NTT stage
and groups all butterflies in that stage behind the boundary. Barrett,
Montgomery, and Shoup reductions are supported, as are natural or bit-reversed
stream orders. This is a fully unrolled stage datapath, so it is a useful
throughput/timing baseline and a substrate for later banked or PE-limited
implementations; incomplete Kyber-style plans and Fermat-shift reduction remain
on the streamed backend.

The SGen-style names are `full-throughput` for an acyclic zero-gap pipeline and
`compact` for hardware-reusing/interleaved execution. Existing `streamed`,
`microcoded`, and `stage-parallel` names remain accepted. YATA presets implement
`full-throughput` as one native recursive radix-8 transaction pipeline,
sustaining one new eight-cycle dataset every eight cycles without replicating
complete engines. HOGE NTT/INTT use deeply pipelined
recursive radix-32 datapaths and streaming switch transposes; both accept a new
1024-point dataset every 32 cycles with no inter-dataset gap.

YATA full-throughput also accepts `-protocol ready-valid`. This adds elastic
stall control across capture, recursive stages, and output. The default
`-protocol next` source omits the ready ports and all stall/backpressure logic.
Twiddle tables have reusable inline, distributed-ROM, and banked block-ROM
lowerings selected deterministically from table size and required read ports.
Generic recursive plans may group radix-2 stages into radix 4, 8, or larger
power-of-two levels while preserving the executable reference semantics.

## Hardware-friendly prime forms

`primeinfo` classifies a field modulus as Goldilocks, Proth, pseudo-Mersenne,
sparse Solinas, generalized Fermat, or generic. It reports two-adicity,
cyclic/negacyclic transform limits, Montgomery-constant costs, lazy butterfly
headroom, and the recommended constant-multiplier lowering:

```bash
./ngen.bat -q 40960001 primeinfo
```

`primegen` finds a prime of the form `k*2^(n+1)+1`, preferring low signed-digit
weight in `k`, and prints a validated root and negacyclic twist:

```bash
./ngen.bat -n 10 -data-width 32 primegen
```

The arithmetic library includes executable generalized Proth SREDC and
pseudo-Mersenne folding specifications, signed Solinas decomposition,
Montgomery cost characterization, lazy-range tracking, and multi-prime RNS
generation to a requested dynamic range. Generalized Fermat fields now accept
non-power-of-two bases; those use Shoup lowering when digit-shift hardware is
not applicable.

Specialized arithmetic RTL can be emitted directly. Proth reducers use their
power-of-two radix and a bounded correction network; pseudo-Mersenne, Solinas,
and Goldilocks reducers use unrolled signed folding. The fused form uses
ordinary inferred multiplication by default. Experimental explicit 27x18
DSP tiling is enabled only with `-dsp-decompose`:

```bash
./ngen.bat -q 40960001 -o proth-reducer.sv primereducer
./ngen.bat -q 40960001 -o fused-butterfly.sv fusedbutterfly
./ngen.bat -q 40960001 -dsp-decompose -o fused-dsp.sv fusedbutterfly
```

For custom streamed and stage-parallel NTTs, `-reduction auto` now consumes the
prime analysis: sparse Solinas, pseudo-Mersenne, and Goldilocks forms use signed
folding, while Proth forms use the Montgomery/SREDC-compatible representation.
Generic primes retain the established fallback. The recursive HOGE pipeline
shares each twiddle product between both butterfly outputs, and native YATA
stages fuse twiddle multiplication, SREDC, and radix-8 butterfly operations.

Recursive arithmetic planning tracks redundant ranges and inserts correction
points only when the configured storage width would overflow. Generated design
metadata reports DSP tile count, partial-product adder depth, and the number of
lazy correction points.

For custom streaming transforms, `-interface axi4stream` wraps the ready/valid
core in a packed AXI4-Stream interface with `TDATA`, `TVALID`, `TREADY`, and
transaction `TLAST`. It also checks incoming `TLAST` boundaries. AXI4-Lite and
other bus wrappers are intentionally not supported. Without this option, NGen
emits the original raw interface and no AXI-related logic.

Radix-2 PEs use a three-stage tagged arithmetic pipeline for Barrett,
Montgomery, or Shoup multiplication. The same pipeline can be emitted as a
standalone one-operation-per-cycle component with:

```bash
./ngen.bat -q 12289 -reduction shoup -o butterfly-pipeline.sv butterflypipeline
```

`butterflypipeline -runtime-field` exposes runtime Barrett modulus and
reciprocal inputs; twiddle and precondition values are already supplied per
operation. The field inputs must remain stable until all tagged operations
drain.

Custom banked cores accept `-runtime-control` to expose synchronous writes to
their packed per-PE operation records. Metadata reports
`control_record_width`, `operation_bundles`, and `pe_count`; each record carries
the operation kind, bank rows, twiddle/precondition values, and fused-layer
constants. Configuration writes must occur while no transform is active.

The radix-2 transform controller issues one independent bundle per cycle,
tracks multiple tagged results in flight, permits simultaneous bank reads and
older-result writes, and drains the arithmetic pipeline only at transform-stage
boundaries. Radix-4/8 PEs register every internal butterfly layer with aligned
valid, kind, scaling, and twiddle controls.

`-protocol ready-valid` replaces the transaction-start interface with
per-chunk `in_valid/in_ready` and `out_valid/out_ready` handshakes. Input
capture and output draining may stall independently; output data and
`out_valid` remain stable under backpressure. The default remains
`-protocol next` for compatibility.

`-input-order` and `-output-order` accept `natural` or `bitreversed` for
complete transforms. `-base-case <size>` selects an incomplete negacyclic
transform that stops at polynomial blocks of that size. `-r 1`, `-r 2`, and
`-r 3` lower verified fused plans to reusable radix-2/4/8 PEs when the radix
logarithm divides `n`. Fused radix-4/8 PEs use constant-twiddle butterfly
networks with shared intermediate results, trading more multipliers
for fewer memory passes.
Their internal layer registers use lazy `[0,2q)` residues and canonicalize only
at twiddle multipliers and external outputs. Inverse scale and negacyclic
untwist factors are folded into final fused-output control records, avoiding a
separate coefficient-memory pass.

`-transpose indexed` preserves the v0.1 compiled-address implementation.
`-transpose switch` uses recursive HOGE `SwitchTransposeUnit` networks for
YATA inverse input/forward output, HOGE inverse input, and square custom
streams where `K` equals the number of stream cycles. Custom input and output
address plans are transformed with the physical networks, preserving natural
external order.
`-transpose distributed` is a HOGE forward-only mode. It decomposes the 32×32
transpose into four independently buffered 16×16 switch networks, reducing the
largest routing region at the cost of additional buffering and latency.
YATA supports switch transpose in both directions. HOGE currently supports it
for `intt` and `ntt`; the forward boundary switch uses corrected stream
indexing, while `distributed` provides the P&R-oriented decomposition.
The standalone primitive/network can be emitted with:

```bash
./ngen.bat -n 3 -data-width 32 -o transpose8.sv switchtranspose
./ngen.bat -n 2 -k 3 -data-width 32 -o transpose4x8.sv switchtranspose
```

The second form is rectangular: it accepts four cycles of eight elements and
emits eight cycles of four elements. Square shapes retain the recursive switch
network; rectangular shapes use an explicit width-changing tensor adapter.
The NTT boundary wrappers still require square streams because their core ports
have a fixed vector width.

Rectangular adapters use two tensor buffers and expose `input_ready`. A source
may start consecutive frames whenever this signal is high. When the transpose
narrows the vector (`input lanes > input cycles`), output bandwidth is lower
than input bandwidth, so `input_ready` eventually applies the unavoidable
backpressure; no finite buffer can sustain an unbounded input sequence at that
rate.

Pass `-fixed-rate` to omit `input_ready` and use a static schedule instead.
The required interval is emitted as `FRAME_INTERVAL`; for the 4×8 example it
is eight cycles, i.e. four capture cycles followed by a four-cycle frame gap.

Built-in YATA/HOGE presets select a conservative backend in `auto` mode:
YATA uses the stage-parallel lowering, while the large HOGE 1024-point preset
retains the compact microcoded reference until target-specific memory and
arithmetic modules are selected.  The experimental lowering can be requested
explicitly with `-preset-backend stage-parallel`; use `-preset-backend
microcoded` to reproduce the original schedule.

Every generation writes `<output-stem>.json`. `-graph` writes the transform
decomposition and `-rtlgraph` writes the scheduled architecture graph.

## LLM-NTT adapter

The standalone adapter maps characterized task IDs to NGen commands and the
sibling evaluator:

```bash
scripts/ngen_llm_ntt.py \
  --task small_yata8x8_raintt_p27 \
  --llm-ntt-root ../LLM-NTT-Examples \
  --transpose switch \
  --with-yosys
```

Supported task IDs cover all rows in the table above. The adapter does not
couple NGen to the LLM candidate-selection runner.

## Verification

- Scala models check modular arithmetic, roots, transform decompositions,
  Kyber's incomplete schedule, timed alignment, and code generation.
- `scripts/test_generated_rtl.sh` lint-compiles and simulates a generated custom
  NTT against a known vector.
- `scripts/test_switch_transpose.sh` verifies an 8-by-8 tagged stream through
  the recursive switch network, and the Yosys smoke suite includes a
  switch-backed YATA streaming wrapper.
- `scripts/test_streamed_ntt.py` generates and simulates randomized cyclic,
  negacyclic, and incomplete NTT/INTT designs across multiple `K`, stream
  orders, pipeline profiles, Barrett/Montgomery/Shoup reductions, and an overlapped
  back-to-back transaction.
- `scripts/test_pipelined_butterfly.py` drives consecutive tagged operations
  through all three modular-reduction pipelines.

The transform library also contains reference-checked composition groundwork
for complete-transform RNS polynomial multiplication with CRT reconstruction,
and general-size planning that classifies composite sizes for mixed-radix
lowering and prime sizes for Bluestein lowering.

Non-power-of-two transforms use `generalntt`. Composite sizes lower through
recursive mixed-radix Cooley–Tukey stages, with optional `-four-step` support:

- `-four-step-factor` can force the first factor (n1) in the `N = n1 × n2` split.

Prime sizes lower through Bluestein;
the caller supplies a power-of-two convolution root whose order equals the
next power of two covering `2*size-1`:

```bash
./ngen.bat -size 6 -q 13 -root 4 -o ntt6.sv generalntt
./ngen.bat -size 5 -q 241 -root 87 -convolution-root 44 \
  -o ntt5.sv generalntt
```

Classical Fermat Number Transforms use `-fermat 0` through `-fermat 4`.
NGen derives the modulus, transform root, and shift-only twiddle exponents.
Power-of-two-base generalized Fermat primes use `-fermat-base <a>
-fermat-index <m>` and the same banked radix-2/4/8 pipelines.

Complete-transform RNS polynomial multiplication is generated with matched
prime/root/twist vectors:

```bash
./ngen.bat -n 3 -rns-q 17,97 -rns-root 9,64 -rns-psi 3,8 \
  -rns-crt -o rns-polymul.sv rnspolymul
```

Each prime receives two forward transforms, pointwise multiplication, and an
inverse transform. Residue outputs are always emitted; `-rns-crt` adds direct
CRT reconstruction outputs.
- `LLM-NTT-Examples` provides TFHEpp/cuHEpp/Kyber vector oracles for every
  characterized preset, including a standalone HOGE forward-NTT oracle.
- The evaluator's `--with-yosys --yosys-candidate-only` path records flattened
  structural statistics for self-contained generated designs.

Preset throughput comparisons use the same transaction definition as the
reference manifests (input burst + maximum wait + output burst):

```bash
python3 scripts/benchmark_presets.py \
  --task small_yata8x8_raintt_p27 \
  --task hoge_streaming_intt_1024_p64 \
  --run --with-yosys
```

Pass `--preset-backend stage-parallel` to benchmark the experimental lowering
explicitly; `auto` is the default policy.

The resulting `comparison.json` keeps latency and Yosys resource counters
separate; Vivado/PPA numbers still require the target toolchain and constraints.

NGen is GPL-3.0 licensed. Generated designs do not copy checked-in reference
RTL; constants and schedules are derived from the declared transform domains.
