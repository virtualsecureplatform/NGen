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

./ngen.bat -fermat 4 -n 5 -k 3 -r 1 \
  -architecture streamed -o fnt65537.sv ntt
```

Options precede the terminal transform. `-n`, `-k`, and `-r` are base-2
logarithms. Pipeline profiles are `baseline` and `f300`; the latter adds a
scheduling gap between microcoded bundles or uses the deeper generic graph
latencies. `f300` is a pipeline intent, not a vendor timing guarantee.

For custom domains, `-architecture auto|fully-parallel|streamed` controls the
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

Radix-2 PEs use a three-stage tagged arithmetic pipeline for Barrett,
Montgomery, or Shoup multiplication. The same pipeline can be emitted as a
standalone one-operation-per-cycle component with:

```bash
./ngen.bat -q 12289 -reduction shoup -o butterfly-pipeline.sv butterflypipeline
```

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

`-transpose indexed` preserves the v0.1 compiled-address implementation.
`-transpose switch` uses recursive HOGE `SwitchTransposeUnit` networks for
YATA inverse input/forward output, HOGE inverse input, and square custom
streams where `K` equals the number of stream cycles. Custom input and output
address plans are transformed with the physical networks, preserving natural
external order.
YATA supports switch transpose in both directions. HOGE currently supports it
for `intt`; forward HOGE requires switches interleaved with the recursive
HomGate butterfly pipeline and is rejected instead of silently changing the
architecture.
The standalone primitive/network can be emitted with:

```bash
./ngen.bat -n 3 -data-width 32 -o transpose8.sv switchtranspose
```

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
lowering and prime sizes for Bluestein lowering. These general-size plans are
executable mathematical oracles; their RTL lowering is not yet exposed by the
CLI.

Non-power-of-two transforms use `generalntt`. Composite sizes lower through
recursive mixed-radix Cooley–Tukey stages. Prime sizes lower through Bluestein;
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

NGen is GPL-3.0 licensed. Generated designs do not copy checked-in reference
RTL; constants and schedules are derived from the declared transform domains.
