# NGen 0.1.0

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

Custom cyclic or negacyclic primes support fully-parallel radix-2 generation
with Barrett reduction (`-k` equal to `-n`, `-r 1`).

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
```

Options precede the terminal transform. `-n`, `-k`, and `-r` are base-2
logarithms. Pipeline profiles are `baseline` and `f300`; the latter adds a
scheduling gap between microcoded bundles or uses the deeper generic graph
latencies. `f300` is a pipeline intent, not a vendor timing guarantee.

Every generation writes `<output-stem>.json`. `-graph` writes the transform
decomposition and `-rtlgraph` writes the scheduled architecture graph.

## LLM-NTT adapter

The standalone adapter maps characterized task IDs to NGen commands and the
sibling evaluator:

```bash
scripts/ngen_llm_ntt.py \
  --task small_yata8x8_raintt_p27 \
  --llm-ntt-root ../LLM-NTT-Examples \
  --with-yosys
```

Supported task IDs cover all rows in the table above. The adapter does not
couple NGen to the LLM candidate-selection runner.

## Verification

- Scala models check modular arithmetic, roots, transform decompositions,
  Kyber's incomplete schedule, timed alignment, and code generation.
- `scripts/test_generated_rtl.sh` lint-compiles and simulates a generated custom
  NTT against a known vector.
- `LLM-NTT-Examples` provides TFHEpp/cuHEpp/Kyber vector oracles for every
  characterized preset, including a standalone HOGE forward-NTT oracle.
- The evaluator's `--with-yosys --yosys-candidate-only` path records flattened
  structural statistics for self-contained generated designs.

NGen is GPL-3.0 licensed. Generated designs do not copy checked-in reference
RTL; constants and schedules are derived from the declared transform domains.
