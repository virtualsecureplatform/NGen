# NGen

NGen is an experimental generator for exact, streaming Number Theoretic
Transform hardware. It adopts SGen's separation between algebraic transforms,
architecture lowering, timed RTL, and backends, while making finite-field and
NTT conventions explicit.

The current `0.1.0-SNAPSHOT` provides:

- exact `BigInt` arithmetic modulo an arbitrary prime;
- validated YATA, HOGE/Goldilocks, and CRYSTALS-Kyber field presets;
- cyclic, twist-based negacyclic, and seven-layer Kyber reference NTTs;
- a compositional transform IR with permutations, diagonals, radix-2 stages,
  and composition; and
- a synthesizable SystemVerilog backend for the fully-parallel YATA radix-8
  NTT/INTT benchmark wrapper; and
- a timed RTL signal graph that inserts explicit delay nodes to align operands
  according to configurable modular-operator latencies.

Kyber is deliberately represented as an incomplete negacyclic transform. Its
prime `3329` supports an order-256 root (`17`) but no order-512 root, so the
Kyber transform stops after seven layers and produces 128 polynomial pairs.
The field, root schedule, forward transform, and inverse transform are available
now. The Kyber PE memory/control protocol adapter follows the first generated
YATA RTL vertical slice.

## SGen-style command line

Like SGen, NGen accepts global design options followed by the transform name.
`-n`, `-k`, and `-r` are log2 values for transform size, streaming width, and
radix respectively.

```bash
sbt test
sbt "run -preset yata512 -k 6 -r 3 -check ntt"
sbt "run -preset hoge1024 -k 5 -r 5 intt"
sbt "run -preset kyber256 -k 0 -r 1 ntt"
sbt "run -n 3 -q 17 -root 9 -check ntt"
sbt "run -preset yata8 -k 3 -r 3 -check -o design.sv raintt"
sbt "run presets"
```

`ntt` and `intt` currently print the validated generation plan; `-check` also
runs a mathematical round trip, including Kyber's incomplete transform
schedule. The combined `raintt` transform emits the dual-interface YATA
radix-8 design required by `small_yata8_raintt_p27`.

The generated candidate can be evaluated in a sibling `LLM-NTT-Examples`
checkout:

```bash
candidate_dir="$(mktemp -d /tmp/ngen-yata8.XXXXXX)"
sbt "run -preset yata8 -k 3 -r 3 -check \
  -o $candidate_dir/SmallYata8RainttP27Rtl.sv raintt"

../LLM-NTT-Examples/scripts/evaluate_candidate.sh \
  --task small_yata8_raintt_p27 \
  --verilog-dir "$candidate_dir"
```

The LLM-NTT evaluator requires its pinned TFHEpp submodule and ignored Chisel
reference outputs because its CMake configuration elaborates all test targets.
From that repository, initialize submodules and run `sbt run` in the YATA and
HOGE Chisel directories before the first evaluation.

The generated radix-8 candidate passes all three TFHEpp-based test vectors with
one input cycle, one wait cycle, and one output cycle in each direction. Its
SREDC implementation deliberately emits no Verilog modulo operator.

## Next milestone

The timed graph already attaches configurable latency metadata to modular
operators and inserts explicit delay nodes on shorter butterfly paths. The next
milestone lowers the complete radix-8 plan through that graph, emits the delay
registers and valid path, and composes those blocks with a transpose buffer for
the 8-lane by 8-cycle task. No checked-in reference RTL is used by the NGen
generation path.
