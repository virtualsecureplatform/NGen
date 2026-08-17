# NGen

NGen is an experimental generator for exact, streaming Number Theoretic
Transform hardware. It adopts SGen's separation between algebraic transforms,
architecture lowering, timed RTL, and backends, while making finite-field and
NTT conventions explicit.

The current `0.1.0-SNAPSHOT` foundation provides:

- exact `BigInt` arithmetic modulo an arbitrary prime;
- validated YATA, HOGE/Goldilocks, and CRYSTALS-Kyber field presets;
- cyclic, twist-based negacyclic, and seven-layer Kyber reference NTTs;
- a compositional transform IR with permutations, diagonals, radix-2 stages,
  and composition; and
- tests comparing the radix-2 decomposition with the direct NTT definition.

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
sbt "run presets"
```

The current prototype prints the validated generation plan. `-check` also runs
a mathematical round trip, including Kyber's incomplete transform schedule.

## Next milestone

The next milestone adds a timed RTL operator IR, modular add/subtract/multiply
operators, a SystemVerilog backend, and an `LLM-NTT-Examples` wrapper for the
`small_yata8_raintt_p27` task. No checked-in reference RTL will be used by that
generation path.
