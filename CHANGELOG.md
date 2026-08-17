# Changelog

## 0.2.0 (development)

- Added canonical complete and incomplete NTT plans with algebraically checked
  radix-fusion planning.
- Added generic streamed radix-2 RTL for arbitrary power-of-two `N` and `K`.
- Added natural/bit-reversed stream ordering, baseline/f300 scheduling, and a
  `ready` transaction contract.
- Added selectable Barrett, Montgomery, and preconditioned Shoup constant multiplication.
- Added automatic exact-order root discovery and configurable incomplete base
  cases beyond Kyber's fixed geometry.
- Added randomized generated-RTL and synthesis regression coverage.
- Added reusable PE counts, synchronous conflict-free memory banks, indexed
  address/twiddle control ROMs, and two-buffer overlap.
- Added generated fused radix-4 and radix-8 PEs for complete transforms.
- Packed each PE's operation addresses and twiddle data into one indexed ROM.
- Added optional per-chunk ready/valid input stalls and output backpressure.
- Added RNS polynomial-multiplication/CRT and general-size mixed-radix or
  Bluestein planning oracles.
- Integrated physical switch-transpose boundaries into square custom streams.
- Added tagged three-stage Barrett, Montgomery, and Shoup radix-2 butterfly
  pipelines and integrated them into the banked radix-2 PE backend.
- Added one-bundle-per-cycle radix-2 issue with tagged retirement and
  drain-before-next-stage barriers.
- Added valid-aligned registers between every fused radix-4/8 butterfly layer.
- Added classical F0-F4 and power-of-two-base generalized Fermat transform
  domains with shift/add twiddle pipelines.

## 0.1.0

- Added SGen-style NTT/INTT/RAINTT/Kyber PE generation.
- Added YATA, HOGE Goldilocks, Kyber, and generic Barrett arithmetic models.
- Added latency-aware graphs with baseline and f300 profiles.
- Added characterized YATA, HOGE, and Kyber hardware adapters.
- Added deterministic metadata and DOT graph artifacts.
- Added the universal `ngen.bat` launcher and LLM-NTT evaluation adapter.
