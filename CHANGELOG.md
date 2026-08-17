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

## 0.1.0

- Added SGen-style NTT/INTT/RAINTT/Kyber PE generation.
- Added YATA, HOGE Goldilocks, Kyber, and generic Barrett arithmetic models.
- Added latency-aware graphs with baseline and f300 profiles.
- Added characterized YATA, HOGE, and Kyber hardware adapters.
- Added deterministic metadata and DOT graph artifacts.
- Added the universal `ngen.bat` launcher and LLM-NTT evaluation adapter.
