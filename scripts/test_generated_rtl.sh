#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d /tmp/ngen-rtl-test.XXXXXX)"
trap 'rm -rf "${run_dir}"' EXIT

cd "${repo_root}"
sbt --error "run -n 3 -k 3 -r 1 -q 17 -root 9 -profile baseline -o ${run_dir}/design.sv ntt"
verilator --lint-only --Wall --top-module main "${run_dir}/design.sv"
verilator --cc --exe --build --top-module main \
  --Mdir "${run_dir}/obj" \
  "${run_dir}/design.sv" \
  "${repo_root}/tests/rtl/generic_ntt_q17_tb.cpp"
"${run_dir}/obj/Vmain"

sbt --error "run -n 4 -k 4 -r 1 -q 12289 -root 4134 -profile baseline -o ${run_dir}/design16.sv ntt"
verilator --lint-only --Wall --top-module main "${run_dir}/design16.sv"
verilator --cc --exe --build --top-module main \
  --Mdir "${run_dir}/obj16" \
  "${run_dir}/design16.sv" \
  "${repo_root}/tests/rtl/generic_ntt_q12289_tb.cpp"
"${run_dir}/obj16/Vmain"
