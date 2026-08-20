#!/usr/bin/env bash
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d /tmp/ngen-switch-test.XXXXXX)"
trap 'rm -rf "${run_dir}"' EXIT
cd "${repo_root}"
bash ngen.bat -n 3 -data-width 32 -top SwitchTranspose8 -o "${run_dir}/SwitchTranspose8.sv" switchtranspose
verilator --lint-only -Wno-fatal --top-module SwitchTranspose8 "${run_dir}/SwitchTranspose8.sv"
verilator --cc --exe --build --top-module SwitchTranspose8 --Mdir "${run_dir}/obj" \
  "${run_dir}/SwitchTranspose8.sv" "${repo_root}/tests/rtl/switch_transpose_8x8_tb.cpp"
"${run_dir}/obj/VSwitchTranspose8"

bash ngen.bat -preset hoge1024 -k 5 -r 5 -transpose distributed -o "${run_dir}/HogeDistributed.sv" ntt
verilator --lint-only -Wno-fatal --top-module HogeDistributedSwitchTranspose_64 "${run_dir}/HogeDistributed.sv"
verilator -Wno-fatal --cc --exe --build --top-module HogeDistributedSwitchTranspose_64 --Mdir "${run_dir}/obj-dist" \
  "${run_dir}/HogeDistributed.sv" "${repo_root}/tests/rtl/distributed_switch_64_tb.cpp"
"${run_dir}/obj-dist/VHogeDistributedSwitchTranspose_64"
