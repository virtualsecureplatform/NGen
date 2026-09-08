#!/usr/bin/env bash
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d /tmp/ngen-switch-stream.XXXXXX)"
trap 'rm -rf "${run_dir}"' EXIT
cd "${repo_root}"
for bits in 1 2 3 4 5; do
  bash ngen.bat -n "$bits" -data-width 32 -top SwitchTransposeStream -o "${run_dir}/transpose.sv" switchtranspose
  iverilog -g2012 -s switch_transpose_stream_tb -P "switch_transpose_stream_tb.LOG_SIZE=$bits" \
    -o "${run_dir}/sim" "${run_dir}/transpose.sv" tests/rtl/switch_transpose_stream_tb.sv
  vvp "${run_dir}/sim"
done
