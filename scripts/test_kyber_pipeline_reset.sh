#!/usr/bin/env bash
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d /tmp/ngen-kyber-reset.XXXXXX)"
trap 'rm -rf "${run_dir}"' EXIT
cd "${repo_root}"
bash ngen.bat -preset-backend "${1:-microcoded}" -preset kyber256 -k 0 -r 1 -o "${run_dir}/KyberHPM1PE.v" kyberpe
iverilog -g2012 -s pipeline_reset_test -o "${run_dir}/simulation" "${run_dir}/KyberHPM1PE.v" tests/rtl/kyber_pipeline_reset_tb.sv
vvp "${run_dir}/simulation"
