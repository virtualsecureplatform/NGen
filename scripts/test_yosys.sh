#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
run_dir="$(mktemp -d /tmp/ngen-yosys-test.XXXXXX)"
trap 'rm -rf "${run_dir}"' EXIT
cd "${repo_root}"

synthesize() {
  local file="$1"
  local top="$2"
  yosys -Q -p "read_verilog -sv ${file}; hierarchy -top ${top}; proc; opt; memory; opt; flatten; opt; stat" >/dev/null
}

bash ngen.bat -n 3 -k 3 -r 1 -q 17 -root 9 -o "${run_dir}/generic.sv" ntt
synthesize "${run_dir}/generic.sv" main

bash ngen.bat -n 4 -k 2 -r 1 -q 12289 -root 4134 -architecture streamed -reduction montgomery -o "${run_dir}/generic-streamed.sv" ntt
synthesize "${run_dir}/generic-streamed.sv" main

bash ngen.bat -n 4 -k 2 -r 1 -q 12289 -root 4134 -architecture streamed -reduction shoup -o "${run_dir}/generic-shoup.sv" ntt
synthesize "${run_dir}/generic-shoup.sv" main

bash ngen.bat -preset yata8 -k 3 -r 3 -o "${run_dir}/yata.sv" raintt
synthesize "${run_dir}/yata.sv" SmallYata8RainttP27Rtl

bash ngen.bat -preset yata64 -k 3 -r 3 -transpose switch -o "${run_dir}/yata-switch.sv" raintt
synthesize "${run_dir}/yata-switch.sv" SmallYata8x8RainttP27Rtl

bash ngen.bat -preset hoge32 -k 5 -r 5 -o "${run_dir}/hoge.sv" ntt
synthesize "${run_dir}/hoge.sv" SmallHoge32P64Rtl

bash ngen.bat -preset kyber256 -k 0 -r 1 -o "${run_dir}/kyber.sv" kyberpe
synthesize "${run_dir}/kyber.sv" KyberHPM1PE

echo "PASS Yosys smoke qualification"
