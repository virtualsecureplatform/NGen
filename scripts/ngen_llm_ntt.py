#!/usr/bin/env python3
"""Generate and evaluate a characterized NGen LLM-NTT task."""

from __future__ import annotations

import argparse
import json
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
TASKS: dict[str, tuple[list[str], str]] = {
    "small_yata8_raintt_p27": (["-preset", "yata8", "-k", "3", "-r", "3", "raintt"], "SmallYata8RainttP27Rtl.sv"),
    "small_yata8x8_raintt_p27": (["-preset", "yata64", "-k", "3", "-r", "3", "raintt"], "SmallYata8x8RainttP27Rtl.sv"),
    "yata_raintt_512_p27": (["-preset", "yata512", "-k", "6", "-r", "3", "raintt"], "YataRainttTop.v"),
    "small_hoge32_p64": (["-preset", "hoge32", "-k", "5", "-r", "5", "ntt"], "SmallHoge32P64Rtl.sv"),
    "hoge_streaming_intt_1024_p64": (["-preset", "hoge1024", "-k", "5", "-r", "5", "intt"], "INTTWrap.v"),
    "hoge_streaming_ntt_1024_p64": (["-preset", "hoge1024", "-k", "5", "-r", "5", "ntt"], "NTTWrap.v"),
    "kyber_ntt_256_p12_pe1": (["-preset", "kyber256", "-k", "0", "-r", "1", "kyberpe"], "KyberHPM1PE.v"),
}
SWITCH_TRANSPOSE_TASKS = {
    "small_yata8_raintt_p27",
    "small_yata8x8_raintt_p27",
    "yata_raintt_512_p27",
    "hoge_streaming_intt_1024_p64",
}


def run(command: list[str], cwd: Path) -> None:
    print("+", " ".join(command), flush=True)
    subprocess.run(command, cwd=cwd, check=True)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--task", required=True, choices=sorted(TASKS))
    parser.add_argument("--llm-ntt-root", type=Path, default=ROOT.parent / "LLM-NTT-Examples")
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--profile", choices=("baseline", "f300"), default="baseline")
    parser.add_argument("--preset-backend", choices=("auto", "microcoded", "stage-parallel"), default="auto")
    parser.add_argument("--transpose", choices=("indexed", "switch"), default="indexed")
    parser.add_argument("--with-yosys", action="store_true")
    parser.add_argument("--ngen", type=Path, default=ROOT / "ngen.bat")
    args = parser.parse_args()
    if args.transpose == "switch" and args.task not in SWITCH_TRANSPOSE_TASKS:
        parser.error(f"task {args.task} does not expose a switch-transpose boundary")

    llm_root = args.llm_ntt_root.resolve()
    output_dir = (args.output_dir or ROOT / "build" / "llm-ntt" / args.task).resolve()
    candidate_dir = output_dir / "candidate"
    eval_dir = output_dir / "eval"
    candidate_dir.mkdir(parents=True, exist_ok=True)
    base, filename = TASKS[args.task]
    output = candidate_dir / filename

    generation_args = base[:-1] + ["-profile", args.profile, "-preset-backend", args.preset_backend,
                                    "-transpose", args.transpose, "-o", str(output), base[-1]]
    if args.ngen.exists():
        run(["bash", str(args.ngen), *generation_args], ROOT)
    else:
        run(["sbt", "--error", "run " + " ".join(generation_args)], ROOT)

    evaluator = llm_root / "scripts" / "evaluate_candidate.sh"
    command = [str(evaluator), "--task", args.task, "--verilog-dir", str(candidate_dir), "--build-dir", str(eval_dir)]
    if args.with_yosys:
        command.extend(("--with-yosys", "--yosys-candidate-only"))
    run(command, llm_root)

    result_path = eval_dir / "results.json"
    result = json.loads(result_path.read_text(encoding="utf-8"))
    summary = {
        "task": args.task,
        "profile": args.profile,
        "preset_backend": args.preset_backend,
        "transpose": args.transpose,
        "candidate": str(output),
        "results": str(result_path),
        "correct": bool(result.get("correct")),
        "synthesis_passed": bool(result.get("synthesis_passed")),
    }
    (output_dir / "summary.json").write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return 0 if summary["correct"] and (not args.with_yosys or summary["synthesis_passed"]) else 1


if __name__ == "__main__":
    sys.exit(main())
