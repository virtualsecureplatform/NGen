#!/usr/bin/env python3
"""Normalize NGen preset results against the checked-in LLM-NTT baselines.

The evaluator reports a wait interval separately from the input/output bursts.
This script compares the normalized transaction length
``input_cycles + max_wait_cycles + output_cycles`` and carries through the
available Yosys resource counters.  It deliberately does not mix Vivado and
Yosys numbers into one score.
"""

from __future__ import annotations

import argparse
import json
import subprocess
from pathlib import Path
from typing import Any


TASKS = {
    "small_yata8_raintt_p27": "small_yata8_raintt_p27.json",
    "small_yata8x8_raintt_p27": "small_yata8x8_raintt_p27.json",
    "yata_raintt_512_p27": "yata_raintt_512_p27.json",
    "small_hoge32_p64": "small_hoge32_p64.json",
    "hoge_streaming_intt_1024_p64": "hoge_streaming_intt_1024_p64.json",
    "hoge_streaming_ntt_1024_p64": "hoge_streaming_ntt_1024_p64.json",
}


def load(path: Path) -> dict[str, Any]:
    return json.loads(path.read_text(encoding="utf-8"))


def task_metrics(task: str, metrics: dict[str, Any]) -> dict[str, Any]:
    if "_raintt_" in task and not task.endswith(("_intt", "_ntt")):
        directions = {}
        direction_prefixes = [task]
        if task.startswith("yata_raintt_"):
            direction_prefixes.append("yata_raintt")
        for direction in ("intt", "ntt"):
            values = {}
            for prefix in direction_prefixes:
                values = task_metrics(f"{prefix}_{direction}", metrics)
                if values:
                    break
            if values:
                directions[direction] = values
        if directions:
            return {
                "directions": directions,
                "transaction_cycles": max(value["transaction_cycles"] for value in directions.values()),
            }
    prefix = task
    keys = {
        "input_cycles": f"{prefix}_input_cycles",
        "output_cycles": f"{prefix}_output_cycles",
        "max_wait_cycles": f"{prefix}_max_wait_cycles",
    }
    # HOGE's historical manifest omitted the task prefix from its keys.
    if task.startswith("hoge_streaming_"):
        direction = "intt" if "_intt_" in task else "ntt"
        keys = {
            "input_cycles": f"hoge_streaming_{direction}_input_cycles",
            "output_cycles": f"hoge_streaming_{direction}_output_cycles",
            "max_wait_cycles": f"hoge_streaming_{direction}_max_wait_cycles",
        }
    result: dict[str, Any] = {}
    for name, key in keys.items():
        if key in metrics:
            result[name] = int(metrics[key])
    if all(name in result for name in ("input_cycles", "output_cycles", "max_wait_cycles")):
        result["transaction_cycles"] = sum(result[name] for name in ("input_cycles", "output_cycles", "max_wait_cycles"))
    return result


def compare(task: str, reference: dict[str, Any], candidate: dict[str, Any]) -> dict[str, Any]:
    ref_metrics = task_metrics(task, reference.get("metrics", {}))
    cand_metrics = task_metrics(task, candidate.get("metrics", {}))
    result: dict[str, Any] = {
        "task": task,
        "correct": bool(candidate.get("correct")),
        "synthesis_passed": bool(candidate.get("synthesis_passed")),
        "reference": ref_metrics,
        "candidate": cand_metrics,
    }
    if "transaction_cycles" in ref_metrics and "transaction_cycles" in cand_metrics:
        result["transaction_ratio"] = cand_metrics["transaction_cycles"] / ref_metrics["transaction_cycles"]
    for name in ("yosys_num_cells", "yosys_num_wire_bits", "yosys_num_pub_wire_bits", "yosys_num_memories", "yosys_num_memory_bits"):
        if name in reference.get("metrics", {}):
            result.setdefault("reference_resources", {})[name] = reference["metrics"][name]
        if name in candidate.get("metrics", {}):
            result.setdefault("candidate_resources", {})[name] = candidate["metrics"][name]
    return result


def main() -> int:
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser()
    parser.add_argument("--task", action="append", choices=sorted(TASKS))
    parser.add_argument("--reference-root", type=Path, default=root.parent / "LLM-NTT-Examples")
    parser.add_argument("--candidate-root", type=Path, default=root / "build" / "preset-benchmarks")
    parser.add_argument("--run", action="store_true", help="generate and evaluate candidates before comparing")
    parser.add_argument("--with-yosys", action="store_true")
    parser.add_argument("--preset-backend", choices=("auto", "microcoded", "stage-parallel"), default="auto")
    parser.add_argument("--transpose", choices=("indexed", "switch"), default="indexed")
    args = parser.parse_args()
    tasks = args.task or list(TASKS)
    reference_dir = args.reference_root.resolve() / "baselines" / "extracted-rtl"
    candidate_root = args.candidate_root.resolve()
    candidate_root.mkdir(parents=True, exist_ok=True)
    comparisons = []
    for task in tasks:
        task_root = candidate_root / task
        if args.run:
            command = ["python3", str(root / "scripts" / "ngen_llm_ntt.py"), "--task", task,
                       "--llm-ntt-root", str(args.reference_root), "--transpose", args.transpose,
                       "--preset-backend", args.preset_backend,
                       "--output-dir", str(task_root)]
            if args.with_yosys:
                command.append("--with-yosys")
            subprocess.run(command, cwd=root, check=True)
        reference = load(reference_dir / TASKS[task])
        candidate_path = task_root / "eval" / "results.json"
        if not candidate_path.exists():
            candidate_path = task_root / "summary.json"
        if not candidate_path.exists():
            raise SystemExit(f"missing candidate result for {task}: {candidate_path}")
        candidate = load(candidate_path)
        comparisons.append(compare(task, reference, candidate))
    output = {"schema": "ngen-preset-comparison-v1", "comparisons": comparisons}
    output_path = candidate_root / "comparison.json"
    output_path.write_text(json.dumps(output, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print("task                                  reference  candidate  ratio")
    for item in comparisons:
        reference = item["reference"].get("transaction_cycles", "-")
        candidate = item["candidate"].get("transaction_cycles", "-")
        ratio = item.get("transaction_ratio", "-")
        ratio_text = f"{ratio:.3f}" if isinstance(ratio, float) else str(ratio)
        print(f"{item['task']:<38} {reference!s:>9} {candidate!s:>10} {ratio_text:>7}")
    print(f"wrote {output_path}")
    return 0 if all(item["correct"] for item in comparisons) else 1


if __name__ == "__main__":
    raise SystemExit(main())
