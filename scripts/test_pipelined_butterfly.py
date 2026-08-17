#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
Q = 17
WIDTH = Q.bit_length()
OPERATIONS = [
    (1, 3, 0, 5),
    (2, 7, 11, 9),
    (3, 4, 15, 13),
    (2, 16, 8, 3),
    (1, 12, 0, 7),
    (3, 9, 2, 6),
]


def expected(kind: int, a: int, b: int, constant: int) -> tuple[int, int]:
    if kind == 1:
        return a * constant % Q, 0
    if kind == 2:
        product = b * constant % Q
        return (a + product) % Q, (a - product) % Q
    return (a + b) % Q, (constant * (b - a)) % Q


def testbench(reduction: str) -> str:
    radix = 1 << WIDTH
    drives = []
    checks = []
    for tag, (kind, a, b, constant) in enumerate(OPERATIONS):
        encoded = constant * radix % Q if reduction == "montgomery" else constant
        precon = constant * radix // Q if reduction == "shoup" else 0
        drives.append(f"    @(negedge clock);valid_in=1;kind_in=2'd{kind};a_in={WIDTH}'d{a};b_in={WIDTH}'d{b};constant_in={WIDTH}'d{encoded};precon_in={WIDTH}'d{precon};tag_in=4'd{tag};")
        out0, out1 = expected(kind, a, b, constant)
        checks.append(f"      4'd{tag}:begin if(out0!=={WIDTH}'d{out0}||out1!=={WIDTH}'d{out1})$fatal(1,\"pipeline mismatch tag {tag}\");end")
    return f"""module test;
  reg clock=0,reset=1,valid_in=0;reg [1:0] kind_in;reg [{WIDTH-1}:0] a_in,b_in,constant_in,precon_in;reg [3:0] tag_in;
  wire valid_out;wire [{WIDTH-1}:0] out0,out1;wire [3:0] tag_out;integer received=0;
  always #5 clock=~clock;
  NGenPipelinedButterfly #(.TAG_WIDTH(4)) dut(clock,reset,valid_in,kind_in,a_in,b_in,constant_in,precon_in,tag_in,valid_out,out0,out1,tag_out);
  always @(posedge clock)begin #1;if(valid_out)begin case(tag_out){' '.join(checks)} default:$fatal(1,"bad tag");endcase received=received+1;end end
  initial begin
    repeat(2)@(posedge clock);@(negedge clock);reset=0;
{chr(10).join(drives)}
    @(negedge clock);valid_in=0;
    while(received<{len(OPERATIONS)})@(posedge clock);
    $display("PASS {reduction} pipelined butterfly");$finish;
  end
  initial begin repeat(100)@(posedge clock);$fatal(1,"timeout");end
endmodule
"""


def main() -> None:
    with tempfile.TemporaryDirectory(prefix="ngen-pipeline-test.") as directory:
        run_dir = Path(directory)
        for reduction in ("barrett", "montgomery", "shoup"):
            rtl = run_dir / f"{reduction}.sv"
            subprocess.run(["bash", str(ROOT / "ngen.bat"), "-q", str(Q), "-reduction", reduction,
                            "-o", str(rtl), "butterflypipeline"], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
            tb = run_dir / f"{reduction}_tb.sv"
            tb.write_text(testbench(reduction))
            sim = run_dir / reduction
            subprocess.run(["iverilog", "-g2012", "-s", "test", "-o", str(sim), str(rtl), str(tb)], check=True)
            subprocess.run(["vvp", str(sim)], check=True)


if __name__ == "__main__":
    main()
