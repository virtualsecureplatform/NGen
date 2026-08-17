#!/usr/bin/env python3
"""Generate and simulate a matrix of generic streamed NTT designs."""

from __future__ import annotations

import json
import random
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


@dataclass(frozen=True)
class Domain:
    name: str
    size: int
    modulus: int
    root: int
    twist: int | None = None
    base_case: int | None = None


def bit_reverse(value: int, bits: int) -> int:
    result = 0
    for _ in range(bits):
        result = (result << 1) | (value & 1)
        value >>= 1
    return result


def cyclic(domain: Domain, values: list[int], inverse: bool) -> list[int]:
    q = domain.modulus
    root = pow(domain.root, -1, q) if inverse else domain.root
    result = [sum(value * pow(root, row * column, q) for column, value in enumerate(values)) % q
              for row in range(domain.size)]
    if inverse:
        scale = pow(domain.size, -1, q)
        result = [value * scale % q for value in result]
    return result


def transform(domain: Domain, values: list[int], inverse: bool) -> list[int]:
    q = domain.modulus
    if domain.base_case is not None:
        levels = (domain.size // domain.base_case).bit_length() - 1
        constants = [pow(domain.root, bit_reverse(index, levels), q) for index in range(domain.size // domain.base_case)]
        result = [value % q for value in values]
        if not inverse:
            constant_index = 1
            length = domain.size // 2
            while length >= domain.base_case:
                for start in range(0, domain.size, 2 * length):
                    zeta = constants[constant_index]
                    constant_index += 1
                    for index in range(start, start + length):
                        product = zeta * result[index + length] % q
                        even = result[index]
                        result[index] = (even + product) % q
                        result[index + length] = (even - product) % q
                length //= 2
        else:
            constant_index = len(constants) - 1
            length = domain.base_case
            while length <= domain.size // 2:
                for start in range(0, domain.size, 2 * length):
                    zeta = constants[constant_index]
                    constant_index -= 1
                    for index in range(start, start + length):
                        even, odd = result[index], result[index + length]
                        result[index] = (even + odd) % q
                        result[index + length] = zeta * (odd - even) % q
                length *= 2
            scale = pow(domain.size // domain.base_case, -1, q)
            result = [value * scale % q for value in result]
        return result
    if domain.twist is None:
        return cyclic(domain, values, inverse)
    if inverse:
        untwisted = cyclic(domain, values, True)
        inverse_twist = pow(domain.twist, -1, q)
        return [value * pow(inverse_twist, index, q) % q for index, value in enumerate(untwisted)]
    twisted = [value * pow(domain.twist, index, q) % q for index, value in enumerate(values)]
    return cyclic(domain, twisted, False)


def testbench(width: int, lanes: int, inputs: list[int], outputs: list[int]) -> str:
    cycles = len(inputs) // lanes
    declarations = "\n".join(f"  reg [{width - 1}:0] i{lane}; wire [{width - 1}:0] o{lane};" for lane in range(lanes))
    connections = ", ".join([".clock(clock)", ".reset(reset)", ".next(next)", ".ready(ready)", ".next_out(next_out)"] +
                            [f".i{lane}(i{lane})" for lane in range(lanes)] + [f".o{lane}(o{lane})" for lane in range(lanes)])
    drive = []
    for cycle in range(cycles):
        assignments = " ".join(f"i{lane} = {width}'d{inputs[cycle * lanes + lane]};" for lane in range(lanes))
        drive.append(f"    @(negedge clock); next = {1 if cycle == 0 else 0}; {assignments}\n    @(posedge clock); #1;")
    checks = []
    for cycle in range(cycles):
        if cycle > 0:
            checks.append("    @(posedge clock); #1;")
        for lane in range(lanes):
            expected = outputs[cycle * lanes + lane]
            checks.append(f"    if (o{lane} !== {width}'d{expected}) $fatal(1, \"cycle {cycle} lane {lane}: got %0d expected {expected} output_count=%0d\", o{lane}, dut.output_count);")
    return f"""module test;
  reg clock = 0, reset = 1, next = 0;
  wire ready, next_out;
{declarations}
  always #5 clock = ~clock;
  main dut({connections});
  initial begin repeat (10000) @(posedge clock); $fatal(1, "timeout"); end
  initial begin
    repeat (2) @(posedge clock);
    @(negedge clock); reset = 0;
{chr(10).join(drive)}
    next = 0;
    while (!next_out) begin @(posedge clock); #1; end
{chr(10).join(checks)}
    $display("PASS streamed NTT");
    $finish;
  end
endmodule
"""


def back_to_back_testbench(width: int, lanes: int, first_inputs: list[int], first_outputs: list[int],
                           second_inputs: list[int], second_outputs: list[int]) -> str:
    cycles = len(first_inputs) // lanes
    declarations = "\n".join(f"  reg [{width - 1}:0] i{lane}; wire [{width - 1}:0] o{lane};" for lane in range(lanes))
    connections = ", ".join([".clock(clock)", ".reset(reset)", ".next(next)", ".ready(ready)", ".next_out(next_out)"] +
                            [f".i{lane}(i{lane})" for lane in range(lanes)] + [f".o{lane}(o{lane})" for lane in range(lanes)])
    def drive(values: list[int], cycle: int, start: bool) -> str:
        assignments = " ".join(f"i{lane}={width}'d{values[cycle * lanes + lane]};" for lane in range(lanes))
        return f"next={1 if start else 0}; {assignments}"
    def checks(values: list[int], cycle: int) -> str:
        return " ".join(f"if(o{lane}!=={width}'d{values[cycle * lanes + lane]}) $fatal(1,\"back-to-back dataset output mismatch\");" for lane in range(lanes))
    first_drive = "\n".join(f"    @(negedge clock); {drive(first_inputs, cycle, cycle == 0)} @(posedge clock); #1;" for cycle in range(cycles))
    second_drive = "\n".join(f"    @(negedge clock); {drive(second_inputs, cycle, cycle == 0)} @(posedge clock); #1;" for cycle in range(cycles))
    first_check = []
    for cycle in range(cycles):
        if cycle > 0:
            first_check.append("    @(posedge clock); #1;")
        first_check.append(f"    {checks(first_outputs, cycle)}")
    second_check = []
    for cycle in range(cycles):
        if cycle > 0:
            second_check.append("    @(posedge clock); #1;")
        second_check.append(f"    {checks(second_outputs, cycle)}")
    return f"""module test;
  reg clock=0, reset=1, next=0; wire ready,next_out;
{declarations}
  always #5 clock=~clock;
  main dut({connections});
  initial begin repeat(10000) @(posedge clock); $fatal(1,"timeout"); end
  initial begin
    repeat(2) @(posedge clock); @(negedge clock); reset=0;
{first_drive}
    if(!ready) $fatal(1,"second ping-pong buffer was not ready during execution");
{second_drive}
    next=0; while(!next_out) begin @(posedge clock); #1; end
{chr(10).join(first_check)}
    while(!next_out) begin @(posedge clock); #1; end
{chr(10).join(second_check)}
    $display("PASS streamed NTT back-to-back"); $finish;
  end
endmodule
"""


def ready_valid_testbench(width: int, lanes: int, inputs: list[int], outputs: list[int]) -> str:
    cycles = len(inputs) // lanes
    declarations = "\n".join(f"  reg [{width - 1}:0] i{lane}; wire [{width - 1}:0] o{lane};" for lane in range(lanes))
    connections = ", ".join([".clock(clock)", ".reset(reset)", ".in_valid(in_valid)", ".in_ready(in_ready)",
                              ".out_valid(out_valid)", ".out_ready(out_ready)"] +
                             [f".i{lane}(i{lane})" for lane in range(lanes)] + [f".o{lane}(o{lane})" for lane in range(lanes)])
    drive = []
    for cycle in range(cycles):
        assignments = " ".join(f"i{lane}={width}'d{inputs[cycle * lanes + lane]};" for lane in range(lanes))
        drive.append(f"    @(negedge clock); in_valid=0; @(posedge clock); #1; while(!in_ready) begin @(posedge clock); #1; end @(negedge clock); {assignments} in_valid=1; @(posedge clock); #1;")
    checks = []
    for cycle in range(cycles):
        expected_checks = " ".join(f"if(o{lane}!=={width}'d{outputs[cycle * lanes + lane]}) $fatal(1,\"ready-valid output mismatch\");" for lane in range(lanes))
        checks.append(f"    while(!out_valid) begin @(posedge clock); #1; end {expected_checks}")
        checks.append(f"    repeat(2) begin @(posedge clock); #1; if(!out_valid) $fatal(1,\"out_valid dropped under backpressure\"); {expected_checks} end")
        checks.append("    @(negedge clock); out_ready=1; @(posedge clock); #1; @(negedge clock); out_ready=0;")
    return f"""module test;
  reg clock=0,reset=1,in_valid=0,out_ready=0;wire in_ready,out_valid;
{declarations}
  always #5 clock=~clock;
  main dut({connections});
  initial begin repeat(10000) @(posedge clock); $fatal(1,"timeout"); end
  initial begin
    repeat(2) @(posedge clock); @(negedge clock); reset=0;
{chr(10).join(drive)}
    @(negedge clock); in_valid=0;
{chr(10).join(checks)}
    $display("PASS streamed NTT ready-valid stalls");$finish;
  end
endmodule
"""


def run_case(run_dir: Path, domain: Domain, streaming_log: int, inverse: bool,
             input_order: str = "natural", output_order: str = "natural",
             profile: str = "baseline", architecture: str = "auto", reduction: str = "auto",
             radix_log: int = 1, pe_count: int | None = None, transpose: str = "indexed") -> None:
    log_size = domain.size.bit_length() - 1
    lanes = 1 << streaming_log
    rng = random.Random((domain.modulus << 8) ^ (streaming_log << 2) ^ int(inverse))
    natural_inputs = [rng.randrange(domain.modulus) for _ in range(domain.size)]
    natural_outputs = transform(domain, natural_inputs, inverse)
    rtl_inputs = natural_inputs if input_order == "natural" else [natural_inputs[bit_reverse(i, log_size)] for i in range(domain.size)]
    rtl_outputs = natural_outputs if output_order == "natural" else [natural_outputs[bit_reverse(i, log_size)] for i in range(domain.size)]

    stem = f"{domain.name}_{'intt' if inverse else 'ntt'}_k{streaming_log}_r{radix_log}_pe{pe_count}_{transpose}_{input_order}_{output_order}_{profile}_{architecture}_{reduction}"
    rtl = run_dir / f"{stem}.sv"
    args = ["bash", str(ROOT / "ngen.bat"), "-n", str(log_size), "-k", str(streaming_log), "-r", str(radix_log),
            "-q", str(domain.modulus), "-root", str(domain.root), "-input-order", input_order,
            "-output-order", output_order, "-profile", profile, "-architecture", architecture,
            "-reduction", reduction, "-transpose", transpose, "-o", str(rtl)]
    if pe_count is not None:
        args.extend(["-pe", str(pe_count)])
    if domain.twist is not None:
        args.extend(["-psi", str(domain.twist)])
    if domain.base_case is not None:
        args.extend(["-base-case", str(domain.base_case)])
    args.append("intt" if inverse else "ntt")
    subprocess.run(args, cwd=ROOT, check=True, stdout=subprocess.DEVNULL)

    metadata = json.loads(rtl.with_suffix(".json").read_text())
    assert metadata["streaming_width"] == lanes
    assert metadata["input_cycles"] == domain.size // lanes
    assert metadata["input_order"] == input_order
    assert metadata["output_order"] == output_order
    assert metadata["profile"] == profile
    parameters = metadata.get("architecture_parameters", {})
    if parameters:
        expected_pe = min(pe_count if pe_count is not None else max(1, lanes // 2), max(1, domain.size // (1 << radix_log)))
        assert parameters["pe_count"] == expected_pe
        assert parameters["radix"] == 1 << radix_log
        assert parameters["coefficient_buffers"] == 2

    tb = run_dir / f"{stem}_tb.sv"
    tb.write_text(testbench(domain.modulus.bit_length(), lanes, rtl_inputs, rtl_outputs))
    simulation = run_dir / stem
    subprocess.run(["iverilog", "-g2012", "-s", "test", "-o", str(simulation), str(rtl), str(tb)], check=True)
    subprocess.run(["vvp", str(simulation)], check=True)
    print(f"PASS {stem}")


def run_fermat_case(run_dir: Path, fermat_index: int, log_size: int, streaming_log: int, radix_log: int, inverse: bool) -> None:
    word_bits = 1 << fermat_index
    modulus = (1 << word_bits) + 1
    size = 1 << log_size
    root = pow(2, (2 * word_bits) // size, modulus)
    domain = Domain(f"fermat{fermat_index}", size, modulus, root)
    rng = random.Random(fermat_index * 1000 + log_size * 10 + int(inverse))
    values = [rng.randrange(modulus) for _ in range(size)]
    expected = transform(domain, values, inverse)
    lanes = 1 << streaming_log
    stem = f"fermat{fermat_index}_{'intt' if inverse else 'ntt'}_n{log_size}_k{streaming_log}_r{radix_log}"
    rtl = run_dir / f"{stem}.sv"
    subprocess.run(["bash", str(ROOT / "ngen.bat"), "-fermat", str(fermat_index), "-n", str(log_size),
                    "-k", str(streaming_log), "-r", str(radix_log), "-architecture", "streamed", "-o", str(rtl),
                    "intt" if inverse else "ntt"], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
    tb = run_dir / f"{stem}_tb.sv"
    tb.write_text(testbench(modulus.bit_length(), lanes, values, expected))
    simulation = run_dir / stem
    subprocess.run(["iverilog", "-g2012", "-s", "test", "-o", str(simulation), str(rtl), str(tb)], check=True)
    subprocess.run(["vvp", str(simulation)], check=True, stdout=subprocess.DEVNULL)
    print(f"PASS {stem}")


def main() -> None:
    domains = [
        Domain("cyclic8_q17", 8, 17, 9),
        Domain("negacyclic8_q97", 8, 97, 64, 8),
        Domain("incomplete8_q17", 8, 17, 9, base_case=2),
        Domain("cyclic16_q12289", 16, 12289, 4134),
    ]
    if not (ROOT / "ngen.bat").exists():
        subprocess.run(["sbt", "assembly"], cwd=ROOT, check=True)
    with tempfile.TemporaryDirectory(prefix="ngen-streamed-test.") as directory:
        run_dir = Path(directory)
        for domain in domains:
            for inverse in (False, True):
                for streaming_log in sorted({0, 1, domain.size.bit_length() - 2}):
                    run_case(run_dir, domain, streaming_log, inverse)
        run_case(run_dir, domains[0], 1, False, input_order="bitreversed")
        run_case(run_dir, domains[0], 1, False, output_order="bitreversed")
        run_case(run_dir, domains[0], 1, False, profile="f300")
        run_case(run_dir, domains[0], 3, False, architecture="streamed")
        run_case(run_dir, domains[0], 1, False, reduction="montgomery")
        run_case(run_dir, domains[1], 1, True, reduction="montgomery")
        run_case(run_dir, domains[0], 1, False, reduction="shoup")
        run_case(run_dir, domains[1], 1, True, reduction="shoup")
        run_case(run_dir, domains[2], 2, False, reduction="shoup")
        run_case(run_dir, domains[3], 2, True, reduction="shoup")
        run_case(run_dir, domains[0], 2, False, reduction="shoup", radix_log=3, pe_count=1)
        run_case(run_dir, domains[0], 2, True, reduction="montgomery", radix_log=3, pe_count=1)
        run_case(run_dir, domains[1], 2, False, reduction="barrett", radix_log=3, pe_count=1)
        run_case(run_dir, domains[3], 2, False, reduction="shoup", radix_log=2, pe_count=1)
        run_case(run_dir, domains[3], 2, True, reduction="montgomery", radix_log=2, pe_count=2)
        run_case(run_dir, domains[3], 2, False, reduction="shoup", radix_log=2, pe_count=1, transpose="switch")
        run_case(run_dir, domains[3], 2, True, reduction="montgomery", radix_log=2, pe_count=1, transpose="switch")
        for inverse in (False, True):
            run_fermat_case(run_dir, 4, 5, 3, 1, inverse)
            run_fermat_case(run_dir, 4, 4, 2, 2, inverse)
            run_fermat_case(run_dir, 4, 3, 2, 3, inverse)
        domain = domains[0]
        rng = random.Random(20260817)
        first = [rng.randrange(domain.modulus) for _ in range(domain.size)]
        second = [rng.randrange(domain.modulus) for _ in range(domain.size)]
        rtl = run_dir / "back_to_back.sv"
        subprocess.run(["bash", str(ROOT / "ngen.bat"), "-n", "3", "-k", "1", "-r", "1", "-q", "17", "-root", "9",
                        "-architecture", "streamed", "-o", str(rtl), "ntt"], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
        tb = run_dir / "back_to_back_tb.sv"
        tb.write_text(back_to_back_testbench(5, 2, first, transform(domain, first, False), second, transform(domain, second, False)))
        simulation = run_dir / "back_to_back"
        subprocess.run(["iverilog", "-g2012", "-s", "test", "-o", str(simulation), str(rtl), str(tb)], check=True)
        subprocess.run(["vvp", str(simulation)], check=True)
        rv_inputs = [rng.randrange(domain.modulus) for _ in range(domain.size)]
        rv_rtl = run_dir / "ready_valid.sv"
        subprocess.run(["bash", str(ROOT / "ngen.bat"), "-n", "3", "-k", "1", "-r", "1", "-pe", "1", "-q", "17", "-root", "9",
                        "-architecture", "streamed", "-reduction", "shoup", "-protocol", "ready-valid", "-o", str(rv_rtl), "ntt"], cwd=ROOT, check=True, stdout=subprocess.DEVNULL)
        rv_tb = run_dir / "ready_valid_tb.sv"
        rv_tb.write_text(ready_valid_testbench(5, 2, rv_inputs, transform(domain, rv_inputs, False)))
        rv_simulation = run_dir / "ready_valid"
        subprocess.run(["iverilog", "-g2012", "-s", "test", "-o", str(rv_simulation), str(rv_rtl), str(rv_tb)], check=True)
        subprocess.run(["vvp", str(rv_simulation)], check=True)
    print("PASS generic streamed NTT matrix")


if __name__ == "__main__":
    main()
