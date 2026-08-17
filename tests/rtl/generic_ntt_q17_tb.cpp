#include <Vmain.h>
#include <verilated.h>

#include <array>
#include <cstdint>
#include <iostream>

namespace {
void tick(Vmain &dut)
{
    dut.clock = 0;
    dut.eval();
    dut.clock = 1;
    dut.eval();
    dut.clock = 0;
    dut.eval();
}
}  // namespace

int main(int argc, char **argv)
{
    Verilated::commandArgs(argc, argv);
    Vmain dut;
    dut.reset = 1;
    dut.next = 0;
    tick(dut);
    tick(dut);
    dut.reset = 0;

    dut.i0 = 0;
    dut.i1 = 1;
    dut.i2 = 2;
    dut.i3 = 3;
    dut.i4 = 4;
    dut.i5 = 5;
    dut.i6 = 6;
    dut.i7 = 7;
    dut.next = 1;
    tick(dut);
    dut.next = 0;

    int watchdog = 0;
    while (!dut.next_out) {
        tick(dut);
        if (++watchdog > 200) {
            std::cerr << "next_out timeout\n";
            return 1;
        }
    }

    const std::array<uint32_t, 8> got = {
        dut.o0, dut.o1, dut.o2, dut.o3, dut.o4, dut.o5, dut.o6, dut.o7};
    const std::array<uint32_t, 8> expected = {11, 1, 12, 3, 13, 6, 14, 8};
    if (got != expected) {
        std::cerr << "q17 NTT mismatch:";
        for (uint32_t value : got) std::cerr << ' ' << value;
        std::cerr << '\n';
        return 1;
    }
    std::cout << "PASS generic_ntt_q17 latency=" << watchdog << '\n';
    dut.final();
    return 0;
}
