#include <VHogeDistributedSwitchTranspose_64.h>
#include <verilated.h>

#include <cstdint>
#include <iostream>

namespace {
void tick(VHogeDistributedSwitchTranspose_64 &dut) {
    dut.clock = 0;
    dut.eval();
    dut.clock = 1;
    dut.eval();
    dut.clock = 0;
    dut.eval();
}

void set_lane(uint32_t *bus, int lane, uint64_t value) {
    bus[2 * lane] = static_cast<uint32_t>(value);
    bus[2 * lane + 1] = static_cast<uint32_t>(value >> 32);
}

uint64_t get_lane(const uint32_t *bus, int lane) {
    return static_cast<uint64_t>(bus[2 * lane]) | (static_cast<uint64_t>(bus[2 * lane + 1]) << 32);
}
}

int main(int argc, char **argv) {
    Verilated::commandArgs(argc, argv);
    VHogeDistributedSwitchTranspose_64 dut;
    dut.reset = 1;
    dut.valid_in = 0;
    tick(dut);
    tick(dut);
    dut.reset = 0;

    for (int cycle = 0; cycle < 32; ++cycle) {
        dut.valid_in = 1;
        for (int lane = 0; lane < 32; ++lane) set_lane(dut.data_in, lane, cycle * 32 + lane);
        tick(dut);
    }
    dut.valid_in = 0;
    int rows = 0;
    for (int cycle = 0; cycle < 100; ++cycle) {
        tick(dut);
        if (!dut.valid_out) continue;
        for (int lane = 0; lane < 32; ++lane) {
            const uint64_t expected = static_cast<uint64_t>(lane * 32 + rows);
            if (get_lane(dut.data_out, lane) != expected) {
                std::cerr << "transpose mismatch row=" << rows << " lane=" << lane
                          << " got=" << get_lane(dut.data_out, lane)
                          << " expected=" << expected << '\n';
                return 1;
            }
        }
        ++rows;
    }
    if (rows != 32) {
        std::cerr << "expected 32 output rows, got " << rows << '\n';
        return 1;
    }
    std::cout << "PASS distributed transpose\n";
    return 0;
}
