#include <Vmain.h>
#include <verilated.h>

#include <array>
#include <cstdint>
#include <iostream>

namespace {
void tick(Vmain &dut) { dut.clock = 0; dut.eval(); dut.clock = 1; dut.eval(); dut.clock = 0; dut.eval(); }
}

int main(int argc, char **argv)
{
    Verilated::commandArgs(argc, argv);
    Vmain dut;
    dut.reset = 1; dut.next = 0; tick(dut); tick(dut); dut.reset = 0;
    dut.i0=11;dut.i1=48;dut.i2=85;dut.i3=122;dut.i4=159;dut.i5=196;dut.i6=233;dut.i7=270;
    dut.i8=307;dut.i9=344;dut.i10=381;dut.i11=418;dut.i12=455;dut.i13=492;dut.i14=529;dut.i15=566;
    dut.next=1;tick(dut);dut.next=0;
    int watchdog=0;while(!dut.next_out){tick(dut);if(++watchdog>500)return 1;}
    const std::array<uint32_t,16> got={dut.o0,dut.o1,dut.o2,dut.o3,dut.o4,dut.o5,dut.o6,dut.o7,dut.o8,dut.o9,dut.o10,dut.o11,dut.o12,dut.o13,dut.o14,dut.o15};
    const std::array<uint32_t,16> expected={4616,4038,9639,7257,4324,5867,399,8746,11993,2951,11298,5830,7373,4440,2058,7659};
    if(got!=expected){std::cerr<<"q12289 NTT mismatch\n";return 1;}
    std::cout<<"PASS generic_ntt_q12289 latency="<<watchdog<<'\n';
    return 0;
}
