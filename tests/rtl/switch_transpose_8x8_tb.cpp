#include <VSwitchTranspose8.h>
#include <verilated.h>

#include <cstdint>
#include <iostream>

namespace {
void tick(VSwitchTranspose8 &dut) { dut.clock=0;dut.eval();dut.clock=1;dut.eval();dut.clock=0;dut.eval(); }

bool sample(VSwitchTranspose8 &dut, int &output_cycle)
{
    if (!dut.valid_out) return true;
    if (output_cycle >= 8) return false;
    for (int lane=0; lane<8; ++lane) {
        const uint32_t expected=lane*8+output_cycle;
        if (dut.data_out[lane] != expected) {
            std::cerr << "transpose mismatch cycle=" << output_cycle << " lane=" << lane
                      << " got=" << dut.data_out[lane] << " expected=" << expected << '\n';
            return false;
        }
    }
    ++output_cycle;
    return true;
}
}

int main(int argc,char **argv)
{
    Verilated::commandArgs(argc,argv);
    VSwitchTranspose8 dut;
    dut.reset=1;dut.valid_in=0;tick(dut);tick(dut);dut.reset=0;
    int output_cycle=0;
    for(int cycle=0;cycle<128 && output_cycle<8;++cycle){
        dut.valid_in=cycle<8;
        const int input_cycle=cycle<8?cycle:7;
        for(int lane=0;lane<8;++lane)dut.data_in[lane]=input_cycle*8+lane;
        dut.clock=0;dut.eval();
        if(!sample(dut,output_cycle))return 1;
        dut.clock=1;dut.eval();dut.clock=0;dut.eval();
    }
    if(output_cycle!=8){std::cerr<<"transpose output timeout: "<<output_cycle<<" cycles\n";return 1;}
    std::cout<<"PASS switch_transpose_8x8\n";
    return 0;
}
