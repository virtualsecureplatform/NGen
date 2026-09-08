module half_test;
KyberHPM1PE dut();
integer value,expected;
initial begin
 for(value=0;value<3329;value=value+1)begin
  expected=(value*1665)%3329;
  if(dut.kyber_half(value)!==expected[11:0])$fatal(1,"half mismatch %d",value);
 end
 $display("PASS all 3329 residues: generated modular halving equals inverse-two multiplication");$finish;
end
endmodule
