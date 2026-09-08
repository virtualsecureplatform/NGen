module pipeline_reset_test;
reg clk=0;always #5 clk=~clk;
reg reset=0,load_a_f=0,load_a_i=0,load_b_f=0,load_b_i=0,read_a=0,read_b=0,start_ab=0,start_fntt=0,start_intt=0;
reg [11:0] din=0;wire [11:0] dout;wire done;
KyberHPM1PE dut(.clk(clk),.reset(reset),.load_a_f(load_a_f),.load_a_i(load_a_i),.load_b_f(load_b_f),.load_b_i(load_b_i),.read_a(read_a),.read_b(read_b),.start_ab(start_ab),.start_fntt(start_fntt),.start_intt(start_intt),.start_pwm2(1'b0),.din(din),.dout(dout),.done(done));
integer epoch,abort_cycle,cycles,index;reg finished;
task clear;
begin
 @(negedge clk);reset=1;start_fntt=0;start_intt=0;read_a=0;read_b=0;
 @(negedge clk);reset=0;
 repeat(16)begin @(negedge clk);if(done)$fatal(1,"stale completion after reset");end
end
endtask
task load_frame(input integer value,input integer bank);
begin
 @(negedge clk);load_a_f=!bank;load_b_f=bank;
 @(negedge clk);load_a_f=0;load_b_f=0;din=value;
 repeat(256)@(negedge clk);
 din=0;
end
endtask
task start_frame(input integer inverse,input integer bank);
begin
 @(negedge clk);start_ab=bank;start_fntt=!inverse;start_intt=inverse;
 @(negedge clk);start_fntt=0;start_intt=0;
end
endtask
initial begin
 clear;
 for(epoch=0;epoch<12;epoch=epoch+1)begin
  case(epoch)8:abort_cycle=127;9:abort_cycle=895;10:abort_cycle=900;11:abort_cycle=903;default:abort_cycle=epoch+1;endcase
  load_frame(1,epoch%2);start_frame((epoch/2)%2,epoch%2);
  repeat(abort_cycle)@(negedge clk);
  clear;
  load_frame(0,epoch%2);start_frame((epoch/2)%2,epoch%2);
  finished=0;
  for(cycles=0;cycles<1100 && !finished;cycles=cycles+1)begin @(negedge clk);finished=done;end
  if(!finished)$fatal(1,"no completion after restart at epoch %0d",epoch);
  @(negedge clk);read_a=!(epoch%2);read_b=epoch%2;
  @(negedge clk);read_a=0;read_b=0;
  repeat(260)begin @(negedge clk);if(dout!==12'd0)$fatal(1,"stale arithmetic after reset at epoch %0d",epoch);end
 end
 $display("PASS Kyber pipeline reset at fetch/read/arithmetic/retirement and stage boundaries");$finish;
end
initial begin #1000000;$fatal(1,"watchdog");end
endmodule
