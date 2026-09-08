module switch_transpose_stream_tb;
  parameter LOG_SIZE = 3;
  localparam LANES = 1 << LOG_SIZE;
  localparam LATENCY = LANES - 1;
  localparam LIMIT = 4096;
  reg clock = 0, reset = 1, valid_in = 0;
  reg [LANES*32-1:0] data_in = 0;
  wire valid_out;
  wire [LANES*32-1:0] data_out;
  SwitchTransposeStream dut(clock, reset, valid_in, data_in, valid_out, data_out);
  integer frames [0:LIMIT-1];
  integer rows [0:LIMIT-1];
  integer cursor, frame_id, cycle, lane, gap, row, source, expected, pass;
  reg expected_valid;

  task add_frame;
    begin
      for (integer r = 0; r < LANES; r = r + 1) begin
        frames[cursor] = frame_id;
        rows[cursor] = r;
        cursor = cursor + 1;
      end
      frame_id = frame_id + 1;
    end
  endtask

  initial begin
    for (cycle = 0; cycle < LIMIT; cycle = cycle + 1) begin
      frames[cycle] = -1;
      rows[cycle] = 0;
    end
    cursor = 3;
    frame_id = 0;
    // Consecutive frames, every short gap, and gaps longer than a frame.
    for (gap = 0; gap <= LANES + 1; gap = gap + 1) begin
      add_frame;
      cursor = cursor + gap;
      add_frame;
      cursor = cursor + LANES + 2;
    end
    // Repeat after resetting an occupied pipeline.
    for (pass = 0; pass < 2; pass = pass + 1) begin
      reset = 1;
      #5; clock = 1; #5; clock = 0;
      reset = 0;
      for (cycle = 0; cycle < cursor + 2*LANES; cycle = cycle + 1) begin
        valid_in = frames[cycle] >= 0;
        for (lane = 0; lane < LANES; lane = lane + 1)
          data_in[lane*32 +: 32] = valid_in ? frames[cycle]*10000 + rows[cycle]*LANES + lane : 32'hdead0000 + cycle*LANES + lane;
        #5;
        source = cycle - LATENCY;
        expected_valid = 0;
        if (source >= 0) expected_valid = frames[source] >= 0;
        if (valid_out !== expected_valid)
          $fatal(1, "valid mismatch lanes=%0d cycle=%0d got=%b expected=%b", LANES, cycle, valid_out, expected_valid);
        if (expected_valid)
          for (lane = 0; lane < LANES; lane = lane + 1) begin
            expected = frames[source]*10000 + lane*LANES + rows[source];
            if (data_out[lane*32 +: 32] !== expected)
              $fatal(1, "data mismatch lanes=%0d cycle=%0d lane=%0d got=%0d expected=%0d", LANES, cycle, lane, data_out[lane*32 +: 32], expected);
          end
        clock = 1; #5; clock = 0;
      end
      valid_in = 1;
      for (row = 0; row < LANES/2; row = row + 1) begin
        #5; clock = 1; #5; clock = 0;
      end
    end
    $display("PASS switch transpose stream lanes=%0d", LANES);
    $finish;
  end
endmodule
