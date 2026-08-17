package ngen.backend

import ngen.arithmetic.YataField

/** SystemVerilog backend for the first fully-parallel YATA radix-8 design. */
object YataSystemVerilog:
  private def signedLiteral(width: Int, value: Long): String =
    if value < 0 then s"-$width'sd${-value}" else s"$width'sd$value"

  private def bothMod(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = yata_sword(work[$left]);",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = yata_sub_mod(temp, work[$right]);"
      )
    }.flatten

  private def addAdd(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = yata_sword(work[$left]);",
        s"work[$left] = yata_add_mod(work[$left], work[$right]);",
        s"work[$right] = temp - work[$right];"
      )
    }.flatten

  private def bothSredc(offset: Int, size: Int): Vector[String] =
    Vector.tabulate(size / 2) { index =>
      val left = offset + index
      val right = left + size / 2
      Vector(
        s"temp = work[$left];",
        s"work[$left] = yata_sredc(work[$left] + work[$right]);",
        s"work[$right] = yata_sredc(temp - work[$right]);"
      )
    }.flatten

  private def inverseRadix4(offset: Int): Vector[String] =
    addAdd(offset, 4) ++
      bothMod(offset, 2) ++
      Vector(s"work[${offset + 3}] = yata_const_twiddle_mul(work[${offset + 3}], 2, 1);") ++
      bothSredc(offset + 2, 2)

  private def inverseRadix8: Vector[String] =
    addAdd(0, 8) ++ inverseRadix4(0) ++ Vector(
      "work[6] = yata_const_twiddle_mul(work[6], 3, 2);",
      "temp = work[4];",
      "work[4] = work[4] + work[6];",
      "work[6] = temp - work[6];",
      "temp = yata_const_twiddle_mul(work[5], 3, 3);",
      "work[5] = yata_const_twiddle_mul(work[5], 3, 1) + yata_const_twiddle_mul(work[7], 3, 3);",
      "work[7] = temp + yata_const_twiddle_mul(work[7], 3, 1);"
    ) ++ bothSredc(4, 2) ++ bothSredc(6, 2)

  private def forwardRadix4(offset: Int): Vector[String] =
    bothMod(offset, 2) ++ bothMod(offset + 2, 2) ++ Vector(
      s"temp = work[$offset];",
      s"work[$offset] = yata_add_mod(work[$offset], work[${offset + 2}]);",
      s"work[${offset + 2}] = yata_sub_mod(temp, work[${offset + 2}]);",
      s"temp = work[${offset + 1}];",
      s"work[${offset + 3}] = -yata_const_twiddle_mul(work[${offset + 3}], 2, 1);",
      s"work[${offset + 1}] = work[${offset + 1}] + work[${offset + 3}];",
      s"work[${offset + 3}] = temp - work[${offset + 3}];"
    )

  private def forwardRadix8: Vector[String] =
    forwardRadix4(0) ++ bothMod(4, 2) ++ bothMod(6, 2) ++ Vector(
      "temp = work[4];",
      "work[4] = yata_add_mod(work[4], work[6]);",
      "work[6] = -yata_const_twiddle_mul(temp - work[6], 3, 2);",
      "temp = -yata_const_twiddle_mul(work[5], 3, 1);",
      "work[5] = -yata_const_twiddle_mul(work[5], 3, 3) - yata_const_twiddle_mul(work[7], 3, 1);",
      "work[7] = temp - yata_const_twiddle_mul(work[7], 3, 3);",
      "temp = yata_sword(work[0]);",
      "work[0] = yata_add_mod(work[0], work[4]);",
      "work[4] = yata_sub_mod(temp, work[4]);"
    ) ++ Vector.tabulate(3) { index =>
      val left = index + 1
      val right = left + 4
      Vector(
        s"temp = work[$left];",
        s"work[$left] = yata_sredc(work[$left] + work[$right]);",
        s"work[$right] = yata_sredc(temp - work[$right]);"
      )
    }.flatten

  private def lines(values: Seq[String], spaces: Int): String =
    val prefix = " " * spaces
    values.map(prefix + _).mkString("\n")

  private def ports(top: String): String =
    val declarations =
      Vector("input clock", "input reset", "input io_intt_validin") ++
        Vector.tabulate(8)(i => s"input [31:0] io_intt_in_$i") ++
        Vector.tabulate(8)(i => s"output reg [26:0] io_intt_out_$i") ++
        Vector("output reg io_intt_validout", "input io_ntt_validin") ++
        Vector.tabulate(8)(i => s"input [26:0] io_ntt_in_$i") ++
        Vector.tabulate(8)(i => s"output reg [31:0] io_ntt_out_$i") ++
        Vector("output reg io_ntt_validout")
    s"module $top(\n${declarations.map("  " + _).mkString(",\n")}\n);"

  def emitRadix8(top: String): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    val tables = YataField.tables(3)
    val inverseLoads = Vector.tabulate(8)(i =>
      s"work[$i] = yata_mul_sredc(intt_in_buf[$i][26:0], ${signedLiteral(27, tables.inttTwist(i))});"
    )
    val forwardLoads = Vector.tabulate(8)(i => s"work[$i] = $$signed(ntt_in_buf[$i]);")
    val inverseStores = Vector.tabulate(8)(i => s"intt_out_buf[$i] <= work[$i][26:0];")
    val forwardStores = Vector.tabulate(8) { i =>
      Vector(
        s"mulres = yata_mul_sredc(yata_sword(work[$i]), ${signedLiteral(27, tables.nttTwist(i))});",
        "posres = (mulres < 0) ? (mulres + YATA_P) : mulres;",
        "scaled = ((posres * YATA_MODSWITCH_SCALE) + 96'd33554432) >> 26;",
        s"ntt_out_buf[$i] <= scaled[31:0];"
      )
    }.flatten
    val inttOutputs = Vector.tabulate(8)(i => s"io_intt_out_$i <= intt_out_buf[$i];")
    val nttOutputs = Vector.tabulate(8)(i => s"io_ntt_out_$i <= ntt_out_buf[$i];")
    val inttInputs = Vector.tabulate(8)(i => s"intt_in_buf[$i] = io_intt_in_$i;")
    val nttInputs = Vector.tabulate(8)(i => s"ntt_in_buf[$i] = io_ntt_in_$i;")

    s"""// Generated by NGen from the YATA radix-8 transform plan.
       |// The SREDC datapath uses shifts, adds, and constant multiplies; no modulo operator is emitted.
       |
       |${ports(top)}
       |  localparam signed [53:0] YATA_P = 54'sd${YataField.Modulus};
       |  localparam signed [63:0] YATA_MODSWITCH_SCALE = 64'sd7036874245;
       |
       |  reg [31:0] intt_in_buf [0:7];
       |  reg signed [26:0] intt_out_buf [0:7];
       |  reg signed [26:0] ntt_in_buf [0:7];
       |  reg [31:0] ntt_out_buf [0:7];
       |  reg signed [53:0] work [0:7];
       |  reg intt_output_pending;
       |  reg ntt_output_pending;
       |  reg signed [53:0] temp;
       |  reg signed [26:0] mulres;
       |  reg [63:0] posres;
       |  reg [95:0] scaled;
       |  integer i;
       |
       |  function automatic signed [26:0] yata_sword(input signed [53:0] x);
       |    begin
       |      yata_sword = x[26:0];
       |    end
       |  endfunction
       |
       |  function automatic signed [26:0] yata_add_mod(input signed [53:0] a, input signed [53:0] b);
       |    reg signed [53:0] sum;
       |    begin
       |      sum = a + b;
       |      if (sum >= YATA_P) yata_add_mod = sum - YATA_P;
       |      else if (sum <= -YATA_P) yata_add_mod = sum + YATA_P;
       |      else yata_add_mod = sum;
       |    end
       |  endfunction
       |
       |  function automatic signed [26:0] yata_sub_mod(input signed [53:0] a, input signed [53:0] b);
       |    reg signed [53:0] difference;
       |    begin
       |      difference = a - b;
       |      if (difference >= YATA_P) yata_sub_mod = difference - YATA_P;
       |      else if (difference <= -YATA_P) yata_sub_mod = difference + YATA_P;
       |      else yata_sub_mod = difference;
       |    end
       |  endfunction
       |
       |  function automatic signed [26:0] yata_sredc(input signed [53:0] a);
       |    reg [26:0] a0;
       |    reg signed [26:0] a1;
       |    reg signed [26:0] m;
       |    reg signed [26:0] t1;
       |    reg signed [53:0] m_wide;
       |    reg signed [53:0] t1_wide;
       |    begin
       |      a0 = a[26:0];
       |      a1 = a[53:27];
       |      m_wide = -(({27'd0, a0} * 54'sd625) <<< 16) + {27'd0, a0};
       |      m = m_wide[26:0];
       |      t1_wide = (($$signed(m) * 54'sd625) <<< 16) + $$signed(m);
       |      t1 = t1_wide[53:27];
       |      yata_sredc = a1 - t1;
       |    end
       |  endfunction
       |
       |  function automatic signed [26:0] yata_mul_sredc(input signed [26:0] a, input signed [26:0] b);
       |    begin
       |      yata_mul_sredc = yata_sredc($$signed(a) * $$signed(b));
       |    end
       |  endfunction
       |
       |  function automatic signed [53:0] yata_const_twiddle_mul(input signed [53:0] a, input integer radixbit, input integer number);
       |    begin
       |      if (radixbit == 2 && number == 1) yata_const_twiddle_mul = (a * 25) <<< 8;
       |      else if (radixbit == 3 && number == 1) yata_const_twiddle_mul = (a * 5) <<< 4;
       |      else if (radixbit == 3 && number == 2) yata_const_twiddle_mul = (a * 25) <<< 8;
       |      else if (radixbit == 3 && number == 3) yata_const_twiddle_mul = (a * 125) <<< 12;
       |      else yata_const_twiddle_mul = a;
       |    end
       |  endfunction
       |
       |  task automatic compute_intt;
       |    begin
       |${lines(inverseLoads ++ inverseRadix8 ++ inverseStores, 6)}
       |    end
       |  endtask
       |
       |  task automatic compute_ntt;
       |    begin
       |${lines(forwardLoads ++ forwardRadix8 ++ forwardStores, 6)}
       |    end
       |  endtask
       |
       |  always @(posedge clock) begin
       |    if (reset) begin
       |      io_intt_validout <= 1'b0;
       |      io_ntt_validout <= 1'b0;
       |      intt_output_pending <= 1'b0;
       |      ntt_output_pending <= 1'b0;
       |      for (i = 0; i < 8; i = i + 1) begin
       |        intt_in_buf[i] <= 32'd0;
       |        intt_out_buf[i] <= 27'sd0;
       |        ntt_in_buf[i] <= 27'sd0;
       |        ntt_out_buf[i] <= 32'd0;
       |      end
       |    end else begin
       |      io_intt_validout <= intt_output_pending;
       |      io_ntt_validout <= ntt_output_pending;
       |${lines(inttOutputs, 6)}
       |${lines(nttOutputs, 6)}
       |      intt_output_pending <= io_intt_validin;
       |      ntt_output_pending <= io_ntt_validin;
       |      if (io_intt_validin) begin
       |${lines(inttInputs, 8)}
       |        compute_intt();
       |      end
       |      if (io_ntt_validin) begin
       |${lines(nttInputs, 8)}
       |        compute_ntt();
       |      end
       |    end
       |  end
       |endmodule
       |""".stripMargin
