package ngen.backend

import ngen.rtl.{TwiddleStorageKind,TwiddleStoragePlan}

/** Reusable constant-table lowering with explicit FPGA memory intent. */
object TwiddleStorageSystemVerilog:
  private def literal(value: BigInt, width: Int): String = s"${width}'h${value.toString(16)}"

  def emit(name: String, values: Vector[BigInt], width: Int, readPorts: Int): (TwiddleStoragePlan,String) =
    require(name.matches("[A-Za-z_][A-Za-z0-9_$]*") && values.nonEmpty && width > 0 && readPorts > 0)
    val plan=TwiddleStoragePlan.choose(values.size,width,readPorts)
    val addressWidth=math.max(1,32-Integer.numberOfLeadingZeros(values.size-1))
    val ports=Vector("input clock")++Vector.tabulate(readPorts)(i=>s"input [$addressWidth-1:0] address_$i")++Vector.tabulate(readPorts)(i=>s"output reg [$width-1:0] data_$i")
    val style=plan.kind match
      case TwiddleStorageKind.Inline => "logic"
      case TwiddleStorageKind.DistributedRom => "distributed"
      case TwiddleStorageKind.BlockRom => "block"
    val declarations=(0 until plan.replicatedCopies).map(copy=>s"(* rom_style=\"$style\" *) reg [${width-1}:0] rom_$copy [0:${values.size-1}];").mkString("\n  ")
    val initialization=values.zipWithIndex.map((v,i)=>s"rom_0[$i]=${literal(v,width)};").mkString(" ")+
      (1 until plan.replicatedCopies).flatMap(copy=>values.indices.map(i=>s"rom_$copy[$i]=rom_0[$i];")).mkString(" ")
    val reads=Vector.tabulate(readPorts){port=>
      val copy=port%plan.replicatedCopies
      if plan.kind==TwiddleStorageKind.Inline then s"always @(*) data_$port=rom_$copy[address_$port];"
      else s"always @(posedge clock) data_$port<=rom_$copy[address_$port];"
    }.mkString("\n  ")
    (plan,s"""module $name(${ports.mkString(",")});
              |  $declarations
              |  initial begin $initialization end
              |  $reads
              |endmodule
              |""".stripMargin)
