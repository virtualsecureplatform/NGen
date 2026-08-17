package ngen.backend

import ngen.arithmetic.{BarrettField, ShoupField}
import ngen.rtl.{PeNttSchedule, PeOperationKind, ProfileName, ReductionKind, StreamProtocol}

object PeStreamingNttSystemVerilog:
  final case class Metrics(
      inputCycles: Int,
      outputCycles: Int,
      bundleCount: Int,
      executionCycles: Int,
      latency: Int,
      initiationInterval: Int,
      bankCount: Int,
      bankDepth: Int,
      peCount: Int,
      radix: Int
  )

  def metrics(schedule: PeNttSchedule, streamingWidth: Int, profile: ProfileName): Metrics =
    val streamCycles = schedule.plan.domain.size / streamingWidth
    val gap = if profile == ProfileName.F300 then 1 else 0
    val executionCycles = 1 + 3 * schedule.bundles.size + math.max(0, schedule.bundles.size - 1) * gap
    val latency = streamCycles + executionCycles + 2
    // Conservative two-buffer bound; capture and output overlap execution whenever a buffer is available.
    val initiationInterval = math.max(streamCycles, executionCycles)
    Metrics(streamCycles, streamCycles, schedule.bundles.size, executionCycles, latency, initiationInterval,
      schedule.mapping.bankCount, schedule.mapping.depth, schedule.peCount, schedule.radix)

  def emit(
      schedule: PeNttSchedule,
      streamingWidth: Int,
      top: String,
      profile: ProfileName,
      reduction: ReductionKind,
      protocol: StreamProtocol = StreamProtocol.NextPulse
  ): String =
    require(top.matches("[A-Za-z_][A-Za-z0-9_$]*"), s"invalid SystemVerilog module name: $top")
    require(Set(ReductionKind.Barrett, ReductionKind.Montgomery, ReductionKind.Shoup)(reduction))
    val plan = schedule.plan
    val domain = plan.domain
    val field = domain.modulus
    val width = field.bitWidth
    val radix = schedule.radix
    val radixLog = schedule.radixLog
    val peCount = schedule.peCount
    val mapping = schedule.mapping
    val generatedMetrics = metrics(schedule, streamingWidth, profile)
    val streamCycles = generatedMetrics.inputCycles
    val gap = if profile == ProfileName.F300 then 1 else 0
    val barrett = BarrettField(field)
    val shoup = ShoupField(field)
    val wordRadix = BigInt(1) << width
    val montgomeryQInv = (-field.q.modInverse(wordRadix)).mod(wordRadix)

    def encoded(value: BigInt): BigInt =
      val normalized = field.normalize(value)
      if reduction == ReductionKind.Montgomery then field.multiply(normalized, wordRadix) else normalized
    def literal(value: BigInt): String = s"${width}'d${encoded(value)}"
    def precondition(value: BigInt): String = s"${width}'d${shoup.prepare(value).precondition}"
    def mulCall(value: String, constant: String, precon: String): String =
      if reduction == ReductionKind.Shoup then s"field_mul($value,$constant,$precon)" else s"field_mul($value,$constant)"

    val reductionParameter = reduction match
      case ReductionKind.Barrett => s"localparam [${2 * width - 1}:0] BARRETT_MU=${2 * width}'d${barrett.mu};"
      case ReductionKind.Montgomery => s"localparam [${width - 1}:0] MONTGOMERY_QINV=${width}'d$montgomeryQInv;"
      case ReductionKind.Shoup => "// Shoup reciprocal travels with each PE constant."
      case _ => throw new IllegalArgumentException("unsupported reduction")
    val multiplyFunction = reduction match
      case ReductionKind.Barrett =>
        s"""function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b);
           |  reg [${2 * width - 1}:0] product;reg [${4 * width - 1}:0] scaled;reg [${2 * width - 1}:0] quotient;reg [${3 * width - 1}:0] quotient_product;reg signed [${3 * width}:0] remainder;
           |  begin product={{$width{1'b0}},a}*{{$width{1'b0}},b};scaled={{${2 * width}{1'b0}},product}*{{${2 * width}{1'b0}},BARRETT_MU};quotient=scaled[${4 * width - 1}:${2 * width}];quotient_product={{$width{1'b0}},quotient}*{{${2 * width}{1'b0}},MODULUS};remainder=$$signed({${width + 1}'d0,product})-$$signed({1'b0,quotient_product});if(remainder<0)remainder=remainder+MODULUS_REMAINDER;if(remainder>=MODULUS_REMAINDER)remainder=remainder-MODULUS_REMAINDER;if(remainder>=MODULUS_REMAINDER)remainder=remainder-MODULUS_REMAINDER;field_mul=remainder[${width - 1}:0];end
           |endfunction""".stripMargin
      case ReductionKind.Montgomery =>
        s"""function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b);
           |  reg [${2 * width - 1}:0] product,correction_product,multiple;reg [${width - 1}:0] correction;reg [${2 * width}:0] sum;reg [$width:0] reduced;
           |  begin product={{$width{1'b0}},a}*{{$width{1'b0}},b};correction_product={{$width{1'b0}},product[${width - 1}:0]}*{{$width{1'b0}},MONTGOMERY_QINV};correction=correction_product[${width - 1}:0];multiple={{$width{1'b0}},correction}*{{$width{1'b0}},MODULUS};sum={1'b0,product}+{1'b0,multiple};reduced=sum[${2 * width}:$width];if(reduced>={1'b0,MODULUS})reduced=reduced-{1'b0,MODULUS};field_mul=reduced[${width - 1}:0];end
           |endfunction""".stripMargin
      case ReductionKind.Shoup =>
        s"""function automatic [${width - 1}:0] field_mul(input [${width - 1}:0] a,input [${width - 1}:0] b,input [${width - 1}:0] b_shoup);
           |  reg [${2 * width - 1}:0] product,approximate_product,quotient_product;reg [${width - 1}:0] approximate_quotient;reg [${2 * width}:0] remainder;
           |  begin product={{$width{1'b0}},a}*{{$width{1'b0}},b};approximate_product={{$width{1'b0}},a}*{{$width{1'b0}},b_shoup};approximate_quotient=approximate_product[${2 * width - 1}:$width];quotient_product={{$width{1'b0}},approximate_quotient}*{{$width{1'b0}},MODULUS};remainder={1'b0,product}-{1'b0,quotient_product};if(remainder>={{${width + 1}{1'b0}},MODULUS})remainder=remainder-{{${width + 1}{1'b0}},MODULUS};field_mul=remainder[${width - 1}:0];end
           |endfunction""".stripMargin
      case _ => throw new IllegalArgumentException("unsupported reduction")

    val protocolPorts = protocol match
      case StreamProtocol.NextPulse => Vector("input next", "output ready", "output reg next_out")
      case StreamProtocol.ReadyValid => Vector("input in_valid", "output in_ready", "output reg out_valid", "input out_ready")
    val ports = Vector("input clock", "input reset") ++ protocolPorts ++
      Vector.tabulate(streamingWidth)(lane => s"input [${width - 1}:0] i$lane") ++
      Vector.tabulate(streamingWidth)(lane => s"output reg [${width - 1}:0] o$lane")
    val rowWidth = math.max(1, 32 - Integer.numberOfLeadingZeros(mapping.depth - 1))
    def memoryName(buffer: Int, bank: Int): String = s"buffer_${buffer}_bank_$bank"
    def portName(buffer: Int, bank: Int, suffix: String): String = s"buffer_${buffer}_bank_${bank}_$suffix"
    val memories = (for buffer <- 0 until 2; bank <- 0 until mapping.bankCount yield
      s"(* ram_style = \"block\" *) reg [${width - 1}:0] ${memoryName(buffer,bank)} [0:BANK_DEPTH-1];reg ${portName(buffer,bank,"read_enable")},${portName(buffer,bank,"write_enable")};reg [${rowWidth - 1}:0] ${portName(buffer,bank,"read_address")},${portName(buffer,bank,"write_address")};reg [${width - 1}:0] ${portName(buffer,bank,"read_data")},${portName(buffer,bank,"write_data")};").mkString("\n  ")
    val memoryPorts = (for buffer <- 0 until 2; bank <- 0 until mapping.bankCount yield
      s"if(${portName(buffer,bank,"read_enable")})${portName(buffer,bank,"read_data")}<=${memoryName(buffer,bank)}[${portName(buffer,bank,"read_address")}];if(${portName(buffer,bank,"write_enable")})${memoryName(buffer,bank)}[${portName(buffer,bank,"write_address")}]<=${portName(buffer,bank,"write_data")};").mkString("\n      ")
    val portDefaults = (for buffer <- 0 until 2; bank <- 0 until mapping.bankCount yield
      s"${portName(buffer,bank,"read_enable")}=0;${portName(buffer,bank,"write_enable")}=0;${portName(buffer,bank,"read_address")}=0;${portName(buffer,bank,"write_address")}=0;${portName(buffer,bank,"write_data")}=0;").mkString(" ")
    def readPort(buffer: Int, address: Int): String =
      val bank = mapping.bank(address)
      s"${portName(buffer,bank,"read_enable")}=1;${portName(buffer,bank,"read_address")}=${rowWidth}'d${mapping.row(address)};"
    def writePort(buffer: Int, address: Int, value: String): String =
      val bank = mapping.bank(address)
      s"${portName(buffer,bank,"write_enable")}=1;${portName(buffer,bank,"write_address")}=${rowWidth}'d${mapping.row(address)};${portName(buffer,bank,"write_data")}=$value;"
    def selectedReadPort(address: Int): String = s"if(exec_buffer)begin ${readPort(1,address)} end else begin ${readPort(0,address)} end"
    def selectedReadData(target: String, address: Int): String =
      val bank = mapping.bank(address)
      s"if(exec_buffer)$target<=${portName(1,bank,"read_data")};else $target<=${portName(0,bank,"read_data")};"
    def selectedWritePort(address: Int, value: String): String = s"if(exec_buffer)begin ${writePort(1,address,value)} end else begin ${writePort(0,address,value)} end"

    def captureWrites(buffer: Int, cycle: Int): String = Vector.tabulate(streamingWidth) { lane =>
        val address = plan.inputAddresses(cycle * streamingWidth + lane)
        writePort(buffer,address,s"i$lane")
      }.mkString(" ")
    val captureCases = (0 until streamCycles).map { cycle =>
      val writes = s"if(capture_buffer)begin ${captureWrites(1,cycle)} end else begin ${captureWrites(0,cycle)} end"
      s"$cycle:begin $writes end"
    }.mkString("\n          ")
    def outputValues(buffer: Int, cycle: Int): String = Vector.tabulate(streamingWidth) { lane =>
      val address = plan.outputAddresses(cycle * streamingWidth + lane)
      s"o$lane<=${portName(buffer,mapping.bank(address),"read_data")};"
    }.mkString(" ")
    def outputReadPorts(buffer: Int, cycle: Int): String = Vector.tabulate(streamingWidth) { lane =>
      readPort(buffer,plan.outputAddresses(cycle * streamingWidth + lane))
    }.mkString(" ")
    val outputValueCases = (0 until streamCycles).map { cycle =>
      s"$cycle:begin if(output_buffer)begin ${outputValues(1,cycle)} end else begin ${outputValues(0,cycle)} end end"
    }.mkString("\n          ")
    val outputCurrentReadCases = (0 until streamCycles).map { cycle =>
      s"$cycle:begin if(output_buffer)begin ${outputReadPorts(1,cycle)} end else begin ${outputReadPorts(0,cycle)} end end"
    }.mkString("\n          ")
    val outputNextReadCases = (0 until math.max(0,streamCycles - 1)).map { cycle =>
      s"$cycle:begin if(output_buffer)begin ${outputReadPorts(1,cycle + 1)} end else begin ${outputReadPorts(0,cycle + 1)} end end"
    }.mkString("\n          ")

    val maxButterflySteps = radixLog * radix / 2
    val networkTemplate = schedule.bundles.flatMap(_.operations).find(_.kind == PeOperationKind.Dense).map(_.steps).getOrElse(Vector.empty)

    val peDeclarations =
      if radix == 2 then (0 until peCount).map { pe =>
        s"reg [1:0] pe_kind_$pe;reg [${width - 1}:0] pe_a_$pe,pe_b_$pe,pe_constant_$pe,pe_precon_$pe;wire [${width - 1}:0] pe_mul_input_$pe=(pe_kind_$pe==3)?mod_sub(pe_b_$pe,pe_a_$pe):((pe_kind_$pe==1)?pe_a_$pe:pe_b_$pe);wire [${width - 1}:0] pe_product_$pe=${mulCall(s"pe_mul_input_$pe",s"pe_constant_$pe",s"pe_precon_$pe")};wire [${width - 1}:0] pe_out_${pe}_0=(pe_kind_$pe==1)?pe_product_$pe:mod_add(pe_a_$pe,(pe_kind_$pe==2)?pe_product_$pe:pe_b_$pe);wire [${width - 1}:0] pe_out_${pe}_1=(pe_kind_$pe==2)?mod_sub(pe_a_$pe,pe_product_$pe):pe_product_$pe;"
      }.mkString("\n  ")
      else (0 until peCount).map { pe =>
        val inputs = (0 until radix).map(slot => s"reg [${width - 1}:0] pe_${pe}_in_$slot;").mkString
        val constants = s"reg [1:0] pe_kind_$pe;reg [${width - 1}:0] pe_scale_c_$pe,pe_scale_p_$pe;" +
          (0 until maxButterflySteps).map(step => s"reg [${width - 1}:0] pe_${pe}_step_c_$step,pe_${pe}_step_p_$step;").mkString
        var references = Vector.tabulate(radix)(slot => s"pe_${pe}_in_$slot")
        val network = networkTemplate.zipWithIndex.flatMap { case (step,index) =>
          val product = s"pe_${pe}_step_product_$index"
          val left = s"pe_${pe}_step_left_$index"
          val right = s"pe_${pe}_step_right_$index"
          val lines = Vector(
            s"wire [${width - 1}:0] $product=${mulCall(references(step.rightSlot),s"pe_${pe}_step_c_$index",s"pe_${pe}_step_p_$index")};",
            s"wire [${width - 1}:0] $left=mod_add(${references(step.leftSlot)},$product);",
            s"wire [${width - 1}:0] $right=mod_sub(${references(step.leftSlot)},$product);"
          )
          references = references.updated(step.leftSlot,left).updated(step.rightSlot,right)
          lines
        }.mkString
        val scaleProduct = s"wire [${width - 1}:0] pe_${pe}_scale_product=${mulCall(s"pe_${pe}_in_0",s"pe_scale_c_$pe",s"pe_scale_p_$pe")};"
        val outputs = (0 until radix).map { output =>
          s"wire [${width - 1}:0] pe_out_${pe}_$output=(pe_kind_$pe==1)?${if output == 0 then s"pe_${pe}_scale_product" else "0"}:${references(output)};"
        }.mkString
        inputs + constants + network + scaleProduct + outputs
      }.mkString("\n  ")

    def operationConstant(operation: ngen.rtl.PeOperation): BigInt = operation.kind match
      case PeOperationKind.Scale => operation.outputs.head.terms.head.coefficient
      case PeOperationKind.DecimationInTime => operation.outputs.head.terms.find(_.inputSlot == 1).get.coefficient
      case PeOperationKind.GentlemanSande => operation.outputs(1).terms.find(_.inputSlot == 1).get.coefficient
      case PeOperationKind.Dense => BigInt(0)
    val loadCases = schedule.bundles.zipWithIndex.map { case (bundle,index) =>
      val defaults =
        if radix == 2 then (bundle.operations.size until peCount).map(pe => s"pe_kind_$pe<=0;")
        else Vector.empty
      val loads = bundle.operations.zipWithIndex.flatMap { case (operation,pe) =>
        if radix == 2 then
          val constant = operationConstant(operation)
          Vector(
            s"pe_kind_$pe<=${operation.kind match
              case PeOperationKind.Scale => 1
              case PeOperationKind.DecimationInTime => 2
              case PeOperationKind.GentlemanSande => 3
              case _ => 0};",
            selectedReadData(s"pe_a_$pe",operation.inputs.head),
            if operation.inputs.size > 1 then selectedReadData(s"pe_b_$pe",operation.inputs(1)) else s"pe_b_$pe<=0;",
            s"pe_constant_$pe<=${literal(constant)};pe_precon_$pe<=${precondition(constant)};"
          )
        else
          val inputLoads = operation.inputs.zipWithIndex.map((address,slot) => selectedReadData(s"pe_${pe}_in_$slot",address)) ++
            (operation.inputs.size until radix).map(slot => s"pe_${pe}_in_$slot<=0;")
          val coefficientLoads = for
            output <- 0 until radix
            slot <- 0 until radix
          yield
            val coefficient = operation.outputs.lift(output).flatMap(_.terms.find(_.inputSlot == slot)).map(_.coefficient).getOrElse(BigInt(0))
            s"pe_${pe}_c_${output}_$slot<=${literal(coefficient)};pe_${pe}_p_${output}_$slot<=${precondition(coefficient)};"
          inputLoads ++ coefficientLoads
      }
      s"$index:begin ${(defaults++loads).mkString(" ")} end"
    }.mkString("\n          ")
    val readPortCases = schedule.bundles.zipWithIndex.map { case (bundle,index) =>
      val reads = bundle.operations.flatMap(_.inputs).map(selectedReadPort)
      s"$index:begin ${reads.mkString(" ")} end"
    }.mkString("\n          ")
    val writePortCases = schedule.bundles.zipWithIndex.map { case (bundle,index) =>
      val writes = bundle.operations.zipWithIndex.flatMap { case (operation,pe) =>
        operation.outputs.zipWithIndex.map((assignment,output) => selectedWritePort(assignment.address,s"pe_out_${pe}_$output"))
      }
      s"$index:begin ${writes.mkString(" ")} end"
    }.mkString("\n          ")

    val bankWidth = math.max(1, Integer.numberOfTrailingZeros(mapping.bankCount))
    final case class ControlField(name: String, bits: Int)
    val controlFields = Vector(ControlField("kind",2),ControlField("constant",width),ControlField("precon",width)) ++
      (0 until radix).flatMap(slot => Vector(ControlField(s"input_valid_$slot",1),ControlField(s"input_bank_$slot",bankWidth),ControlField(s"input_row_$slot",rowWidth))) ++
      (0 until radix).flatMap(output => Vector(ControlField(s"output_valid_$output",1),ControlField(s"output_bank_$output",bankWidth),ControlField(s"output_row_$output",rowWidth))) ++
      (if radix == 2 then Vector.empty else (0 until maxButterflySteps).flatMap(step => Vector(ControlField(s"step_constant_$step",width),ControlField(s"step_precon_$step",width))))
    val controlOffsets = controlFields.scanLeft(0)((offset,field) => offset + field.bits).dropRight(1).zip(controlFields).map { case (offset,field) => field.name -> offset }.toMap
    val controlWidth = controlFields.map(_.bits).sum
    def controlValue(operation: Option[ngen.rtl.PeOperation], fieldName: String): BigInt = fieldName match
      case "kind" => operation.map(_.kind match
        case PeOperationKind.Scale => 1
        case PeOperationKind.DecimationInTime | PeOperationKind.Dense => 2
        case PeOperationKind.GentlemanSande => 3).getOrElse(0)
      case "constant" => encoded(operation.map(operationConstant).getOrElse(BigInt(0)))
      case "precon" => shoup.prepare(operation.map(operationConstant).getOrElse(BigInt(0))).precondition
      case name if name.startsWith("input_") =>
        val parts = name.split("_")
        val property = parts(1)
        val slot = parts(2).toInt
        val address = operation.flatMap(_.inputs.lift(slot))
        property match
          case "valid" => if address.isDefined then 1 else 0
          case "bank" => address.map(mapping.bank).getOrElse(0)
          case "row" => address.map(mapping.row).getOrElse(0)
      case name if name.startsWith("output_") =>
        val parts = name.split("_")
        val property = parts(1)
        val output = parts(2).toInt
        val assignment = operation.flatMap(_.outputs.lift(output))
        property match
          case "valid" => if assignment.isDefined then 1 else 0
          case "bank" => assignment.map(value => mapping.bank(value.address)).getOrElse(0)
          case "row" => assignment.map(value => mapping.row(value.address)).getOrElse(0)
      case name if name.startsWith("step_") =>
        val parts = name.split("_")
        val property = parts(1)
        val step = parts(2).toInt
        val twiddle = operation.flatMap(_.steps.lift(step)).map(_.twiddle).getOrElse(BigInt(0))
        if property == "constant" then encoded(twiddle) else shoup.prepare(twiddle).precondition
      case other => throw new IllegalArgumentException(s"unknown packed control field $other")
    def packedControl(operation: Option[ngen.rtl.PeOperation]): BigInt = controlFields.foldLeft(BigInt(0)) { (record,field) =>
      record | (controlValue(operation,field.name) << controlOffsets(field.name))
    }
    def alias(pe: Int, field: ControlField): String =
      val range = if field.bits == 1 then s"[${controlOffsets(field.name)}]" else s"[${controlOffsets(field.name)} +: ${field.bits}]"
      val declaration = if field.bits == 1 then "wire" else s"wire [${field.bits - 1}:0]"
      val signalName = field.name match
        case "kind" => s"op_kind_$pe"
        case "constant" => s"op_constant_$pe"
        case "precon" => s"op_precon_$pe"
        case name if name.startsWith("input_") =>
          val parts = name.split("_"); s"input_${parts(1)}_${pe}_${parts(2)}"
        case name if name.startsWith("output_") =>
          val parts = name.split("_"); s"output_${parts(1)}_${pe}_${parts(2)}"
        case name if name.startsWith("step_") =>
          val parts = name.split("_"); s"step_${parts(1)}_${pe}_${parts(2)}"
      s"$declaration $signalName=control_$pe$range;"
    val romDeclarations = (0 until peCount).map { pe =>
      s"(* rom_style = \"distributed\" *) reg [${controlWidth - 1}:0] control_${pe}_rom[0:BUNDLE_COUNT-1];wire [${controlWidth - 1}:0] control_$pe=control_${pe}_rom[bundle_index];${controlFields.map(alias(pe,_)).mkString}"
    }.mkString("\n  ")
    val romInitializers = schedule.bundles.zipWithIndex.flatMap { case (bundle,bundleIndex) =>
      (0 until peCount).map { pe =>
        val record = packedControl(bundle.operations.lift(pe))
        s"control_${pe}_rom[$bundleIndex]=${controlWidth}'h${record.toString(16)};"
      }
    }.mkString("\n    ")
    def bankCase(bankSignal: String)(body: Int => String): String =
      (0 until mapping.bankCount).map(bank => s"${bankWidth}'d$bank:begin ${body(bank)} end").mkString(s"case($bankSignal)"," "," default:begin end endcase")
    val dynamicReadPorts = (for pe <- 0 until peCount; slot <- 0 until radix yield
      s"if(input_valid_${pe}_${slot})begin ${bankCase(s"input_bank_${pe}_${slot}") { bank =>
        s"if(exec_buffer)begin ${portName(1,bank,"read_enable")}=1;${portName(1,bank,"read_address")}=input_row_${pe}_${slot};end else begin ${portName(0,bank,"read_enable")}=1;${portName(0,bank,"read_address")}=input_row_${pe}_${slot};end"
      }} end").mkString(" ")
    val dynamicLoads = (0 until peCount).flatMap { pe =>
      val inputLoads = (0 until radix).map { slot =>
        val target = if radix == 2 then (if slot == 0 then s"pe_a_$pe" else s"pe_b_$pe") else s"pe_${pe}_in_$slot"
        s"if(input_valid_${pe}_${slot})begin ${bankCase(s"input_bank_${pe}_${slot}") { bank =>
          s"if(exec_buffer)$target<=${portName(1,bank,"read_data")};else $target<=${portName(0,bank,"read_data")};"
        }} end else $target<=0;"
      }
      val controls =
        if radix == 2 then Vector(s"pe_kind_$pe<=op_kind_$pe;pe_constant_$pe<=op_constant_$pe;pe_precon_$pe<=op_precon_$pe;")
        else Vector(s"pe_kind_$pe<=op_kind_$pe;pe_scale_c_$pe<=op_constant_$pe;pe_scale_p_$pe<=op_precon_$pe;") ++
          (0 until maxButterflySteps).map(step => s"pe_${pe}_step_c_$step<=step_constant_${pe}_$step;pe_${pe}_step_p_$step<=step_precon_${pe}_$step;")
      inputLoads ++ controls
    }.mkString(" ")
    val dynamicWritePorts = (for pe <- 0 until peCount; output <- 0 until radix yield
      s"if(output_valid_${pe}_${output})begin ${bankCase(s"output_bank_${pe}_${output}") { bank =>
        s"if(exec_buffer)begin ${portName(1,bank,"write_enable")}=1;${portName(1,bank,"write_address")}=output_row_${pe}_${output};${portName(1,bank,"write_data")}=pe_out_${pe}_$output;end else begin ${portName(0,bank,"write_enable")}=1;${portName(0,bank,"write_address")}=output_row_${pe}_${output};${portName(0,bank,"write_data")}=pe_out_${pe}_$output;end"
      }} end").mkString(" ")

    val resetOutputs = Vector.tabulate(streamingWidth)(lane => s"o$lane<=0;").mkString
    val emptyBufferReady = "(buffer_0_state==EMPTY)||(buffer_1_state==EMPTY)"
    val readyAssignment = protocol match
      case StreamProtocol.NextPulse => s"assign ready=!capture_active&&($emptyBufferReady);"
      case StreamProtocol.ReadyValid => s"assign in_ready=capture_active?1'b1:($emptyBufferReady);"
    val firstInputAccepted = if protocol == StreamProtocol.NextPulse then "next&&ready" else "in_valid&&in_ready"
    val continuingInputAccepted = if protocol == StreamProtocol.NextPulse then "1'b1" else "in_valid"
    val resetProtocol = if protocol == StreamProtocol.NextPulse then "next_out<=0;" else "out_valid<=0;"
    val defaultProtocol = if protocol == StreamProtocol.NextPulse then "next_out<=0;" else ""
    val outputReadGuard = if protocol == StreamProtocol.NextPulse then "" else "&&!out_valid"
    val outputAdvance = protocol match
      case StreamProtocol.NextPulse =>
        s"""case(output_count)$outputValueCases default:begin end endcase
           |          if(output_count==0)next_out<=1;
           |          if(output_count==STREAM_CYCLES-1)begin output_active<=0;output_prefetched<=0;output_count<=0;if(output_buffer)buffer_1_state<=EMPTY;else buffer_0_state<=EMPTY;end else output_count<=output_count+1;""".stripMargin
      case StreamProtocol.ReadyValid =>
        s"""if(!out_valid)begin case(output_count)$outputValueCases default:begin end endcase out_valid<=1;end
           |          else if(out_ready)begin out_valid<=0;if(output_count==STREAM_CYCLES-1)begin output_active<=0;output_prefetched<=0;output_count<=0;if(output_buffer)buffer_1_state<=EMPTY;else buffer_0_state<=EMPTY;end else output_count<=output_count+1;end""".stripMargin
    s"""// Generated by NGen's reusable-PE banked streaming backend.
       |/* verilator lint_off DECLFILENAME */
       |/* verilator lint_off WIDTHEXPAND */
       |/* verilator lint_off WIDTHTRUNC */
       |/* verilator lint_off UNUSEDSIGNAL */
       |module $top(
       |${ports.map("  "+_).mkString(",\n")}
       |);
       |  localparam integer N=${domain.size},STREAMING_WIDTH=$streamingWidth,STREAM_CYCLES=$streamCycles,BUNDLE_COUNT=${schedule.bundles.size},BANK_COUNT=${mapping.bankCount},BANK_DEPTH=${mapping.depth},PE_COUNT=$peCount,RADIX=$radix,BUNDLE_GAP=$gap;
       |  localparam [2:0] EMPTY=0,CAPTURING=1,READY_STATE=2,EXECUTING=3,DONE=4,OUTPUTTING=5;
       |  localparam [${width - 1}:0] MODULUS=${width}'d${field.q};localparam [${width}:0] MODULUS_EXT=${width + 1}'d${field.q};localparam signed [${3 * width}:0] MODULUS_REMAINDER=${3 * width + 1}'sd${field.q};
       |  $reductionParameter
       |  reg [2:0] buffer_0_state,buffer_1_state;reg capture_active,capture_buffer,exec_active,exec_buffer,output_active,output_buffer,output_prefetched;reg [1:0] exec_phase;integer capture_count,bundle_index,gap_count,output_count;
       |  $memories
       |  $romDeclarations
       |  $peDeclarations
       |  $readyAssignment
       |  function automatic [${width - 1}:0] mod_add(input [${width - 1}:0] a,input [${width - 1}:0] b);reg [$width:0] sum,reduced;begin sum={1'b0,a}+{1'b0,b};if(sum>=MODULUS_EXT)reduced=sum-MODULUS_EXT;else reduced=sum;mod_add=reduced[${width - 1}:0];end endfunction
       |  function automatic [${width - 1}:0] mod_sub(input [${width - 1}:0] a,input [${width - 1}:0] b);reg [$width:0] difference;begin if(a>=b)difference={1'b0,a}-{1'b0,b};else difference={1'b0,a}+MODULUS_EXT-{1'b0,b};mod_sub=difference[${width - 1}:0];end endfunction
       |  $multiplyFunction
       |  initial begin
       |    $romInitializers
       |  end
       |  always @(*) begin
       |    $portDefaults
       |    if(!capture_active)begin
       |      if($firstInputAccepted)begin if(buffer_0_state==EMPTY)begin ${captureWrites(0,0)} end else begin ${captureWrites(1,0)} end end
       |    end else if($continuingInputAccepted)begin case(capture_count)$captureCases default:begin end endcase end
       |    if(exec_active&&gap_count==0)begin
       |      if(exec_phase==0)begin $dynamicReadPorts end
       |      else if(exec_phase==2)begin $dynamicWritePorts end
       |    end
       |    if(output_active)begin
       |      if(!output_prefetched)begin case(output_count)$outputCurrentReadCases default:begin end endcase end
       |      else if((output_count<STREAM_CYCLES-1)$outputReadGuard)begin case(output_count)$outputNextReadCases default:begin end endcase end
       |    end
       |  end
       |  always @(posedge clock) begin
       |    $memoryPorts
       |  end
       |  always @(posedge clock) begin
       |    if(reset)begin buffer_0_state<=EMPTY;buffer_1_state<=EMPTY;capture_active<=0;exec_active<=0;output_active<=0;output_prefetched<=0;capture_count<=0;bundle_index<=0;gap_count<=0;output_count<=0;exec_phase<=0;$resetProtocol$resetOutputs end
       |    else begin
       |      $defaultProtocol
       |      if(!capture_active)begin
       |        if($firstInputAccepted)begin
       |          if(buffer_0_state==EMPTY)begin capture_buffer<=0;buffer_0_state<=${if streamCycles == 1 then "READY_STATE" else "CAPTURING"};end else begin capture_buffer<=1;buffer_1_state<=${if streamCycles == 1 then "READY_STATE" else "CAPTURING"};end
       |          ${if streamCycles == 1 then "capture_active<=0;capture_count<=0;" else "capture_active<=1;capture_count<=1;"}
       |        end
       |      end else if($continuingInputAccepted)begin
       |        if(capture_count==STREAM_CYCLES-1)begin capture_active<=0;capture_count<=0;if(capture_buffer)buffer_1_state<=READY_STATE;else buffer_0_state<=READY_STATE;end else capture_count<=capture_count+1;
       |      end
       |      if(!exec_active)begin
       |        if(buffer_0_state==READY_STATE)begin exec_active<=1;exec_buffer<=0;buffer_0_state<=EXECUTING;bundle_index<=0;exec_phase<=0;end
       |        else if(buffer_1_state==READY_STATE)begin exec_active<=1;exec_buffer<=1;buffer_1_state<=EXECUTING;bundle_index<=0;exec_phase<=0;end
       |      end else if(gap_count!=0)gap_count<=gap_count-1;
       |      else if(exec_phase==0)exec_phase<=1;
       |      else if(exec_phase==1)begin $dynamicLoads exec_phase<=2;end
       |      else begin
       |        exec_phase<=0;
       |        if(bundle_index==BUNDLE_COUNT-1)begin exec_active<=0;bundle_index<=0;if(exec_buffer)buffer_1_state<=DONE;else buffer_0_state<=DONE;end
       |        else begin bundle_index<=bundle_index+1;gap_count<=BUNDLE_GAP;end
       |      end
       |      if(!output_active)begin
       |        if(buffer_0_state==DONE)begin output_active<=1;output_buffer<=0;output_count<=0;output_prefetched<=0;buffer_0_state<=OUTPUTTING;end
       |        else if(buffer_1_state==DONE)begin output_active<=1;output_buffer<=1;output_count<=0;output_prefetched<=0;buffer_1_state<=OUTPUTTING;end
       |      end else begin
       |        if(!output_prefetched)output_prefetched<=1;
       |        else begin $outputAdvance end
       |      end
       |    end
       |  end
       |endmodule
       |/* verilator lint_on UNUSEDSIGNAL */
       |/* verilator lint_on WIDTHTRUNC */
       |/* verilator lint_on WIDTHEXPAND */
       |/* verilator lint_on DECLFILENAME */
       |""".stripMargin
