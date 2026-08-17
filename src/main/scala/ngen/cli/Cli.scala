package ngen.cli

import ngen.algebra.{Domains, Modulus, NttDomain, TransformShape}
import ngen.rtl.{ArchitectureKind, ProfileName, ReductionChoice, TransposeKind}
import ngen.transform.DataOrder

import scala.collection.mutable

enum Direction:
  case Forward, Inverse, Both

final case class GeneratorConfig(
    domain: NttDomain,
    streamingLog: Int,
    radixLog: Int,
    direction: Direction,
    check: Boolean,
    output: Option[String],
    top: Option[String],
    profile: ProfileName,
    architecture: ArchitectureKind,
    reduction: ReductionChoice,
    transpose: TransposeKind,
    inputOrder: DataOrder,
    outputOrder: DataOrder,
    graph: Boolean,
    rtlGraph: Boolean
):
  val streamingWidth: Int = 1 << streamingLog
  val radix: Int = 1 << radixLog

  require(streamingLog >= 0 && streamingLog <= domain.logSize)
  require(radixLog > 0 && radixLog <= domain.logSize)
  require(domain.logSize % radixLog == 0, s"radix log $radixLog must divide transform log ${domain.logSize}")

enum Command:
  case Generate(config: GeneratorConfig)
  case SwitchTranspose(logSize: Int, dataWidth: Int, output: Option[String], top: Option[String])
  case Presets
  case Version
  case Help

object Cli:
  val Version = "0.2.0-dev"

  val usage: String =
    """NGen - A Generator of Streaming NTT Hardware
      |
      |Usage: ngen [options] <transform>
      |
      |Options:
      |  -preset <name>  Use yata8, yata512, hoge1024, or kyber256 conventions.
      |  -n <n>          log2 of the transform size (required without a preset).
      |  -k <k>          log2 of the streaming width; defaults to n.
      |  -r <r>          log2 of the radix; defaults to the largest divisor of n <= k.
      |  -q <prime>      Field modulus for a custom domain (decimal or 0x hexadecimal).
      |  -root <value>   Primitive N-th root for a custom domain; value may be auto.
      |  -psi <value>    Optional primitive 2N-th root; use -root auto -psi auto for automatic negacyclic roots.
      |  -base-case <n>  Stop an incomplete negacyclic NTT at blocks of size n.
      |  -o <file>       Output file; defaults to design.sv for RTL transforms.
      |  -top <name>     Override the generated top-level module name.
      |  -data-width <w> Element width for switchtranspose; defaults to 64.
      |  -profile <name> Pipeline profile: baseline (default) or f300.
      |  -architecture <a> RTL architecture: auto (default), fully-parallel, or streamed.
      |  -reduction <r>  Modular reduction: auto (default), barrett, montgomery, or shoup.
      |  -transpose <t>  Streaming transpose: indexed (default) or switch.
      |  -input-order <o> Input stream order: natural (default) or bitreversed.
      |  -output-order <o> Output stream order: natural (default) or bitreversed.
      |  -graph          Emit the transform-decomposition DOT graph.
      |  -rtlgraph       Emit the scheduled RTL DOT graph.
      |  -check          Run the mathematical round-trip check before generation.
      |  -nologo         Accepted for SGen command-line compatibility.
      |
      |Transforms:
      |  ntt             Forward NTT.
      |  intt            Inverse NTT.
      |  raintt          Combined YATA forward/inverse benchmark wrapper.
      |  kyberpe         Combined Kyber PE1 forward/inverse wrapper.
      |  switchtranspose Generate a HOGE-style recursive switch transpose.
      |  presets         List built-in field/domain presets.
      |  version         Print the NGen version.
      |
      |Examples:
      |  ngen -preset yata512 -k 6 -r 3 ntt
      |  ngen -preset hoge1024 -k 5 -r 5 intt
      |  ngen -preset kyber256 -k 0 -r 1 ntt
      |  ngen -preset yata8 -k 3 -r 3 -o SmallYata8RainttP27Rtl.sv raintt
      |  ngen -n 3 -q 17 -root 9 -check ntt
      |""".stripMargin

  private def integer(text: String): BigInt =
    if text.startsWith("0x") || text.startsWith("0X") then BigInt(text.drop(2), 16)
    else BigInt(text)

  private def requiredValue(args: mutable.Queue[String], option: String): String =
    args.removeHeadOption().getOrElse(throw new IllegalArgumentException(s"$option requires an argument"))

  def parse(rawArgs: Seq[String]): Command =
    if rawArgs.isEmpty then return Command.Help

    val args = mutable.Queue.from(rawArgs)
    var preset: Option[String] = None
    var n: Option[Int] = None
    var k: Option[Int] = None
    var r: Option[Int] = None
    var q: Option[BigInt] = None
    var root: Option[String] = None
    var psi: Option[String] = None
    var baseCase: Option[Int] = None
    var check = false
    var output: Option[String] = None
    var top: Option[String] = None
    var dataWidth = 64
    var profile = ProfileName.Baseline
    var architecture = ArchitectureKind.Auto
    var reduction = ReductionChoice.Auto
    var transpose = TransposeKind.Indexed
    var inputOrder = DataOrder.Natural
    var outputOrder = DataOrder.Natural
    var graph = false
    var rtlGraph = false
    var terminal: Option[String] = None

    while args.nonEmpty do
      args.dequeue() match
        case "-preset" => preset = Some(requiredValue(args, "-preset").toLowerCase)
        case "-n" => n = Some(requiredValue(args, "-n").toInt)
        case "-k" => k = Some(requiredValue(args, "-k").toInt)
        case "-r" => r = Some(requiredValue(args, "-r").toInt)
        case "-q" => q = Some(integer(requiredValue(args, "-q")))
        case "-root" => root = Some(requiredValue(args, "-root"))
        case "-psi" => psi = Some(requiredValue(args, "-psi"))
        case "-base-case" => baseCase = Some(requiredValue(args, "-base-case").toInt)
        case "-o" => output = Some(requiredValue(args, "-o"))
        case "-top" => top = Some(requiredValue(args, "-top"))
        case "-data-width" => dataWidth = requiredValue(args, "-data-width").toInt
        case "-profile" => profile = ProfileName.parse(requiredValue(args, "-profile"))
        case "-architecture" => architecture = ArchitectureKind.parse(requiredValue(args, "-architecture"))
        case "-reduction" => reduction = ReductionChoice.parse(requiredValue(args, "-reduction"))
        case "-transpose" => transpose = TransposeKind.parse(requiredValue(args, "-transpose"))
        case "-input-order" => inputOrder = DataOrder.parse(requiredValue(args, "-input-order"))
        case "-output-order" => outputOrder = DataOrder.parse(requiredValue(args, "-output-order"))
        case "-graph" => graph = true
        case "-rtlgraph" => rtlGraph = true
        case "-check" => check = true
        case "-nologo" => ()
        case "-h" | "--help" | "help" => terminal = Some("help")
        case value @ ("ntt" | "intt" | "raintt" | "kyberpe" | "switchtranspose" | "presets" | "version") =>
          require(terminal.isEmpty, s"multiple transforms specified: ${terminal.get} and $value")
          terminal = Some(value)
        case unknown => throw new IllegalArgumentException(s"unknown argument: $unknown")

    terminal match
      case Some("help") => Command.Help
      case Some("presets") => Command.Presets
      case Some("version") => Command.Version
      case Some("switchtranspose") =>
        require(preset.isEmpty && q.isEmpty && root.isEmpty && psi.isEmpty && baseCase.isEmpty, "switchtranspose uses -n and -data-width, not a transform domain")
        val logSize = n.getOrElse(throw new IllegalArgumentException("switchtranspose requires -n"))
        require(logSize > 0 && logSize < 16)
        require(dataWidth > 0)
        Command.SwitchTranspose(logSize, dataWidth, output, top)
      case Some(transform @ ("ntt" | "intt" | "raintt" | "kyberpe")) =>
        val selected = preset match
          case Some(name) =>
            require(n.isEmpty && q.isEmpty && root.isEmpty && psi.isEmpty && baseCase.isEmpty, "a preset cannot be combined with custom-domain options")
            Domains.named(name).getOrElse(
              throw new IllegalArgumentException(s"unknown preset '$name'; expected ${Domains.all.map(_.name).mkString(", ")}")
            )
          case None =>
            val logSize = n.getOrElse(throw new IllegalArgumentException("-n is required without -preset"))
            require(logSize > 0 && logSize < 31, s"-n must be between 1 and 30, got $logSize")
            val modulus = Modulus(q.getOrElse(throw new IllegalArgumentException("-q is required without -preset")))
            require(modulus.q.isProbablePrime(80), s"-q must be prime, got ${modulus.q}")
            val rootText = root.getOrElse(throw new IllegalArgumentException("-root is required without -preset"))
            require(baseCase.isEmpty || psi.isEmpty, "incomplete transforms do not use -psi")
            require(psi.forall(_.toLowerCase != "auto") || rootText.toLowerCase == "auto", "-psi auto requires -root auto")
            val automaticPsi = psi.filter(_.toLowerCase == "auto").map { _ =>
              require(logSize < 30, "automatic 2N-th root search requires n < 30")
              modulus.findPowerOfTwoRoot(2 << logSize)
            }
            val normalizedPsi = psi.map(text => automaticPsi.getOrElse(integer(text)))
            val nthRoot =
              if rootText.toLowerCase == "auto" then automaticPsi.map(value => modulus.multiply(value, value)).getOrElse(modulus.findPowerOfTwoRoot(1 << logSize))
              else integer(rootText)
            NttDomain(
              name = "custom",
              size = 1 << logSize,
              modulus = modulus,
              root = nthRoot,
              shape = baseCase.map(TransformShape.IncompleteNegacyclic.apply).getOrElse(if psi.isDefined then TransformShape.Negacyclic else TransformShape.Cyclic),
              twist = normalizedPsi,
              description = "custom command-line NTT domain"
            )

        selected.validate()
        val streamingLog = k.getOrElse(selected.logSize)
        val radixLog = r.getOrElse((1 to streamingLog).reverse.find(selected.logSize % _ == 0).getOrElse(1))
        Command.Generate(
          GeneratorConfig(
            selected,
            streamingLog,
            radixLog,
            if transform == "ntt" then Direction.Forward
            else if transform == "intt" then Direction.Inverse
            else Direction.Both,
            check,
            output,
            top,
            profile,
            architecture,
            reduction,
            transpose,
            inputOrder,
            outputOrder,
            graph,
            rtlGraph
          )
        )
      case _ => throw new IllegalArgumentException("a transform name is required")
