package ngen.rtl

enum ProfileName:
  case Baseline, F300

object ProfileName:
  def parse(value: String): ProfileName = value.toLowerCase match
    case "baseline" => ProfileName.Baseline
    case "f300" => ProfileName.F300
    case other => throw new IllegalArgumentException(s"unknown pipeline profile '$other'; expected baseline or f300")

final case class PipelineProfile(
    name: ProfileName,
    addLatency: Int,
    multiplierLatency: Int,
    reductionLatency: Int,
    memoryReadLatency: Int
):
  require(Seq(addLatency, multiplierLatency, reductionLatency, memoryReadLatency).forall(_ >= 0))

object PipelineProfile:
  val Baseline = PipelineProfile(ProfileName.Baseline, addLatency = 1, multiplierLatency = 2, reductionLatency = 1, memoryReadLatency = 1)
  val F300 = PipelineProfile(ProfileName.F300, addLatency = 1, multiplierLatency = 3, reductionLatency = 2, memoryReadLatency = 1)
  def named(name: ProfileName): PipelineProfile = if name == ProfileName.Baseline then Baseline else F300

enum ReductionKind:
  case YataSredc, Goldilocks, KyberMontgomery, Barrett, Montgomery, Shoup, FermatShift

enum ReductionChoice:
  case Auto, Barrett, Montgomery, Shoup, FermatShift

object ReductionChoice:
  def parse(value: String): ReductionChoice = value.toLowerCase match
    case "auto" => ReductionChoice.Auto
    case "barrett" => ReductionChoice.Barrett
    case "montgomery" => ReductionChoice.Montgomery
    case "shoup" => ReductionChoice.Shoup
    case "fermat" | "fermat-shift" => ReductionChoice.FermatShift
    case other => throw new IllegalArgumentException(s"unknown reduction '$other'; expected auto, barrett, montgomery, shoup, or fermat-shift")

enum ArchitectureKind:
  case Auto, FullyParallel, Streamed

object ArchitectureKind:
  def parse(value: String): ArchitectureKind = value.toLowerCase.replace("-", "") match
    case "auto" => ArchitectureKind.Auto
    case "fullyparallel" | "parallel" => ArchitectureKind.FullyParallel
    case "streamed" | "streaming" => ArchitectureKind.Streamed
    case other => throw new IllegalArgumentException(s"unknown architecture '$other'; expected auto, fully-parallel, or streamed")

enum TransposeKind:
  case Indexed, Switch

object TransposeKind:
  def parse(value: String): TransposeKind = value.toLowerCase match
    case "indexed" => TransposeKind.Indexed
    case "switch" => TransposeKind.Switch
    case other => throw new IllegalArgumentException(s"unknown transpose '$other'; expected indexed or switch")

enum StreamProtocol:
  case NextPulse, ReadyValid

object StreamProtocol:
  def parse(value: String): StreamProtocol = value.toLowerCase.replace("-", "") match
    case "next" | "nextpulse" => StreamProtocol.NextPulse
    case "readyvalid" | "rv" => StreamProtocol.ReadyValid
    case other => throw new IllegalArgumentException(s"unknown stream protocol '$other'; expected next or ready-valid")

enum PortDirection:
  case Input, Output

final case class Port(name: String, direction: PortDirection, format: ValueFormat)

final case class MemorySpec(
    name: String,
    depth: Int,
    format: ValueFormat,
    banks: Int = 1,
    readLatency: Int = 1
):
  require(depth > 0 && banks > 0 && readLatency >= 0)

final case class CounterSpec(name: String, modulus: Int):
  require(modulus > 0)
  val width: Int = math.max(1, 32 - Integer.numberOfLeadingZeros(modulus - 1))

final case class StreamingContract(
    transformSize: Int,
    streamingWidth: Int,
    inputCycles: Int,
    outputCycles: Int,
    latency: Int,
    initiationInterval: Int
):
  require(transformSize > 0 && streamingWidth > 0)
  require(inputCycles > 0 && outputCycles > 0 && latency >= 0 && initiationInterval > 0)

/** Stateful shell around one or more acyclic timed datapath graphs. */
final case class Architecture(
    name: String,
    ports: Vector[Port],
    datapaths: Vector[TimedGraph],
    memories: Vector[MemorySpec],
    counters: Vector[CounterSpec],
    contract: StreamingContract,
    reduction: ReductionKind,
    profile: PipelineProfile
)
