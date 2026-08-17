package ngen

import ngen.algebra.Domains
import ngen.cli.{Cli, Command, Direction, GeneratorConfig}
import ngen.transform.ReferenceNtt

object Main:
  private def printPlan(config: GeneratorConfig): Unit =
    val direction = if config.direction == Direction.Forward then "forward NTT" else "inverse NTT"
    println(s"Transform: $direction")
    println(s"Domain: ${config.domain.name}")
    println(s"Size: ${config.domain.size} (2^${config.domain.logSize})")
    println(s"Modulus: ${config.domain.modulus.q}")
    println(s"Streaming width: ${config.streamingWidth} (2^${config.streamingLog})")
    println(s"Radix: ${config.radix} (2^${config.radixLog})")
    println(s"Shape: ${config.domain.shape}")

  private def check(config: GeneratorConfig): Unit =
    val input = Vector.tabulate(config.domain.size)(BigInt(_))
    val output = ReferenceNtt.inverse(config.domain, ReferenceNtt.forward(config.domain, input))
    require(output == input.map(config.domain.modulus.normalize), s"round trip failed for ${config.domain.name}")
    println("Mathematical NTT/INTT round-trip passed.")

  def main(args: Array[String]): Unit =
    try
      Cli.parse(args.toSeq) match
        case Command.Help => println(Cli.usage)
        case Command.Version => println(Cli.Version)
        case Command.Presets =>
          Domains.all.foreach(domain => println(s"${domain.name}\tq=${domain.modulus.q}\tN=${domain.size}\t${domain.description}"))
        case Command.Generate(config) =>
          printPlan(config)
          if config.check then check(config)
          println("RTL emission is the next implementation milestone.")
    catch
      case error: IllegalArgumentException =>
        Console.err.println(s"Error: ${error.getMessage}")
        Console.err.println(Cli.usage)
        sys.exit(2)
