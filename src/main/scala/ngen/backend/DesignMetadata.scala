package ngen.backend

import ngen.algebra.NttDomain
import ngen.rtl.{Architecture, ProfileName}

final case class DesignMetadata(
    generatorVersion: String,
    domain: NttDomain,
    architecture: Architecture,
    direction: String,
    radix: Int,
    outputFile: String,
    inputOrder: String = "natural",
    outputOrder: String = "natural",
    architectureParameters: Map[String, Int] = Map.empty
):
  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""

  def toJson: String =
    val profile = architecture.profile.name match
      case ProfileName.Baseline => "baseline"
      case ProfileName.F300 => "f300"
    val parameters = architectureParameters.toVector.sortBy(_._1).map { case (name, value) => s"    ${quote(name)}: $value" }.mkString(",\n")
    s"""{
       |  "schema": "ngen-design-v1",
       |  "generator_version": ${quote(generatorVersion)},
       |  "domain": ${quote(domain.name)},
       |  "modulus": ${quote(domain.modulus.q.toString)},
       |  "transform_size": ${domain.size},
       |  "direction": ${quote(direction)},
       |  "architecture": ${quote(architecture.name)},
       |  "input_order": ${quote(inputOrder)},
       |  "output_order": ${quote(outputOrder)},
       |  "streaming_width": ${architecture.contract.streamingWidth},
       |  "radix": $radix,
       |  "profile": ${quote(profile)},
       |  "reduction": ${quote(architecture.reduction.toString)},
       |  "latency": ${architecture.contract.latency},
       |  "initiation_interval": ${architecture.contract.initiationInterval},
       |  "input_cycles": ${architecture.contract.inputCycles},
       |  "output_cycles": ${architecture.contract.outputCycles},
       |  "architecture_parameters": {
       |$parameters
       |  },
       |  "dependencies": [],
       |  "output_file": ${quote(outputFile)}
       |}
       |""".stripMargin
