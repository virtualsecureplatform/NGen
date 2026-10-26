package ngen.backend

/** Advertised axes are a superset; `plan` validates a concrete combination using
  * the same lowering as emission, so unsupported cross-products are never claims.
  */
object SearchCapabilities:
  val json: String = """{
    "schema":"ngen-capabilities-v1",
    "validation_command":"plan",
    "validation_method":"actual lowering into temporary storage",
    "generic":{
      "architectures":["streamed","compact","fully-parallel","stage-parallel","full-throughput"],
      "reductions":["auto","barrett","montgomery","shoup"],
      "profiles":["baseline","f300","split-barrett"],
      "profile_constraints":"split-barrett requires custom fully-parallel architecture",
      "protocols":["next","ready-valid"],
      "stage_groups":{"min":1,"max":"log2(N)","constraints":"groups > 1 require complete radix-2 custom streamed/compact ready-valid raw indexed designs"},
      "pipeline_controls":{
        "multiplier_pipeline_stages":[1,3],
        "reduction_pipeline_stages":[3,4,5],
        "output_register":[false,true],
        "default":"multiplier 1 for fields up to 32 bits, otherwise 3; reduction 4; output register false",
        "constraints":"custom complete radix-2 streamed/compact Montgomery ready-valid raw indexed; no runtime control",
        "butterfly_latency":"2 + multiplier_pipeline_stages + reduction_pipeline_stages; core output register adds one core cycle"
      },
      "pe_count":"positive; actual count and bank constraints are reported by plan",
      "radix_log":"positive divisor of log2(N)"
    },
    "presets":{
      "names":["yata8","yata64","yata512","hoge32","hoge1024","kyber256"],
      "backends":["microcoded","compact","stage-parallel","full-throughput"],
      "constraints":"fixed field, lane width, radix and interface; validate each combination with plan"
    },
    "metadata_timing":"declared, requires independent simulation measurement"
  }"""
