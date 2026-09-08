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
      "profiles":["baseline","f300"],
      "protocols":["next","ready-valid"],
      "stage_groups":{"min":1,"max":"log2(N)","constraints":"groups > 1 require complete radix-2 custom streamed/compact ready-valid raw indexed designs"},
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
