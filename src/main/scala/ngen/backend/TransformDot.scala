package ngen.backend

import ngen.algebra.{NttDomain, TransformShape}

object TransformDot:
  def emit(domain: NttDomain, inverse: Boolean, radixLog: Int = 1): String =
    require(radixLog > 0)
    val effectiveLog = domain.shape match
      case TransformShape.IncompleteNegacyclic(baseCaseSize) => domain.logSize - Integer.numberOfTrailingZeros(baseCaseSize)
      case _ => domain.logSize
    val stageCount = (effectiveLog + radixLog - 1) / radixLog
    val stages = (1 to stageCount).map { stage =>
      val consumed = math.min(stage * radixLog, effectiveLog)
      val span = 1 << consumed
      s"  stage$stage [label=\"radix-${1 << math.min(radixLog, effectiveLog)} span $span\"];"
    }
    val edges = (0 until stageCount).map { index =>
      val from = if index == 0 then "bitreverse" else s"stage$index"
      s"  $from -> stage${index + 1};"
    }
    val twistIn = if domain.shape == TransformShape.Negacyclic && !inverse then Vector("  twist_in [label=\"negacyclic twist\"];", "  twist_in -> bitreverse;") else Vector.empty
    val scaleOut = if inverse then Vector(s"  scale [label=\"inverse scale${if domain.shape == TransformShape.Negacyclic then " + untwist" else ""}\"];", s"  stage$stageCount -> scale;") else Vector.empty
    (Vector("digraph transform {", s"  bitreverse [label=\"bit reverse N=${domain.size}\"];") ++ twistIn ++ stages ++ edges ++ scaleOut ++ Vector("}")).mkString("\n")
