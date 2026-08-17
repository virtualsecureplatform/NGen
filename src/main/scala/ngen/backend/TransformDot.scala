package ngen.backend

import ngen.algebra.{NttDomain, TransformShape}

object TransformDot:
  def emit(domain: NttDomain, inverse: Boolean): String =
    val stages = (1 to domain.logSize).map { stage =>
      val span = 1 << stage
      s"  stage$stage [label=\"radix-2 span $span\"];"
    }
    val edges = (0 until domain.logSize).map { index =>
      val from = if index == 0 then "bitreverse" else s"stage$index"
      s"  $from -> stage${index + 1};"
    }
    val twistIn = if domain.shape == TransformShape.Negacyclic && !inverse then Vector("  twist_in [label=\"negacyclic twist\"];", "  twist_in -> bitreverse;") else Vector.empty
    val scaleOut = if inverse then Vector(s"  scale [label=\"inverse scale${if domain.shape == TransformShape.Negacyclic then " + untwist" else ""}\"];", s"  stage${domain.logSize} -> scale;") else Vector.empty
    (Vector("digraph transform {", s"  bitreverse [label=\"bit reverse N=${domain.size}\"];") ++ twistIn ++ stages ++ edges ++ scaleOut ++ Vector("}")).mkString("\n")
