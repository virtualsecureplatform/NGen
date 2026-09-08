import sbtassembly.AssemblyPlugin.defaultUniversalScript

ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "ngen",
    organization := "org.virtualsecureplatform",
    version := "0.1.0",
    Compile / run / mainClass := Some("ngen.Main"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-release", "17"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    Compile / resourceGenerators += Def.task {
      val base = baseDirectory.value
      val main = ((base / "src" / "main") ** "*").get.filter(_.isFile)
      val projectInputs = ((base / "project") ** "*").get.filter { f =>
        val relative = IO.relativize(base / "project", f).get.split("/").toSeq
        f.isFile && !relative.contains("target") && !relative.dropRight(1).contains("project") &&
          Set("sbt", "scala", "properties").contains(f.ext)
      }
      val inputs = (Seq(base / "build.sbt") ++ main ++ projectInputs).distinct
        .sortBy(f => IO.relativize(base, f).get)
      val rows = inputs.map { f =>
        val name = IO.relativize(base, f).get.replace('\\', '/')
        require(!name.exists(c => c == '\n' || c == '\r' || c == '\t'), "unsupported build-input path")
        val hash = java.security.MessageDigest.getInstance("SHA-256").digest(IO.readBytes(f))
          .map(b => f"${b & 0xff}%02x").mkString
        s"$hash\t$name"
      }.mkString("", "\n", "\n")
      val output = (Compile / resourceManaged).value / "META-INF" / "ngen" / "source-inputs.sha256"
      if (!output.exists || IO.read(output) != rows) IO.write(output, rows)
      Seq(output)
    }.taskValue,
    assembly / mainClass := Some("ngen.Main"),
    assembly / assemblyJarName := "ngen.bat",
    assembly / assemblyOutputPath := baseDirectory.value / "ngen.bat",
    assembly / assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))
  )
