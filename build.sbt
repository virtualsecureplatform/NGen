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
    assembly / mainClass := Some("ngen.Main"),
    assembly / assemblyJarName := "ngen.bat",
    assembly / assemblyOutputPath := baseDirectory.value / "ngen.bat",
    assembly / assemblyPrependShellScript := Some(defaultUniversalScript(shebang = false))
  )
