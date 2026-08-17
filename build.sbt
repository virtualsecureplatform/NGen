ThisBuild / scalaVersion := "3.7.3"

lazy val root = (project in file("."))
  .settings(
    name := "ngen",
    organization := "org.virtualsecureplatform",
    version := "0.1.0-SNAPSHOT",
    Compile / run / mainClass := Some("ngen.Main"),
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-release", "17"),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
