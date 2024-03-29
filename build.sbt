ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test"

lazy val root = (project in file("."))
  .settings(
    name := "Raytracer",
    Compile / scalaSource := baseDirectory.value / "src/main",
    Test / scalaSource := baseDirectory.value / "src/test"
  )
