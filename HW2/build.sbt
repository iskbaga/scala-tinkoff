import Dependencies._

ThisBuild / scalaVersion     := "2.13.10"
ThisBuild / version          := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "converter",
    libraryDependencies += scalatest,
    Test / javaOptions ++= Seq("-Xss1M"),
    Test / fork := true
  )
