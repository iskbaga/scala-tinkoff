import Dependencies._

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "building-adt",
    libraryDependencies += scalaTest % Test
  )
