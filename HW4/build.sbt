import Dependencies.scalaTest

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"

Compile / compile / scalacOptions ++= Seq(
  "-Werror",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused",
  "-Wvalue-discard",
  "-Xlint",
  "-Xlint:-byname-implicit",
  "-Xlint:-implicit-recursion",
  "-unchecked",
)

lazy val root = (project in file("."))
  .settings(
    name := "scala-template",
    libraryDependencies += scalaTest
  )
