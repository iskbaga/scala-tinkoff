import sbt._

object Dependencies {
  val scalaTestVersion = "3.2.17"
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion % Test
}
