import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2" % "test"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.1" % "test"
}
