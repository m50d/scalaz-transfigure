import sbt._
import Keys._

object Transfigure extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.1",
    organization := "org.scalaz",
    version := "0.1.0-SNAPSHOT"
  )

  lazy val root = Project(
    id = "scalaz-transfigure",
    base = file("."),
    settings = sharedSettings
  ) settings (
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.0.6",
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    )
  )
}
