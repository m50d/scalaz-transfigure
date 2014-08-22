import sbt._
import Keys._

object Transfigure extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.2",
    organization := "org.scalaz",
    version := "0.1.0-SNAPSHOT"
  )

  lazy val macro = Project(
    id = "scalaz-transfigure-macro",
    base = file("macro"),
    settings = sharedSettings
  ) settings (
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.specs2" %% "specs2" % "2.4.1" % "test",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  )

  lazy val root = Project(
    id = "scalaz-transfigure",
    base = file("."),
    settings = sharedSettings
  ) settings (
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.1.0",
      "org.specs2" %% "specs2" % "2.4.1" % "test"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  ) dependsOn(macro) aggregate(macro)
}
