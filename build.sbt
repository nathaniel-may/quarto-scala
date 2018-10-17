
scalaVersion := "2.12.4"
name := "hello-world"
organization := "ch.epfl.scala"
version := "1.0"

lazy val quarto = (project in file("."))
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  )