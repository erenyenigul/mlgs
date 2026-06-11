import sbt.Keys.libraryDependencies

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.0"

lazy val root = (project in file("."))
  .settings(
    name := "ml-gs",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.8.1",
  )
