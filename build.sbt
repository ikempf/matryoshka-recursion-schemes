import Dependencies._

lazy val recursionSchemes = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.ikempf",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )),
    libraryDependencies += matryoshka,
    scalacOptions += "-Ypartial-unification"
  )
