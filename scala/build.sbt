val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "GildedRose",
    version := "1.0",
    scalaVersion := scala3Version,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
