val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
      name                                      := "GildedRose",
      version                                   := "1.0",
      scalaVersion                              := scala3Version,
      libraryDependencies += "org.scalatest"    %% "scalatest"     % "3.2.19" % "test",
      libraryDependencies += "com.approvaltests" % "approvaltests" % "25.4.3" % "test",
      libraryDependencies += "org.junit.jupiter" % "junit-jupiter" % "5.11.4" % "test"
  )
