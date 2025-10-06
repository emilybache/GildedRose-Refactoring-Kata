val scala3Version = "3.7.3"

lazy val root = project
  .in(file("."))
  .settings(
      name         := "GildedRose",
      version      := "1.0",
      scalaVersion := scala3Version,
      libraryDependencies ++= Seq(
          "org.scalatest"       %% "scalatest"         % "3.2.19" % Test,
          "com.approvaltests"    % "approvaltests"     % "25.4.3" % Test,
          "junit"                % "junit"             % "4.13.2",
          "com.github.sbt.junit" % "jupiter-interface" % "0.15.1" % Test,
          "org.junit.jupiter"    % "junit-jupiter"     % "6.0.0"  % Test
      ),
      testOptions += Tests.Argument(TestFrameworks.JUnit)
  )
