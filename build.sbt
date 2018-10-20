// *****************************************************************************
// Projects
// *****************************************************************************

lazy val bob =
  project
    .in(file("."))
    .enablePlugins(AutomateHeaderPlugin)
    .aggregate(core)
    .dependsOn(core)
    .settings(settings)

lazy val core =
  project
    .in(file("core"))
    .settings(
      libraryDependencies ++= Seq(
        library.fastparse,
        library.scalaCheck % Test,
        library.scalaTest  % Test
      )
    )

// *****************************************************************************
// Library dependencies
// *****************************************************************************

lazy val library =
  new {
    object Version {
      val fastparse  = "1.0.0"
      val scalaCheck = "1.14.0"
      val scalaTest  = "3.0.5"
    }
    val fastparse  = "com.lihaoyi"    %% "fastparse"  % Version.fastparse
    val scalaCheck = "org.scalacheck" %% "scalacheck" % Version.scalaCheck
    val scalaTest  = "org.scalatest"  %% "scalatest"  % Version.scalaTest
  }

// *****************************************************************************
// Settings
// *****************************************************************************

lazy val settings =
  commonSettings ++
  scalafmtSettings

lazy val commonSettings =
  Seq(
    scalaVersion := "2.12.7",
    organization := "Branislav Lazic",
    organizationName := "Branislav Lazic",
    startYear := Some(2018),
    licenses += ("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-language:_",
      "-target:jvm-1.8",
      "-encoding", "UTF-8",
      "-Ypartial-unification",
      "-Ywarn-unused-import"
    ),
    Compile / unmanagedSourceDirectories := Seq((Compile / scalaSource).value),
    Test / unmanagedSourceDirectories := Seq((Test / scalaSource).value),
    testFrameworks += new TestFramework("utest.runner.Framework")
)

lazy val scalafmtSettings =
  Seq(
    scalafmtOnCompile := true
  )
