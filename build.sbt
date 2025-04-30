ThisBuild / organization := "es.eriktorr"
ThisBuild / version := "1.0.0"
ThisBuild / idePackagePrefix := Some("es.eriktorr")
Global / excludeLintKeys += idePackagePrefix

ThisBuild / scalaVersion := "3.6.4"

ThisBuild / semanticdbEnabled := true
ThisBuild / javacOptions ++= Seq("-source", "21", "-target", "21")

Global / cancelable := true
Global / fork := true
Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias(
  "check",
  "; undeclaredCompileDependenciesTest; unusedCompileDependenciesTest; scalafixAll; scalafmtSbtCheck; scalafmtCheckAll",
)

lazy val MUnitFramework = new TestFramework("munit.Framework")
//lazy val warts = Warts.unsafe.filter(_ != Wart.DefaultArguments)
lazy val warts = Warts.unsafe.filter(x => !Set(Wart.Any, Wart.DefaultArguments).contains(x)) // TODO

lazy val withBaseSettings: Project => Project = _.settings(
  Compile / doc / sources := Seq(),
  tpolecatDevModeOptions ++= Set(
    org.typelevel.scalacoptions.ScalacOptions.other("-java-output-version", List("21"), _ => true),
    org.typelevel.scalacoptions.ScalacOptions.warnOption("safe-init"),
    org.typelevel.scalacoptions.ScalacOptions.privateOption("explicit-nulls"),
  ),
  Compile / compile / wartremoverErrors ++= warts,
  Test / compile / wartremoverErrors ++= warts,
  libraryDependencies ++= Seq(
    "org.scalameta" %% "munit" % "1.0.4" % Test,
    "org.scalameta" %% "munit-scalacheck" % "1.0.0" % Test,
  ),
  Test / envVars := Map(
    "SBT_TEST_ENV_VARS" -> "true",
  ),
  Test / testFrameworks += MUnitFramework,
  Test / testOptions += Tests.Argument(MUnitFramework, "--exclude-tags=online"),
)

lazy val withCatsEffect: Project => Project = withBaseSettings.compose(
  _.settings(
    libraryDependencies ++= Seq(
      "io.chrisdavenport" %% "cats-scalacheck" % "0.3.2" % Test,
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-effect" % "3.6.1",
      "org.typelevel" %% "cats-effect-kernel" % "3.6.1",
      "org.typelevel" %% "cats-effect-std" % "3.6.1",
      "org.typelevel" %% "cats-kernel" % "2.13.0",
      "org.typelevel" %% "munit-cats-effect" % "2.1.0" % Test,
      "org.typelevel" %% "scalacheck-effect-munit" % "1.0.4" % Test,
    ),
  ),
)

lazy val root = (project in file("."))
  .configure(withCatsEffect)
  .settings(
    name := "shopping-cart",
    libraryDependencies ++= Seq(
      "io.github.iltotore" %% "iron" % "3.0.0",
      "io.github.iltotore" %% "iron-cats" % "3.0.0",
    ),
  )
