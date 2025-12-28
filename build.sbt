// QuickJS-Scala: JavaScript Engine in Scala 3

import sbtassembly.AssemblyPlugin.autoImport.assembly
import sbtassembly.PathList
import sbtassembly.MergeStrategy

lazy val scala3Version = "3.7.4"

lazy val quickjsScala = project
  .in(file("."))
  .aggregate(
    core,
    parser,
    compiler,
    runtime,
    stdlib,
    runner
  )
  .settings(
    name := "quickjs-scala",
    version := "0.1.0-SNAPSHOT",
    organization := "quickjs",
    scalaVersion := scala3Version,
    publish / skip := true
  )

lazy val core = project
  .settings(
    name := "quickjs-core",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    )
  )

lazy val parser = project
  .dependsOn(core)
  .settings(
    name := "quickjs-parser",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    )
  )

lazy val compiler = project
  .dependsOn(core, parser)
  .settings(
    name := "quickjs-compiler",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    )
  )

lazy val runtime = project
  .dependsOn(core, compiler)
  .settings(
    name := "quickjs-runtime",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.26.1",
      "org.scalameta" %% "munit" % "1.0.2" % Test
    )
  )

lazy val stdlib = project
  .dependsOn(runtime)
  .settings(
    name := "quickjs-stdlib",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    ),
    Compile / mainClass := Some("quickjs.stdlib.Main")
  )

lazy val runner = project
  .dependsOn(stdlib)
  .enablePlugins(AssemblyPlugin)
  .settings(
    name := "quickjs-runner",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.2" % Test
    ),
    Compile / mainClass := Some("quickjs.stdlib.Runner"),
    assembly / mainClass := Some("quickjs.stdlib.Runner"),
    assembly / assemblyJarName := "quickjs-runner.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    }
  )

