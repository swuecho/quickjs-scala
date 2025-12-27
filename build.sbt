// QuickJS-Scala: JavaScript Engine in Scala 3

lazy val scala3Version = "3.6.2"

lazy val quickjsScala = project
  .in(file("."))
  .aggregate(
    core,
    parser,
    compiler,
    runtime,
    stdlib
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
