// QuickJS-Scala: JavaScript Engine in Scala 3

import sbtassembly.AssemblyPlugin.autoImport.assembly
import sbtassembly.PathList
import sbtassembly.MergeStrategy

lazy val scala3Version = "3.7.4"

// Shared by every module.
ThisBuild / organization := "quickjs"
ThisBuild / version := "0.1.0"

// Publish to GitHub Packages (https://maven.pkg.github.com/swuecho/quickjs-scala).
ThisBuild / publishTo := Some(
  "GitHub Package Registry" at "https://maven.pkg.github.com/swuecho/quickjs-scala"
)
ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_ACTOR", "swuecho"),
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

// Licensing metadata shared by every module.
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/swuecho/quickjs-scala"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/swuecho/quickjs-scala"),
    "scm:git:https://github.com/swuecho/quickjs-scala.git"
  )
)

// Console capture is thread-local (`Console.withOutput`), so suites can run in
// parallel. Cap concurrency at the processor count: the interpreter-heavy
// suites would otherwise oversubscribe the machine.
ThisBuild / Test / parallelExecution := true
ThisBuild / Test / concurrentRestrictions := Seq(
  Tags.limit(
    Tags.Test,
    math.max(2, java.lang.Runtime.getRuntime.availableProcessors())
  )
)

lazy val quickjsScala = project
  .in(file("."))
  .aggregate(
    core,
    parser,
    compiler,
    runtime,
    stdlib,
    runner,
    webFrontend
  )
  .settings(
    name := "quickjs-scala",
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
      "com.ibm.icu" % "icu4j" % "78.3",
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
      // The runtime executes parser bytecode and therefore must carry the
      // parser's Unicode property provider on downstream classpaths.
      "com.ibm.icu" % "icu4j" % "78.3",
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
  .settings(publish / skip := true)
  .settings(
    name := "quickjs-runner",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      // The runner tokenizes scripts, so ICU must be on its classpath even
      // though it is only declared (transitively) by the runtime project.
      "com.ibm.icu" % "icu4j" % "78.3",
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

lazy val webFrontend = project
  .in(file("web"))
  .enablePlugins(ScalaJSPlugin)
  .settings(publish / skip := true)
  .settings(
    name := "quickjs-web",
    scalaVersion := scala3Version,
    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("quickjs.web.TraceApp"),
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "16.0.0",
      "org.scala-js" %%% "scalajs-dom" % "2.8.0"
    ),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "dist",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "dist"
  )

// ThisBuild / scalacOptions ++= Seq(
//   "-rewrite",
//   "-no-indent",
// )
