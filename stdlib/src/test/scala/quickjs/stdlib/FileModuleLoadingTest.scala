package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.module.{FileModuleLoader, ModuleLoader}
import quickjs.value.JSValue
import munit.*

import java.nio.file.{Files, Path}
import scala.collection.mutable

class FileModuleLoadingTest extends FunSuite:

  // Track temp directories for cleanup
  private val tempDirs = mutable.ArrayBuffer.empty[Path]

  override def afterAll(): Unit =
    // Clean up all temp directories
    tempDirs.synchronized {
      tempDirs.foreach { dir =>
        if dir != null && Files.exists(dir) then
          try
            Files
              .walk(dir)
              .sorted(java.util.Comparator.reverseOrder())
              .forEach(Files.delete(_))
          catch case _: Exception => () // Ignore cleanup errors
      }
    }

  /** Create a fresh temp directory for a test */
  private def createTempDir(): Path =
    val dir = Files.createTempDirectory("quickjs-module-test")
    tempDirs.synchronized {
      tempDirs += dir
    }
    dir

  private def writeModule(tempDir: Path, name: String, content: String): Path =
    val path = tempDir.resolve(name)
    Files.createDirectories(path.getParent)
    Files.writeString(path, content)
    path

  private def evalWithModuleLoader(
      source: String,
      moduleName: String = "<main>"
  )(using ctx: JSContext, loader: ModuleLoader): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileModule(ast, moduleName)
    val interpreter = Interpreter()
    ctx.currentModulePath = moduleName
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("load module from file with named exports") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    // Create the module file
    writeModule(
      tempDir,
      "math.js",
      """
        |export const PI = 3.14159;
        |export function square(x) { return x * x; }
        |export function add(a, b) { return a + b; }
        |""".stripMargin
    )

    // Create main module that imports from math.js
    val mainPath = writeModule(
      tempDir,
      "main.js",
      """
        |import { PI, square, add } from "./math.js";
        |export const result = add(square(2), PI);
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    // Check the exported result
    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    val result = mainExports.get.get("result")
    // square(2) = 4, add(4, 3.14159) = 7.14159
    assertEquals(result.toNumber, 7.14159, 0.00001)
  }

  test("load module with default export") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "greeter.js",
      """
        |export default function(name) { return "Hello, " + name + "!"; }
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "greet-main.js",
      """
        |import greet from "./greeter.js";
        |export const message = greet("World");
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    val message = mainExports.get.get("message")
    assertEquals(message.toString, "Hello, World!")
  }

  test("load module with namespace import") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "utils.js",
      """
        |export const version = "1.0.0";
        |export function double(x) { return x * 2; }
        |export function triple(x) { return x * 3; }
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "utils-main.js",
      """
        |import * as utils from "./utils.js";
        |export const v = utils.version;
        |export const result = utils.double(5) + utils.triple(3);
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    assertEquals(mainExports.get.get("v").toString, "1.0.0")
    // double(5) = 10, triple(3) = 9, total = 19
    assertEquals(mainExports.get.get("result").toNumber, 19.0)
  }

  test("relative imports from subdirectory") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    // Create a subdirectory structure
    writeModule(
      tempDir,
      "lib/helper.js",
      """
        |export function format(s) { return "[" + s + "]"; }
        |""".stripMargin
    )

    writeModule(
      tempDir,
      "lib/index.js",
      """
        |import { format } from "./helper.js";
        |export function wrap(s) { return format(s); }
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "subdir-main.js",
      """
        |import { wrap } from "./lib/index.js";
        |export const result = wrap("test");
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    assertEquals(mainExports.get.get("result").toString, "[test]")
  }

  test("re-export from another module") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "base.js",
      """
        |export const a = 1;
        |export const b = 2;
        |export const c = 3;
        |""".stripMargin
    )

    writeModule(
      tempDir,
      "reexport.js",
      """
        |export { a, b } from "./base.js";
        |export const extra = 100;
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "reexport-main.js",
      """
        |import { a, b, extra } from "./reexport.js";
        |export const sum = a + b + extra;
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    // 1 + 2 + 100 = 103
    assertEquals(mainExports.get.get("sum").toNumber, 103.0)
  }

  test("export * from another module") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "source.js",
      """
        |export const x = 10;
        |export const y = 20;
        |export default "ignored";
        |""".stripMargin
    )

    writeModule(
      tempDir,
      "barrel.js",
      """
        |export * from "./source.js";
        |export const z = 30;
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "barrel-main.js",
      """
        |import { x, y, z } from "./barrel.js";
        |export const total = x + y + z;
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    // 10 + 20 + 30 = 60
    assertEquals(mainExports.get.get("total").toNumber, 60.0)
  }

  test("circular dependency returns partial exports") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    // Module A imports B, B imports A
    writeModule(
      tempDir,
      "circular-a.js",
      """
        |export const fromA = "A";
        |import { fromB } from "./circular-b.js";
        |export const gotFromB = fromB;
        |""".stripMargin
    )

    writeModule(
      tempDir,
      "circular-b.js",
      """
        |export const fromB = "B";
        |import { fromA } from "./circular-a.js";
        |export const gotFromA = fromA;
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "circular-main.js",
      """
        |import { fromA, gotFromB } from "./circular-a.js";
        |export const a = fromA;
        |export const b = gotFromB;
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    assertEquals(mainExports.get.get("a").toString, "A")
    assertEquals(mainExports.get.get("b").toString, "B")
  }

  test("module resolution adds .js extension") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "no-ext.js",
      """
        |export const value = 42;
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "no-ext-main.js",
      """
        |import { value } from "./no-ext";
        |export const result = value;
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    assertEquals(mainExports.get.get("result").toNumber, 42.0)
  }

  test("dynamic import resolves relative file module") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    writeModule(
      tempDir,
      "dynamic-child.js",
      """
        |export const value = 33;
        |""".stripMargin
    )

    val mainPath = writeModule(
      tempDir,
      "dynamic-main.js",
      """
        |var seen = [];
        |import("./dynamic-child.js").then(function(ns) {
        |  seen.push(ns.value);
        |});
        |export const values = seen;
        |""".stripMargin
    )

    evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)

    val mainExports = summon[JSContext].rt.getModuleExports(mainPath.toString)
    assert(mainExports.isDefined)
    mainExports.get.get("values") match
      case JSValue.JSArrayVal(arr) =>
        assertEquals(arr.get(0).toNumber, 33.0)
      case other => fail(s"Expected array export, got $other")
  }

  test("module not found throws error") {
    val tempDir = createTempDir()
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    given ModuleLoader = FileModuleLoader(tempDir)
    StdLib.initialize(summon[JSContext], Some(summon[ModuleLoader]))

    val mainPath = writeModule(
      tempDir,
      "missing-main.js",
      """
        |import { foo } from "./nonexistent.js";
        |""".stripMargin
    )

    // Should throw a JSException when trying to load a non-existent module
    intercept[quickjs.runtime.JSException] {
      evalWithModuleLoader(Files.readString(mainPath), mainPath.toString)
    }
  }
