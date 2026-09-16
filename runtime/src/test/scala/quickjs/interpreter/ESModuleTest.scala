package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSException, JSRuntime, StdLib}
import quickjs.module.InMemoryModuleLoader
import quickjs.value.JSValue
import munit.*

class ESModuleTest extends FunSuite:

  def withContext(testCode: (JSRuntime, JSContext) => Unit): Unit =
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)
    testCode(runtime, ctx)

  def evalModule(source: String, moduleName: String = "<test>")(using
      ctx: JSContext
  ): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = new Parser(tokens, moduleMode = true)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileModule(ast, moduleName)
    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)
    // Module bodies are compiled async; surface evaluation failures the way a
    // loader would (rejections become throws).
    quickjs.module.ModuleEvaluation.settleAndCheck(result)
    result

  test("basic export default") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val source = """
        export default 42;
      """

      evalModule(source, "test.js")
      val exports = runtime.getModuleExports("test.js").get
      assertEquals(exports.get("default"), JSValue.Int32(42))
    }
  }

  test("basic named export") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val source = """
        export const foo = 123;
        export function bar() { return 456; }
      """

      evalModule(source, "test.js")
      val exports = runtime.getModuleExports("test.js").get
      assertEquals(exports.get("foo"), JSValue.Int32(123))
      // Function exports would need to be checked differently
    }
  }

  test("import from another module") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      // Set up module loader with two modules
      val loader = InMemoryModuleLoader()
        .register(
          "math.js",
          """
          export const add = (a, b) => a + b;
          export const PI = 3.14159;
        """
        )
        .register(
          "main.js",
          """
          import { add, PI } from "math.js";
          export const result = add(10, 20);
          export const circumference = 2 * PI * 5;
        """
        )

      runtime.setModuleLoader(loader)

      // Evaluate main.js which imports math.js
      evalModule(loader.load("main.js").source, "main.js")

      val exports = runtime.getModuleExports("main.js").get
      assertEquals(exports.get("result"), JSValue.Int32(30))
      // PI would be Float64
      val circumference = exports.get("circumference")
      assert(circumference.toNumber > 31.4 && circumference.toNumber < 31.42)
    }
  }

  test("import default") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val loader = InMemoryModuleLoader()
        .register(
          "utils.js",
          """
          export default function greet(name) {
            return "Hello, " + name;
          }
        """
        )
        .register(
          "main.js",
          """
          import greet from "utils.js";
          export const message = greet("World");
        """
        )

      runtime.setModuleLoader(loader)

      evalModule(loader.load("main.js").source, "main.js")

      val exports = runtime.getModuleExports("main.js").get
      assertEquals(exports.get("message"), JSValue.fromString("Hello, World"))
    }
  }

  test("import namespace") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val loader = InMemoryModuleLoader()
        .register(
          "constants.js",
          """
          export const A = 1;
          export const B = 2;
          export const C = 3;
        """
        )
        .register(
          "main.js",
          """
          import * as Consts from "constants.js";
          export const sum = Consts.A + Consts.B + Consts.C;
        """
        )

      runtime.setModuleLoader(loader)

      evalModule(loader.load("main.js").source, "main.js")

      val exports = runtime.getModuleExports("main.js").get
      assertEquals(exports.get("sum"), JSValue.Int32(6))
    }
  }

  test("export all") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val loader = InMemoryModuleLoader()
        .register(
          "a.js",
          """
          export const x = 1;
          export const y = 2;
        """
        )
        .register(
          "b.js",
          """
          export * from "a.js";
          export const z = 3;
        """
        )
        .register(
          "main.js",
          """
          import { x, y, z } from "b.js";
          export const sum = x + y + z;
        """
        )

      runtime.setModuleLoader(loader)

      evalModule(loader.load("main.js").source, "main.js")

      val exports = runtime.getModuleExports("main.js").get
      assertEquals(exports.get("sum"), JSValue.Int32(6))
    }
  }

  test("module top-level this is undefined") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      evalModule(
        "export const captured = this;",
        "this.js"
      )

      val exports = runtime.getModuleExports("this.js").get
      assertEquals(exports.get("captured"), JSValue.Undefined)
    }
  }

  test("module without loader throws error") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx

      val source = """
        import { foo } from "nonexistent.js";
      """

      intercept[JSException] {
        evalModule(source, "test.js")
      }
    }
  }
