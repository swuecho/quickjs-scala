package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSException, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.value.JSValue
import munit.*

class StrictModeTest extends FunSuite:

  def withContext(testCode: (JSRuntime, JSContext) => Unit): Unit =
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)
    StdLib.initialize(ctx)
    testCode(runtime, ctx)

  def eval(source: String)(using ctx: JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("strict mode: assignment to undeclared variable throws ReferenceError") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        "use strict";
        x = 1;
      """

      val exception = intercept[JSException] {
        eval(source)
      }
      // Get the underlying JS error object and check its message property
      val errorValue = exception.getValue
      errorValue match {
        case JSValue.Object(obj) =>
          val msg = obj.get("message")(using ctx)
          assertEquals(msg, JSValue.fromString("x is not defined"))
        case _ =>
          fail(s"Expected Error object but got: $errorValue")
      }
    }
  }

  test("non-strict mode: assignment to undeclared variable creates global") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        x = 1;
        x;
      """

      val result = eval(source)
      assertEquals(result, JSValue.Int32(1))
    }
  }

  test("strict mode: can assign to declared variable") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        "use strict";
        var x = 1;
        x = 2;
        x;
      """

      val result = eval(source)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test("strict mode: can assign to let-declared variable") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        "use strict";
        let x = 1;
        x = 2;
        x;
      """

      val result = eval(source)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test(
    "strict mode in function: assignment to undeclared variable throws ReferenceError"
  ) {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        function f() {
          "use strict";
          x = 1;
        }
        f();
      """

      val exception = intercept[JSException] {
        eval(source)
      }
      // Get the underlying JS error object and check its message property
      val errorValue = exception.getValue
      errorValue match {
        case JSValue.Object(obj) =>
          val msg = obj.get("message")(using ctx)
          assertEquals(msg, JSValue.fromString("x is not defined"))
        case _ =>
          fail(s"Expected Error object but got: $errorValue")
      }
    }
  }

  test("strict mode in function: can assign to declared variable") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        function f() {
          "use strict";
          var x = 1;
          x = 2;
          return x;
        }
        f();
      """

      val result = eval(source)
      assertEquals(result, JSValue.Int32(2))
    }
  }

  test("non-strict function: assignment to undeclared variable creates global") {
    withContext { (runtime, ctx) =>
      given JSContext = ctx
      val source = """
        function f() {
          x = 1;
        }
        f();
        x;
      """

      val result = eval(source)
      assertEquals(result, JSValue.Int32(1))
    }
  }
