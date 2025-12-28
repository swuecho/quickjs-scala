package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

import scala.io.Source
import scala.util.control.NonFatal

class QuickJSLanguageIsolationTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("isolate first failing test_language function") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)
    JSON.initialize()

    val path = "stdlib/src/test/resources/quickjs-tests/test_language.js"
    val source = Source.fromFile(path).mkString
    val lines = source.split("\n").toIndexedSeq
    val filtered = lines.filterNot(_.trim.startsWith("test_")).mkString("\n")
    eval(filtered)

    val calls = Seq(
      "test_op1();",
      "test_cvt();",
      "test_eq();",
      "test_inc_dec();",
      "test_op2();",
      "test_constructor();",
      "test_delete();",
      "test_prototype();",
      "test_arguments();",
      "test_class();",
      "test_template();",
      "test_template_skip();",
      "test_object_literal();",
      "test_regexp_skip();",
      "test_labels();",
      "test_labels2();",
      "test_destructuring();",
      "test_spread();",
      "test_function_length();",
      "test_argument_scope();",
      "test_function_expr_name();",
      "test_parse_semicolon();",
      "test_optional_chaining();",
      "test_parse_arrow_function();",
      "test_unicode_ident();",
      "test_global_var_opt();"
    )

    var failed: String | Null = null
    for call <- calls if failed == null do
      try
        if call == "test_class();" then
          eval("""
            var __assertIndex = 0;
            var __assert = assert;
            assert = function(a, b, m) {
              __assertIndex++;
              if (arguments.length == 1) b = true;
              if (Object.is(a, b)) return;
              throw Error("assert#" + __assertIndex + " got |" + a + "| expected |" + b + "|" +
                          (m ? " (" + m + ")" : ""));
            };
          """)
          val debugResult = eval("var E1 = class E { static F() { return E; } }; E1.F();")
          val debugEq = eval("var E1 = class E { static F() { return E; } }; E1 === E1.F();")
          val debugGlobalEq = eval("var E1 = class E { static F() { return E; } }; E1 === E;")
          println(s"DEBUG class expr E1.F(): $debugResult")
          println(s"DEBUG class expr E1 === E1.F(): $debugEq")
          println(s"DEBUG class expr E1 === E: $debugGlobalEq")
        eval(call)
      catch
        case NonFatal(e) =>
          failed = call
          val msg = e match
            case je: quickjs.runtime.JSException =>
              je.getValue match
                case JSValue.Object(obj) =>
                  obj.get("message")(using ctx) match
                    case JSValue.JSStr(s) => s"JavaScript exception: $s"
                    case _ => je.getMessage
                case _ => je.getMessage
            case _ =>
              e.getMessage
          println(s"First failing call: $call -> $msg")
          e match
            case je: quickjs.runtime.JSException =>
              je.getValue match
                case JSValue.Object(obj) =>
                  obj.get("stack")(using ctx) match
                    case JSValue.JSStr(stack) => println(stack)
                    case _ => ()
                case _ => ()
            case _ => ()

    assertEquals(failed, null)
  }
