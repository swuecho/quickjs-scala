package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class NestedArrayDebugTest extends FunSuite:

  test("debug: nested arrays") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    val source = "var arr = [[1, 2], [3, 4]]; arr[0]"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    println(s"Result: $result")
    println(s"Result class: ${result.getClass}")
    println(s"Result tag: ${result.tag}")
    println(s"Result isObject: ${result.isObject}")

    // Also check what's stored in the global scope
    summon[JSContext].globalScope.getVariable("arr") match
      case Some(JSValue.JSArrayVal(arr)) =>
        println(s"Global arr is array! Length: ${arr.length}")
        println(s"Global arr element 0: ${arr.get(0)}")
        println(s"Global arr element 0 class: ${arr.get(0).getClass}")
        println(s"Global arr element 1: ${arr.get(1)}")
      case Some(v) =>
        println(s"Global arr is not array: $v")
      case None =>
        println(s"Global arr not found")

    result match
      case JSValue.JSArrayVal(arr) =>
        println(s"Result is an array! Length: ${arr.length}")
        println(s"Element 0: ${arr.get(0)}")
        println(s"Element 1: ${arr.get(1)}")
      case _ =>
        println(s"Result not an array: $result")

    assert(result.isObject)
  }
