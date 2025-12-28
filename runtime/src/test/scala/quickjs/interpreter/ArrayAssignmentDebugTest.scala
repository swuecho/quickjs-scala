package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.objmodel.JSArray
import scala.collection.mutable
import munit.*

class ArrayAssignmentDebugTest extends FunSuite {

  test("debug: JSArray.set works directly") {
    // Test that JSArray.set works correctly
    val elements = mutable.ArrayBuffer[JSValue](JSValue.fromInt(1), JSValue.fromInt(2), JSValue.fromInt(3))
    val arr = new JSArray(elements, mutable.LinkedHashMap.empty, 3)

    println(s"\n=== Direct JSArray.set test ===")
    println(s"  Initial: arr.get(0) = ${arr.get(0)}")
    arr.set(0, JSValue.fromInt(10))
    println(s"  After arr.set(0, 10): arr.get(0) = ${arr.get(0)}")
    assertEquals(arr.get(0), JSValue.fromInt(10), "arr.set updates index 0")
  }

  test("debug: array object identity in global scope") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create array and store in global
    val source1 = "var arr = [1, 2, 3]"
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    val interpreter = Interpreter()
    interpreter.call(bytecode1, JSValue.Undefined, Array.empty)

    // Get the array from global scope
    val ctx = summon[JSContext]
    val globalArrOpt = ctx.globalScope.getVariable("arr")
    println(s"\n=== Array object identity test ===")
    println(s"  After 'var arr = [1, 2, 3]'")
    println(s"  Global['arr'] = $globalArrOpt")

    globalArrOpt match
      case Some(JSValue.JSArrayVal(arr)) =>
        println(s"  arr.get(0) = ${arr.get(0)}")
        println(s"  Calling arr.set(0, JSValue.fromInt(10))")
        arr.set(0, JSValue.fromInt(10))
        println(s"  After set: arr.get(0) = ${arr.get(0)}")
      case _ =>
        println(s"  ERROR: Not an array or not found!")

    // Now access arr[0] from JavaScript
    val source2 = "arr[0]"
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.withREPLMode { compiler2.compileScript(ast2) }
    val result = interpreter.call(bytecode2, JSValue.Undefined, Array.empty)
    println(s"\n  After accessing 'arr[0]' from JavaScript:")
    println(s"  Result = $result")
    println(s"  Expected = ${JSValue.fromInt(10)}")
  }

  test("debug: SetElem opcode with debug output") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create array first
    val source1 = "var arr = [1, 2, 3]"
    val lexer1 = Lexer(source1)
    val parser1 = Parser(lexer1.tokenize())
    val ast1 = parser1.parseScript()
    val compiler1 = Compiler()
    val bytecode1 = compiler1.compileScript(ast1)
    val interpreter = Interpreter()
    interpreter.call(bytecode1, JSValue.Undefined, Array.empty)

    println(s"\n=== SetElem debug test ===")
    println(s"  Created array: var arr = [1, 2, 3]")

    // Manually get the array and check it
    val ctx = summon[JSContext]
    val globalArr1 = ctx.globalScope.getVariable("arr")
    globalArr1 match
      case Some(JSValue.JSArrayVal(arr)) =>
        println(s"  Before JS: arr.get(0) = ${arr.get(0)}")
      case _ => ()

    // Now assign using JavaScript
    val source2 = "arr[0] = 10"
    val lexer2 = Lexer(source2)
    val parser2 = Parser(lexer2.tokenize())
    val ast2 = parser2.parseScript()
    val compiler2 = Compiler()
    val bytecode2 = compiler2.compileScript(ast2)
    interpreter.call(bytecode2, JSValue.Undefined, Array.empty)

    // Check array again
    val globalArr2 = ctx.globalScope.getVariable("arr")
    globalArr2 match
      case Some(JSValue.JSArrayVal(arr)) =>
        println(s"  After 'arr[0] = 10': arr.get(0) = ${arr.get(0)}")
        println(s"  Expected: 10")
      case _ => ()
  }
}
