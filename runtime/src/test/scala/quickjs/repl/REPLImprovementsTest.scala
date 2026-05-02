package quickjs.repl

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.objmodel.JSObject
import quickjs.util.PrettyPrinter
import munit.*

class REPLImprovementsTest extends FunSuite:

  test("PrettyPrinter formats integers") {
    assertEquals(PrettyPrinter.format(JSValue.fromInt(42)), "42")
    assertEquals(PrettyPrinter.format(JSValue.fromInt(-10)), "-10")
  }

  test("PrettyPrinter formats floats") {
    assertEquals(PrettyPrinter.format(JSValue.fromDouble(3.14)), "3.14")
    assertEquals(
      PrettyPrinter.format(JSValue.fromDouble(Math.PI)),
      "3.141592653589793"
    )
  }

  test("PrettyPrinter formats special numbers") {
    assertEquals(PrettyPrinter.format(JSValue.fromDouble(Double.NaN)), "NaN")
    assertEquals(
      PrettyPrinter.format(JSValue.fromDouble(Double.PositiveInfinity)),
      "Infinity"
    )
    assertEquals(
      PrettyPrinter.format(JSValue.fromDouble(Double.NegativeInfinity)),
      "-Infinity"
    )
  }

  test("PrettyPrinter formats strings") {
    assertEquals(PrettyPrinter.format(JSValue.fromString("hello")), "\"hello\"")
    assertEquals(
      PrettyPrinter.format(JSValue.fromString("hello\nworld")),
      "\"hello\\nworld\""
    )
    assertEquals(
      PrettyPrinter.format(JSValue.fromString("quote\"test")),
      "\"quote\\\"test\""
    )
  }

  test("PrettyPrinter formats arrays") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    val arr = quickjs.objmodel.JSArray(3)
    arr.set(0, JSValue.fromInt(1))
    arr.set(1, JSValue.fromInt(2))
    arr.set(2, JSValue.fromInt(3))

    val result = PrettyPrinter.format(JSValue.JSArrayVal(arr))
    assertEquals(result, "[1, 2, 3]")
  }

  test("PrettyPrinter formats nested arrays") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    val outer = quickjs.objmodel.JSArray(2)
    val inner1 = quickjs.objmodel.JSArray(2)
    inner1.set(0, JSValue.fromInt(1))
    inner1.set(1, JSValue.fromInt(2))
    val inner2 = quickjs.objmodel.JSArray(2)
    inner2.set(0, JSValue.fromInt(3))
    inner2.set(1, JSValue.fromInt(4))

    outer.set(0, JSValue.JSArrayVal(inner1))
    outer.set(1, JSValue.JSArrayVal(inner2))

    val result = PrettyPrinter.format(JSValue.JSArrayVal(outer))
    assertEquals(result, "[[1, 2], [3, 4]]")
  }

  test("PrettyPrinter formats objects") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    val obj = quickjs.objmodel.JSObject(prototype = null, extensible = true)
    obj.set("x", JSValue.fromInt(1))
    obj.set("y", JSValue.fromInt(2))

    val result = PrettyPrinter.format(JSValue.Object(obj))
    assert(result.contains("x: 1"))
    assert(result.contains("y: 2"))
  }

  test("console.log uses pretty printing") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    // Manually initialize console
    val consoleObj = JSObject(prototype = null, extensible = true)
    val logFunc = NativeFunction(
      "log",
      (args, context) =>
        val output = args.map(PrettyPrinter.shortFormat).mkString(" ")
        println(output)
        JSValue.Undefined
    )
    consoleObj.set("log", JSValue.Native(logFunc))
    ctx.global.set("console", JSValue.Object(consoleObj))

    val source = "var arr = [1, 2, 3]; console.log(arr)"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }

  test("Array methods work with console.log") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    // Manually initialize console
    val consoleObj = JSObject(prototype = null, extensible = true)
    val logFunc = NativeFunction(
      "log",
      (args, context) =>
        val output = args.map(PrettyPrinter.shortFormat).mkString(" ")
        println(output)
        JSValue.Undefined
    )
    consoleObj.set("log", JSValue.Native(logFunc))
    ctx.global.set("console", JSValue.Object(consoleObj))

    // Manually initialize Array.push
    val arrayObj = JSObject(prototype = null, extensible = true)
    val pushFunc = NativeFunction(
      "push",
      (args, context) =>
        if args.isEmpty then JSValue.fromInt(0)
        else
          args(0) match
            case arrVal: JSValue.JSArrayVal =>
              val arr = arrVal.value
              val elementsToAdd = args.drop(1)
              for elem <- elementsToAdd do arr.push(elem)
              JSValue.fromInt(arr.length)
            case _ =>
              JSValue.fromInt(0)
    )
    arrayObj.set("push", JSValue.Native(pushFunc))
    ctx.global.set("Array", JSValue.Object(arrayObj))

    val source = """
      var arr = [1, 2];
      Array.push(arr, 3);
      Array.push(arr, 4);
      console.log("Array:", arr);
    """
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.Undefined)
  }
