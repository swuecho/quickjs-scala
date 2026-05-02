package quickjs.interpreter

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import quickjs.value.NativeFunction
import quickjs.objmodel.JSObject
import munit.*

class ThisBindingTest extends FunSuite:

  test("method call with 'this' binding - arr.push(1)") {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    // Manually initialize Array.push method
    val arrayObj = JSObject(prototype = ctx.arrayPrototype, extensible = true)
    val pushFunc = NativeFunction(
      "push",
      (args, context) =>
        if args.isEmpty then JSValue.fromInt(0)
        else
          // args(0) is 'this' (the array)
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

    val source = "var arr = [1, 2]; arr.push(3); arr"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result should be an array
    assert(result.isObject, s"Expected object but got: $result")

    result match
      case JSValue.JSArrayVal(arr) =>
        assertEquals(arr.getLength, 3)
        assertEquals(arr.get(0), JSValue.fromInt(1))
        assertEquals(arr.get(1), JSValue.fromInt(2))
        assertEquals(arr.get(2), JSValue.fromInt(3))
      case _ =>
        fail(s"Expected array but got: $result")
  }

  test("method call chain - arr.push(1).push(2)") {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    // Manually initialize Array.push method
    val arrayObj = JSObject(prototype = ctx.arrayPrototype, extensible = true)
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

    val source = "var arr = []; arr.push(1); arr.push(2); arr.length"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Result should be the array length (2)
    assertEquals(result, JSValue.fromInt(2))
  }

  test("custom object method with 'this'") {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    // Create an object with a method that uses 'this'
    val obj = JSObject(prototype = null, extensible = true)
    obj.set("x", JSValue.fromInt(10))

    val getXFunc = NativeFunction(
      "getX",
      (args, context) =>
        // args(0) is 'this'
        args(0) match
          case JSValue.Object(obj) =>
            obj.get("x")
          case _ =>
            JSValue.Undefined
    )
    obj.set("getX", JSValue.Native(getXFunc))

    ctx.global.set("obj", JSValue.Object(obj))

    val source = "obj.getX()"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(10))
  }

  test("regular function call still works") {
    given runtime: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(runtime)

    val source = "function add(a, b) { return a + b; } add(1, 2)"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))

    val interpreter = Interpreter()
    val result = interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    assertEquals(result, JSValue.fromInt(3))
  }
