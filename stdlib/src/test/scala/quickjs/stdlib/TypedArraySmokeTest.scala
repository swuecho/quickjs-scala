package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.runtime.StdLib
import quickjs.runtime.builtins.TypedArrayBuiltins
import quickjs.value.JSValue
import munit.*

class TypedArraySmokeTest extends FunSuite:

  def eval(source: String)(using ctx: JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("ArrayBuffer construction and byteLength") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    assertEquals(eval("new ArrayBuffer(16).byteLength"), JSValue.fromInt(16))
  }

  test("Uint8Array basic operations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var u8 = new Uint8Array(4)")
    eval("u8[0] = 10; u8[1] = 20; u8[2] = 30; u8[3] = 40")
    assertEquals(eval("u8[0]"), JSValue.fromInt(10))
    assertEquals(eval("u8[1]"), JSValue.fromInt(20))
    assertEquals(eval("u8.length"), JSValue.fromInt(4))
    assertEquals(eval("u8.byteLength"), JSValue.fromInt(4))
  }

  test("Int32Array basic operations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var i32 = new Int32Array(2)")
    eval("i32[0] = 123456; i32[1] = -789012")
    assertEquals(eval("i32[0]"), JSValue.fromInt(123456))
    assertEquals(eval("i32[1]"), JSValue.fromInt(-789012))
    assertEquals(eval("i32.length"), JSValue.fromInt(2))
    assertEquals(eval("i32.byteLength"), JSValue.fromInt(8))
  }

  test("Float64Array basic operations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var f64 = new Float64Array(2)")
    eval("f64[0] = 3.14159")
    val result = eval("f64[0]").toNumber
    assert(Math.abs(result - 3.14159) < 0.001)
  }

  test("ArrayBuffer.isView") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var u8 = new Uint8Array(4)")
    eval("var ab = new ArrayBuffer(8)")
    assertEquals(eval("ArrayBuffer.isView(u8)"), JSValue.Bool(true))
    assertEquals(eval("ArrayBuffer.isView(ab)"), JSValue.Bool(false))
  }

  test("DataView basic operations") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var dv = new DataView(new ArrayBuffer(8))")
    eval("dv.setInt32(0, 0x01020304)")
    assertEquals(eval("dv.getInt8(0)"), JSValue.fromInt(1))
    assertEquals(eval("dv.getUint16(2)"), JSValue.fromInt(772))
  }

  test("All TypedArray types construct and report correct length") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val types = List(
      ("Int8Array", 1),
      ("Uint8Array", 1),
      ("Uint8ClampedArray", 1),
      ("Int16Array", 2),
      ("Uint16Array", 2),
      ("Int32Array", 4),
      ("Uint32Array", 4),
      ("Float32Array", 4),
      ("Float64Array", 8)
    )
    for ((name, bpe) <- types) {
      val len = eval(s"new $name(5).length")
      assertEquals(len, JSValue.fromInt(5), s"$name(5).length")
      val byteLen = eval(s"new $name(5).byteLength")
      assertEquals(byteLen, JSValue.fromInt(5 * bpe), s"$name(5).byteLength")
    }
  }

  test("TypedArray BYTES_PER_ELEMENT") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    assertEquals(eval("Int8Array.BYTES_PER_ELEMENT"), JSValue.fromInt(1))
    assertEquals(eval("Int32Array.BYTES_PER_ELEMENT"), JSValue.fromInt(4))
    assertEquals(eval("Float64Array.BYTES_PER_ELEMENT"), JSValue.fromInt(8))
  }
