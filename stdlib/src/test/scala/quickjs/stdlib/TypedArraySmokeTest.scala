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

  test("TypedArray indexed access via brackets") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var ta = new Int32Array(5)")
    eval("ta[0] = 10; ta[1] = 20; ta[2] = 30")
    assertEquals(eval("ta[0]"), JSValue.fromInt(10))
    assertEquals(eval("ta[1]"), JSValue.fromInt(20))
    assertEquals(eval("ta[2]"), JSValue.fromInt(30))
    assertEquals(eval("ta[99]"), JSValue.Undefined)
  }

  test("Float64Array indexed access") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var f64 = new Float64Array(2)")
    eval("f64[0] = 3.14; f64[1] = -2.5")
    val v0 = eval("f64[0]").toNumber
    val v1 = eval("f64[1]").toNumber
    assert(Math.abs(v0 - 3.14) < 0.001, s"expected ~3.14 got $v0")
    assert(Math.abs(v1 - (-2.5)) < 0.001, s"expected ~-2.5 got $v1")
  }

  test("TypedArray subarray shares buffer") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    eval("var u8 = new Uint8Array(10)")
    eval("u8[5] = 99")
    eval("var sub = u8.subarray(4, 8)")
    assertEquals(eval("sub[0]"), JSValue.fromInt(0))
    assertEquals(eval("sub[1]"), JSValue.fromInt(99))
    assertEquals(eval("sub.length"), JSValue.fromInt(4))
  }

  test("TypedArray iterator values keys entries and Symbol.iterator") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var ta = new Uint8Array(2);
      |ta[0] = 7;
      |ta[1] = 9;
      |var values = ta.values();
      |var keys = ta.keys();
      |var entries = ta.entries();
      |var symIter = ta[Symbol.iterator]();
      |var entry = entries.next().value;
      |values.next().value === 7 &&
      |  values.next().value === 9 &&
      |  values.next().done === true &&
      |  keys.next().value === 0 &&
      |  keys.next().value === 1 &&
      |  entry[0] === 0 &&
      |  entry[1] === 7 &&
      |  symIter.next().value === 7 &&
      |  symIter[Symbol.iterator]() === symIter;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray iterator methods are inherited and non-enumerable") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var ta = new Int16Array(1);
      |typeof ta.values === 'function' &&
      |  typeof ta.keys === 'function' &&
      |  typeof ta.entries === 'function' &&
      |  typeof ta[Symbol.iterator] === 'function' &&
      |  Object.keys(Object.getPrototypeOf(Object.getPrototypeOf(ta))).indexOf('values') === -1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray.from consumes arrays iterators strings and typed arrays") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var fromArrayIterator = Uint8Array.from([3, 4].values());
      |var fromString = Uint8Array.from("56");
      |var source = new Uint8Array(2);
      |source[0] = 7;
      |source[1] = 8;
      |var fromTypedArray = Uint8Array.from(source);
      |fromArrayIterator.length === 2 &&
      |  fromArrayIterator[0] === 3 &&
      |  fromArrayIterator[1] === 4 &&
      |  fromString[0] === 5 &&
      |  fromString[1] === 6 &&
      |  fromTypedArray[0] === 7 &&
      |  fromTypedArray[1] === 8;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray.from maps values with thisArg and index") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var receiver = { offset: 10 };
      |var indexes = "";
      |var out = Uint8Array.from([1, 2], function(value, index) {
      |  indexes = indexes + index;
      |  return this.offset + value;
      |}, receiver);
      |out.length === 2 &&
      |  out[0] === 11 &&
      |  out[1] === 12 &&
      |  indexes === "01";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray.from rejects invalid sources mapper and iterator results") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var nullRejected = false;
      |var mapperRejected = false;
      |var iteratorRejected = false;
      |var nextRejected = false;
      |var resultRejected = false;
      |try { Uint8Array.from(null); } catch (e) { nullRejected = e instanceof TypeError; }
      |try { Uint8Array.from([1], 1); } catch (e) { mapperRejected = e instanceof TypeError; }
      |var badIterator = {};
      |Object.defineProperty(badIterator, Symbol.iterator, { value: 1 });
      |try { Uint8Array.from(badIterator); } catch (e) { iteratorRejected = e instanceof TypeError; }
      |var badNext = {};
      |Object.defineProperty(badNext, Symbol.iterator, {
      |  value: function() { return { next: 1 }; }
      |});
      |try { Uint8Array.from(badNext); } catch (e) { nextRejected = e instanceof TypeError; }
      |var badResult = {};
      |Object.defineProperty(badResult, Symbol.iterator, {
      |  value: function() { return { next: function() { return 1; } }; }
      |});
      |try { Uint8Array.from(badResult); } catch (e) { resultRejected = e instanceof TypeError; }
      |nullRejected && mapperRejected && iteratorRejected && nextRejected && resultRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
