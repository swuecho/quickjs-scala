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

  test("ArrayBuffer transfer detaches source and preserves bytes") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var buffer = new ArrayBuffer(4);
      |var source = new Uint8Array(buffer);
      |source[0] = 10;
      |source[1] = 20;
      |source[2] = 30;
      |source[3] = 40;
      |var transferred = buffer.transfer();
      |var target = new Uint8Array(transferred);
      |buffer.detached === true &&
      |  buffer.byteLength === 0 &&
      |  source.length === 0 &&
      |  source.byteLength === 0 &&
      |  source.byteOffset === 0 &&
      |  source[0] === undefined &&
      |  transferred.detached === false &&
      |  transferred.byteLength === 4 &&
      |  target[0] === 10 &&
      |  target[1] === 20 &&
      |  target[2] === 30 &&
      |  target[3] === 40;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("ArrayBuffer transfer can resize and rejects detached source") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var buffer = new ArrayBuffer(2);
      |var source = new Uint8Array(buffer);
      |source[0] = 7;
      |source[1] = 8;
      |var grown = buffer.transferToFixedLength(4);
      |var grownView = new Uint8Array(grown);
      |var rejected = false;
      |try { buffer.transfer(); } catch (e) { rejected = e instanceof TypeError; }
      |var shrunkSource = new ArrayBuffer(4);
      |var shrunkView = new Uint8Array(shrunkSource);
      |shrunkView[0] = 1;
      |shrunkView[1] = 2;
      |shrunkView[2] = 3;
      |shrunkView[3] = 4;
      |var shrunk = shrunkSource.transfer(2);
      |var finalView = new Uint8Array(shrunk);
      |grown.byteLength === 4 &&
      |  grownView[0] === 7 &&
      |  grownView[1] === 8 &&
      |  grownView[2] === 0 &&
      |  grownView[3] === 0 &&
      |  rejected &&
      |  shrunk.byteLength === 2 &&
      |  finalView[0] === 1 &&
      |  finalView[1] === 2 &&
      |  shrunkSource.detached === true;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
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

  test("DataView honors littleEndian for numeric accessors") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var buffer = new ArrayBuffer(32);
      |var dv = new DataView(buffer);
      |dv.setUint16(0, 0x1234, true);
      |dv.setInt32(2, -2023406815, true); // 0x87654321
      |dv.setFloat32(6, 1.5, true);
      |dv.setFloat64(10, -2.25, true);
      |dv.setBigUint64(18, 0x0102030405060708n, true);
      |dv.setFloat16(26, 1, true);
      |dv.getUint8(0) === 0x34 &&
      |  dv.getUint8(1) === 0x12 &&
      |  dv.getUint16(0, true) === 0x1234 &&
      |  dv.getUint16(0, false) === 0x3412 &&
      |  dv.getInt32(2, true) === -2023406815 &&
      |  dv.getUint32(2, false) === 0x21436587 &&
      |  dv.getFloat32(6, true) === 1.5 &&
      |  dv.getFloat64(10, true) === -2.25 &&
      |  dv.getBigUint64(18, true) === 0x0102030405060708n &&
      |  dv.getFloat16(26, true) === 1;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("DataView accessors and methods reject detached buffers") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var buffer = new ArrayBuffer(8);
      |var dv = new DataView(buffer, 2, 4);
      |var sameBufferBeforeDetach = dv.buffer === buffer;
      |buffer.transfer();
      |var byteLengthRejected = false;
      |var byteOffsetRejected = false;
      |var getRejected = false;
      |var setRejected = false;
      |try { dv.byteLength; } catch (e) { byteLengthRejected = e instanceof TypeError; }
      |try { dv.byteOffset; } catch (e) { byteOffsetRejected = e instanceof TypeError; }
      |try { dv.getUint8(0); } catch (e) { getRejected = e instanceof TypeError; }
      |try { dv.setUint8(0, 1); } catch (e) { setRejected = e instanceof TypeError; }
      |sameBufferBeforeDetach &&
      |  dv.buffer === buffer &&
      |  buffer.detached === true &&
      |  byteLengthRejected &&
      |  byteOffsetRejected &&
      |  getRejected &&
      |  setRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
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

  test("TypedArray prototype mutating and search methods") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var ta = new Uint8Array([1, 2, 3, 4]);
      |var fillReturn = ta.fill(9, 1, -1);
      |var afterFill = ta[0] === 1 && ta[1] === 9 && ta[2] === 9 && ta[3] === 4;
      |var copyReturn = ta.copyWithin(1, 2);
      |var afterCopy = ta[0] === 1 && ta[1] === 9 && ta[2] === 4 && ta[3] === 4;
      |var reverseReturn = ta.reverse();
      |afterFill &&
      |  afterCopy &&
      |  fillReturn === ta &&
      |  copyReturn === ta &&
      |  reverseReturn === ta &&
      |  ta[0] === 4 && ta[1] === 4 && ta[2] === 9 && ta[3] === 1 &&
      |  ta.at(-1) === 1 &&
      |  ta.at(99) === undefined &&
      |  ta.includes(9) === true &&
      |  ta.indexOf(4) === 0 &&
      |  ta.lastIndexOf(4) === 1 &&
      |  ta.join("-") === "4-4-9-1" &&
      |  ta.toLocaleString() === "4,4,9,1";
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray includes uses SameValueZero while indexOf uses strict equality") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var floats = new Float64Array([1, NaN, -0]);
      |floats.includes(NaN) === true &&
      |  floats.indexOf(NaN) === -1 &&
      |  floats.includes(0) === true &&
      |  floats.indexOf(0) === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray copy-by-change methods with and toReversed") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var source = new Int16Array([1, 2, 3]);
      |var changed = source.with(-1, 9);
      |var reversed = source.toReversed();
      |var rangeRejected = false;
      |try { source.with(3, 10); } catch (e) { rangeRejected = e instanceof RangeError; }
      |changed instanceof Int16Array &&
      |  reversed instanceof Int16Array &&
      |  changed !== source &&
      |  reversed !== source &&
      |  source[0] === 1 && source[1] === 2 && source[2] === 3 &&
      |  changed[0] === 1 && changed[1] === 2 && changed[2] === 9 &&
      |  reversed[0] === 3 && reversed[1] === 2 && reversed[2] === 1 &&
      |  rangeRejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray sort and toSorted use numeric typed-array ordering") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var ints = new Int16Array([10, 2, -1, 4]);
      |var sortReturn = ints.sort();
      |var floats = new Float64Array([NaN, 3, -0, 0, -2]);
      |floats.sort();
      |sortReturn === ints &&
      |  ints[0] === -1 && ints[1] === 2 && ints[2] === 4 && ints[3] === 10 &&
      |  floats[0] === -2 &&
      |  1 / floats[1] === -Infinity &&
      |  1 / floats[2] === Infinity &&
      |  floats[3] === 3 &&
      |  floats.includes(NaN);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray sort accepts comparator and toSorted leaves source unchanged") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var source = new Uint8Array([1, 3, 2]);
      |var sorted = source.toSorted(function(a, b) { return b - a; });
      |var rejected = false;
      |try { source.sort(1); } catch (e) { rejected = e instanceof TypeError; }
      |sorted instanceof Uint8Array &&
      |  sorted !== source &&
      |  source[0] === 1 && source[1] === 3 && source[2] === 2 &&
      |  sorted[0] === 3 && sorted[1] === 2 && sorted[2] === 1 &&
      |  rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray callback iteration methods use value index receiver and thisArg") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var ta = new Uint8Array([2, 4, 6]);
      |var receiver = { limit: 3, offset: 1 };
      |var seen = "";
      |var forEachResult = ta.forEach(function(value, index, array) {
      |  if (array === ta) seen = seen + index + ":" + (value + this.offset) + ";";
      |}, receiver);
      |var everyResult = ta.every(function(value) { return value > this.limit; }, receiver);
      |var someResult = ta.some(function(value) { return value === 4; });
      |var found = ta.find(function(value) { return value > 3; });
      |var foundIndex = ta.findIndex(function(value) { return value > 3; });
      |var foundLast = ta.findLast(function(value) { return value > 3; });
      |var foundLastIndex = ta.findLastIndex(function(value) { return value > 3; });
      |forEachResult === undefined &&
      |  seen === "0:3;1:5;2:7;" &&
      |  everyResult === false &&
      |  someResult === true &&
      |  found === 4 &&
      |  foundIndex === 1 &&
      |  foundLast === 6 &&
      |  foundLastIndex === 2;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("TypedArray map filter reduce and reduceRight") {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)

    val result = eval("""
      |var source = new Int16Array([1, 2, 3, 4]);
      |var mapped = source.map(function(value, index) { return value * 2 + index; });
      |var filtered = source.filter(function(value) { return value % 2 === 0; });
      |var sum = source.reduce(function(acc, value, index, array) {
      |  return acc + value + (array === source ? index : 100);
      |}, 0);
      |var right = source.reduceRight(function(acc, value) { return acc + "" + value; }, "");
      |var rejected = false;
      |try { source.map(1); } catch (e) { rejected = e instanceof TypeError; }
      |mapped instanceof Int16Array &&
      |  filtered instanceof Int16Array &&
      |  mapped[0] === 2 && mapped[1] === 5 && mapped[2] === 8 && mapped[3] === 11 &&
      |  filtered.length === 2 && filtered[0] === 2 && filtered[1] === 4 &&
      |  sum === 16 &&
      |  right === "4321" &&
      |  rejected;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }
