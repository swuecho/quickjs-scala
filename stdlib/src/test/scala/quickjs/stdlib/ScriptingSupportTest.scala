package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.module.FileModuleLoader
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.FunSuite
import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.file.Files

/** Tests for the host/scripting surface: console output, timers, text codecs,
  * base64 helpers and ES module execution.
  */
class ScriptingSupportTest extends FunSuite:

  private def eval(source: String)(using ctx: JSContext): JSValue =
    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val bytecode = Compiler().compileScript(ast)
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  private def withContext(body: JSContext ?=> Unit): Unit = {
    given rt: JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(rt)
    StdLib.initialize(ctx)
    JSON.initialize()
    body
  }

  test("console.log omits the receiver and honors multiple arguments") {
    val out = new ByteArrayOutputStream()
    val previous = System.out
    System.setOut(new PrintStream(out))
    try
      withContext {
        Console.initialize()
        eval("""console.log("hello", 42, {a: 1});""")
      }
    finally System.setOut(previous)
    assertEquals(out.toString.trim, "hello 42 {a: 1}")
  }

  test("console.error writes to stderr") {
    val err = new ByteArrayOutputStream()
    val previous = System.err
    System.setErr(new PrintStream(err))
    try
      withContext {
        Console.initialize()
        eval("""console.error("bad", 1);""")
      }
    finally System.setErr(previous)
    assertEquals(err.toString.trim, "bad 1")
  }


  test("legacy escape/unescape globals round-trip") {
    withContext {
      Globals.initialize()
      val result = eval(
        """escape("a b~é") + "|" + unescape("a%20b%7E%E9")"""
      )
      assertEquals(
        result,
        JSValue.fromString("a%20b%7E%E9|a b~é")
      )
    }
  }

  test("base64 helpers round-trip and validate") {
    withContext {
      Globals.initialize()
      val result = eval("""btoa("hello") + "|" + atob("aGVsbG8=")""")
      assertEquals(result, JSValue.fromString("aGVsbG8=|hello"))
      val threw = eval("""
        |var threw = false;
        |try { atob("!!!"); } catch (e) { threw = e.name === "InvalidCharacterError"; }
        |threw;
        |""".stripMargin)
      assertEquals(threw, JSValue.Bool(true))
    }
  }

  test("TextEncoder/TextDecoder round-trip UTF-8") {
    withContext {
      Globals.initialize()
      val result = eval("""
        |var bytes = new TextEncoder().encode("héllo");
        |bytes.length + ":" + new TextDecoder().decode(bytes);
        |""".stripMargin)
      assertEquals(result, JSValue.fromString("6:héllo"))
    }
  }

  test("structuredClone copies nested data") {
    withContext {
      Globals.initialize()
      val result = eval("""
        |var src = { a: [1, { b: 2 }] };
        |var copy = structuredClone(src);
        |copy.a[1].b = 99;
        |src.a[1].b + "," + copy.a[1].b;
        |""".stripMargin)
      assertEquals(result, JSValue.fromString("2,99"))
    }
  }

  test("structuredClone covers Map, Set, typed arrays and ArrayBuffers") {
    withContext {
      Globals.initialize()
      val result = eval("""
        |var out = [];
        |var m = new Map([["a", { n: 1 }]]);
        |m.set("self", m);
        |var m2 = structuredClone(m);
        |m2.get("a").n = 9;
        |out.push(m.get("a").n + "," + m2.get("a").n + "," + (m2.get("self") === m2));
        |
        |var s = new Set([1, 2, [3]]);
        |var s2 = structuredClone(s);
        |s2.add(4);
        |out.push(s.size + "," + s2.size + "," + (s2.has(2) && !s.has(4)));
        |
        |var ta = new Uint8Array([1, 2, 3]);
        |var ta2 = structuredClone(ta);
        |ta2[0] = 9;
        |out.push(ta[0] + "," + ta2[0] + "," + (ta2.buffer !== ta.buffer));
        |
        |var ab = new ArrayBuffer(4);
        |new Uint8Array(ab)[0] = 7;
        |var ab2 = structuredClone(ab);
        |out.push(new Uint8Array(ab2)[0] + "," + (ab2.byteLength === 4) + "," + (ab2 !== ab));
        |
        |var dv = new DataView(new ArrayBuffer(2));
        |dv.setUint8(0, 42);
        |var dv2 = structuredClone(dv);
        |out.push(dv2.getUint8(0) + "," + (dv2.buffer !== dv.buffer));
        |
        |var sub = ta.subarray(1);
        |var sub2 = structuredClone(sub);
        |out.push(sub2[0] + "," + (sub2.byteOffset === 0) + "," + (sub2.buffer !== sub.buffer));
        |out.push(structuredClone(new Date(5)).getTime() + "," + structuredClone(/a/gi).flags);
        |out.push(structuredClone(new String("boxed")).valueOf());
        |
        |var threw = 0;
        |try { structuredClone(function () {}); } catch (e) { if (e.name === "DataCloneError") threw++; }
        |try { structuredClone(Symbol("x")); } catch (e) { if (e.name === "DataCloneError") threw++; }
        |try { structuredClone({ f: function () {} }); } catch (e) { if (e.name === "DataCloneError") threw++; }
        |try { structuredClone(new WeakMap()); } catch (e) { if (e.name === "DataCloneError") threw++; }
        |out.push(threw);
        |out.join("|");
        |""".stripMargin)
      assertEquals(
        result,
        JSValue.fromString(
          "1,9,true|3,4,true|1,9,true|7,true,true|42,true|2,true,true|5,gi|boxed|4"
        )
      )
    }
  }

  test("setTimeout runs in due order and intervals can be cleared") {
    withContext {
      val timers = Timers.newState()
      Timers.initialize(timers)
      eval("""
        |var log = [];
        |setTimeout(function () { log.push("second"); }, 20);
        |setTimeout(function () { log.push("first"); }, 1);
        |var count = 0;
        |var id = setInterval(function () {
        |  count++;
        |  if (count >= 2) clearInterval(id);
        |}, 1);
        |setTimeout(function () { log.push("count=" + count); }, 40);
        |""".stripMargin)
      Timers.runPending(timers)
      val result = eval("""log.join(",")""")
      assertEquals(result, JSValue.fromString("first,second,count=2"))
    }
  }

  test("setTimeout accepts a string body") {
    withContext {
      val timers = Timers.newState()
      Timers.initialize(timers)
      eval("""var fired = false; setTimeout('fired = true', 0);""")
      Timers.runPending(timers)
      assertEquals(eval("fired"), JSValue.Bool(true))
    }
  }

  test("ES modules support import, dynamic import and import.meta") {
    val dir = Files.createTempDirectory("qjs-mod")
    try {
      Files.writeString(dir.resolve("dep.mjs"), "export const value = 41;\n")
      Files.writeString(
        dir.resolve("main.mjs"),
        """import { value } from "./dep.mjs";
          |const dyn = await import("./dep.mjs");
          |globalThis.__result = `${value + 1}:${dyn.value}:${typeof import.meta.url}`;
          |""".stripMargin
      )
      given rt: JSRuntime = JSRuntime()
      given ctx: JSContext = JSContext(rt)
      StdLib.initialize(ctx)
      JSON.initialize()
      Console.initialize()
      rt.setModuleLoader(FileModuleLoader(dir))
      val source = Files.readString(dir.resolve("main.mjs"))
      val tokens = Lexer(source).tokenize()
      val ast = new Parser(tokens, moduleMode = true).parseScript()
      val bytecode = Compiler().compileModule(ast, dir.resolve("main.mjs").toString)
      Interpreter().call(bytecode, JSValue.Undefined, Array.empty)
      ctx.runMicrotasks()
      val result = ctx.global.get("__result")
      assertEquals(result, JSValue.fromString("42:41:string"))
    } finally {
      try
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.deleteIfExists(p))
      catch case _: Exception => ()
    }
  }

  test("named capture groups expose groups and $<name> replacement") {
    withContext {
      Console.initialize()
      val result = eval("""
        |var re = /(?<word>\w+)-(\d+)/;
        |var m = re.exec("abc-42");
        |m.groups.word + "|" + "abc-42".replace(re, "$<word>:$2");
        |""".stripMargin)
      assertEquals(result, JSValue.fromString("abc|abc:42"))
    }
  }
