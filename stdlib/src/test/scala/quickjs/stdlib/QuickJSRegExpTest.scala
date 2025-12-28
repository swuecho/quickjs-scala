package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSException, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class QuickJSRegExpTest extends FunSuite:

  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode { compiler.compileScript(ast) }
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("RegExp basics: test, match, replace, split") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(summon[JSContext])

    val result =
      try eval("""
      |(function() {
      |  function assertEq(a, b, msg) {
      |    if (a !== b) throw Error(msg + ": got " + a + " expected " + b);
      |  }
      |
      |  var r = new RegExp("a");
      |  assertEq(r.test("cat"), true, "test");
      |
      |  var m1 = "cat".match(/a/);
      |  assertEq(m1[0], "a", "match");
      |
      |  var m2 = "a1a2".match(/a/g);
      |  assertEq(m2.length, 2, "match global length");
      |  assertEq(m2[0], "a", "match global 0");
      |  assertEq(m2[1], "a", "match global 1");
      |
      |  assertEq("a1a2".replace(new RegExp("a", "g"), "b"), "b1b2", "replace global");
      |  assertEq("a1a2".replace(new RegExp("a"), "$&"), "a1a2", "replace $&");
      |  assertEq("a1a2".replace(new RegExp("(a)"), "$1"), "a1a2", "replace $1");
      |  assertEq("a1a2".replace(new RegExp("(a)"), "b"), "b1a2", "replace first");
      |
      |  var s = "a1a2".split(new RegExp("(1)"));
      |  assertEq(s[0], "a", "split 0");
      |  assertEq(s[1], "1", "split capture");
      |  assertEq(s[2], "a2", "split 2");
      |
      |  var g = new RegExp("a", "g");
      |  var e1 = g.exec("a1a");
      |  assertEq(e1[0], "a", "exec 0");
      |  assertEq(e1.index, 0, "exec index");
      |  assertEq(e1.input, "a1a", "exec input");
      |  assertEq(e1.groups, undefined, "exec groups");
      |  assertEq(g.lastIndex, 1, "lastIndex after first");
      |  var e2 = g.exec("a1a");
      |  assertEq(e2[0], "a", "exec 1");
      |  assertEq(g.lastIndex, 3, "lastIndex after second");
      |  var e3 = g.exec("a1a");
      |  assertEq(e3, null, "exec null");
      |  assertEq(g.lastIndex, 0, "lastIndex reset");
      |  assertEq(g.source, "a", "source");
      |  assertEq(g.flags, "g", "flags");
      |  assertEq(g.global, true, "global");
      |  assertEq(g.toString(), "/a/g", "toString");
      |
      |  assertEq("a1a2".search(/a/), 0, "search regex");
      |  assertEq("a1a2".search("1"), 1, "search string");
      |
      |  var all = "a1a2".matchAll(/a/g);
      |  assertEq(all.length, 2, "matchAll length");
      |  assertEq(all[0][0], "a", "matchAll 0");
      |  assertEq(all[1][0], "a", "matchAll 1");
      |  assertEq(all[0].index, 0, "matchAll index");
      |  assertEq(all[0].groups, undefined, "matchAll groups");
      |
      |  var m3 = "a1a2".match(/a/);
      |  assertEq(m3.index, 0, "match index");
      |  assertEq(m3.input, "a1a2", "match input");
      |  assertEq(m3.groups, undefined, "match groups");
      |
      |  var bad = false;
      |  try { new RegExp("a", "gg"); } catch (e) { bad = (e instanceof SyntaxError); }
      |  assertEq(bad, true, "duplicate flags");
      |  bad = false;
      |  try { new RegExp("a", "z"); } catch (e) { bad = (e instanceof SyntaxError); }
      |  assertEq(bad, true, "invalid flags");
      |
      |  return true;
      |})()
      |""".stripMargin)
      catch
        case ex: JSException =>
          val detail = ex.getValue match
            case JSValue.Object(obj) =>
              val msg = obj.get("message").toString
              val stack = obj.get("stack").toString
              s"message=$msg stack=$stack"
            case other =>
              s"value=$other"
          fail(s"JavaScript exception in test: $detail")

    assertEquals(result, JSValue.fromBoolean(true))
  }
