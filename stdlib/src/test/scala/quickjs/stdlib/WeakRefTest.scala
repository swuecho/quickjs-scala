package quickjs.stdlib

import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class WeakRefTest extends FunSuite:

  private def eval(source: String): JSValue =
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)

    val tokens = Lexer(source).tokenize()
    val ast = Parser(tokens).parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    Interpreter().call(bytecode, JSValue.Undefined, Array.empty)

  test("WeakRef constructor and deref") {
    val result = eval("""
      |var target = { value: 42 };
      |var ref = new WeakRef(target);
      |ref.deref() === target && ref.deref().value === 42;
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("WeakRef requires new and object or symbol target") {
    val result = eval("""
      |var ok = 0;
      |try { WeakRef({}); } catch (e) { ok += e instanceof TypeError ? 1 : 0; }
      |try { new WeakRef(1); } catch (e) { ok += e instanceof TypeError ? 2 : 0; }
      |try { new WeakRef(Symbol("x")); ok += 4; } catch (e) {}
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(7))
  }

  test("FinalizationRegistry constructor and unregister") {
    val result = eval("""
      |var calls = 0;
      |var registry = new FinalizationRegistry(function(value) { calls += value; });
      |var target = {};
      |var token = {};
      |registry.register(target, 10, token);
      |var first = registry.unregister(token);
      |var second = registry.unregister(token);
      |(first === true && second === false && calls === 0);
      |""".stripMargin)
    assertEquals(result, JSValue.Bool(true))
  }

  test("FinalizationRegistry validates callback target held value and token") {
    val result = eval("""
      |var ok = 0;
      |try { FinalizationRegistry(function() {}); } catch (e) { ok += e instanceof TypeError ? 1 : 0; }
      |try { new FinalizationRegistry(1); } catch (e) { ok += e instanceof TypeError ? 2 : 0; }
      |var registry = new FinalizationRegistry(function() {});
      |var target = {};
      |try { registry.register(1, "held"); } catch (e) { ok += e instanceof TypeError ? 4 : 0; }
      |try { registry.register(target, target); } catch (e) { ok += e instanceof TypeError ? 8 : 0; }
      |try { registry.register(target, "held", 1); } catch (e) { ok += e instanceof TypeError ? 16 : 0; }
      |try { registry.unregister(1); } catch (e) { ok += e instanceof TypeError ? 32 : 0; }
      |ok;
      |""".stripMargin)
    assertEquals(result, JSValue.fromInt(63))
  }
