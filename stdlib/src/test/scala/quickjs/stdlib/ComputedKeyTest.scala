package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime, StdLib}
import quickjs.value.JSValue
import munit.*

class ComputedKeyTest extends FunSuite:
  test("computed keys convert via ToPropertyKey") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])
    StdLib.initialize(ctx)
    val src = """
      |var q = {};
      |var k2 = function () {};
      |q[k2] = 7;
      |if (q[k2] !== 7) throw new Error('same object key mismatch');
      |var keys = Object.keys(q);
      |if (keys.length !== 1) throw new Error('keys length: ' + keys.length);
      |if (keys[0].indexOf('function') !== 0) throw new Error('key text: ' + keys[0]);
      |var o2 = {};
      |o2[() => {}] = 1;
      |if (o2[() => {}] !== 1) throw new Error('arrow key mismatch');
      |var b = {}; b[true] = 't'; b[1n] = 'one'; b[null] = 'n';
      |if (b[true] !== 't' || b[1n] !== 'one' || b[null] !== 'n') throw new Error('primitive keys');
      |var C = class { [() => { }]() { return 1; } static [() => { }]() { return 1; } };
      |var c = new C();
      |if (c[() => { }]() !== 1) throw new Error('class arrow key');
      |if (C[() => { }]() !== 1) throw new Error('static class arrow key');
      |"ok"
      |""".stripMargin
    val tokens = Lexer(src).tokenize()
    val ast = Parser(tokens).parseScript()
    val bc = Compiler().compileScript(ast)
    println(Interpreter().call(bc, JSValue.Undefined, Array.empty))
  }
