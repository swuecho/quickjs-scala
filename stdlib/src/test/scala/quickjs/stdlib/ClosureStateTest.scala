package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class ClosureStateTest extends FunSuite:

  test("debug: simple increment") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Simpler test: just define the counter function, don't call it yet
    val source = """
      function makeCounter() {
        var n = 0;
        return function() { ++n; return n; };
      }
      var counter = makeCounter();
    """
    val lexer = Lexer(source.stripMargin)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute to create the counter function
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Check the counter function
    val ctx = summon[JSContext]
    val funcObjOpt = ctx.globalScope.getVariable("counter")
    println(s"Counter function: $funcObjOpt")
    funcObjOpt match {
      case Some(f: JSValue.Function) =>
        println(s"Function closure: ${f.closure}")
        println(s"Closure keys: ${f.closure.keys.mkString(", ")}")
        f.closure.get("n") match {
          case Some(varRef) =>
            println(s"Variable 'n' in closure: $varRef")
            println(s"Variable 'n' value BEFORE call: ${varRef.get}")

            // Now call counter() and check again
            val callSource = "counter()"
            val callLexer = Lexer(callSource)
            val callTokens = callLexer.tokenize()
            val callParser = Parser(callTokens)
            val callAST = callParser.parseScript()
            val callBytecode = compiler.withREPLMode { compiler.compileScript(callAST) }

            val result = interpreter.call(callBytecode, JSValue.Undefined, Array.empty)
            println(s"Result: $result")

            // Check n value after call
            println(s"Variable 'n' value AFTER call: ${varRef.get}")
          case None =>
            println(s"Variable 'n' NOT found in closure!")
        }
      case Some(other) =>
        println(s"Not a function: $other")
      case None =>
        println(s"Variable 'counter' not found!")
    }
  }

  test("debug: assignment n = n + 5") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Test assignment expression with closure variable
    val source = """
      function makeAdder() {
        var n = 0;
        return function() { n = n + 5; return n; };
      }
      var adder = makeAdder();
    """
    val lexer = Lexer(source.stripMargin)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()

    val compiler = Compiler()
    val bytecode = compiler.compileScript(ast)

    // Execute to create the adder function
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

    // Check the adder function
    val ctx = summon[JSContext]
    val funcObjOpt = ctx.globalScope.getVariable("adder")
    println(s"Adder function: $funcObjOpt")
    funcObjOpt match {
      case Some(f: JSValue.Function) =>
        println(s"Function closure: ${f.closure}")
        println(s"Closure keys: ${f.closure.keys.mkString(", ")}")
        f.closure.get("n") match {
          case Some(varRef) =>
            println(s"Variable 'n' in closure: $varRef")
            println(s"Variable 'n' value BEFORE call: ${varRef.get}")

            // Now call adder() and check again
            val callSource = "adder()"
            val callLexer = Lexer(callSource)
            val callTokens = callLexer.tokenize()
            val callParser = Parser(callTokens)
            val callAST = callParser.parseScript()
            val callBytecode = compiler.withREPLMode { compiler.compileScript(callAST) }

            val result = interpreter.call(callBytecode, JSValue.Undefined, Array.empty)
            println(s"Result: $result, expected: 5")

            // Check n value after call
            println(s"Variable 'n' value AFTER call: ${varRef.get}, expected: 5")
          case None =>
            println(s"Variable 'n' NOT found in closure!")
        }
      case Some(other) =>
        println(s"Not a function: $other")
      case None =>
        println(s"Variable 'adder' not found!")
    }
  }

