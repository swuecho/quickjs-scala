package quickjs.parser

import quickjs.lexer.Lexer
import quickjs.ast.{
  FunctionDeclaration,
  ArrowFunctionExpression,
  VariableDeclaration
}
import munit.*

class StrictModeParserTest extends FunSuite:

  test("parseScript: non-strict script has strict=false") {
    val source = "var x = 1;"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assert(!script.strict, "Non-strict script should have strict=false")
  }

  test("parseScript: script with 'use strict' directive has strict=true") {
    val source = """"use strict"; var x = 1;"""
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assert(script.strict, "Script with 'use strict' should have strict=true")
    // Directive should be removed from body
    assertEquals(script.body.size, 1)
  }

  test(
    "parseScript: script with \"use strict\" (double quotes) directive has strict=true"
  ) {
    val source = """"use strict"; var x = 1;"""
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    assert(script.strict, "Script with \"use strict\" should have strict=true")
  }

  test("parseScript: function with 'use strict' has strict=true") {
    val source = "function f() { 'use strict'; return 1; }"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    script.body.head match
      case funcDecl: FunctionDeclaration =>
        assert(
          funcDecl.strict,
          "Function with 'use strict' should have strict=true"
        )
        // Directive should be removed from body
        assertEquals(funcDecl.body.statements.size, 1)
      case _ => fail("Expected FunctionDeclaration")
  }

  test("parseScript: function without 'use strict' has strict=false") {
    val source = "function f() { return 1; }"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    script.body.head match
      case funcDecl: FunctionDeclaration =>
        assert(
          !funcDecl.strict,
          "Function without 'use strict' should have strict=false"
        )
      case _ => fail("Expected FunctionDeclaration")
  }

  test(
    "parseScript: arrow function with 'use strict' block body has strict=true"
  ) {
    val source = "const f = () => { 'use strict'; return 1; }"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    script.body.head match
      case varDecl: VariableDeclaration =>
        val init = varDecl.declarations.head.init
        assert(init != null, "Expected non-null init")
        init match
          case arrow: ArrowFunctionExpression =>
            assert(
              arrow.strict,
              "Arrow function with 'use strict' should have strict=true"
            )
          case _ => fail("Expected ArrowFunctionExpression")
      case _ => fail("Expected VariableDeclaration")
  }

  test("parseScript: arrow function with concise body has strict=false") {
    val source = "const f = () => 1"
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val script = parser.parseScript()

    script.body.head match
      case varDecl: VariableDeclaration =>
        val init = varDecl.declarations.head.init
        assert(init != null, "Expected non-null init")
        init match
          case arrow: ArrowFunctionExpression =>
            assert(
              !arrow.strict,
              "Arrow function with concise body should have strict=false"
            )
          case _ => fail("Expected ArrowFunctionExpression")
      case _ => fail("Expected VariableDeclaration")
  }
