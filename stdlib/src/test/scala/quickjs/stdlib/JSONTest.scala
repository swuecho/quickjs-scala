package quickjs.stdlib

import quickjs.lexer.Lexer
import quickjs.parser.Parser
import quickjs.compiler.Compiler
import quickjs.interpreter.Interpreter
import quickjs.runtime.{JSContext, JSRuntime}
import quickjs.value.JSValue
import munit.*

class JSONTest extends FunSuite:

  /** Helper to evaluate JavaScript code and return result */
  private def eval(source: String)(using JSContext): JSValue =
    val lexer = Lexer(source)
    val tokens = lexer.tokenize()
    val parser = Parser(tokens)
    val ast = parser.parseScript()
    val compiler = Compiler()
    val bytecode = compiler.withREPLMode(compiler.compileScript(ast))
    val interpreter = Interpreter()
    interpreter.call(bytecode, JSValue.Undefined, Array.empty)

  test("JSON.parse() - simple string") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('\"hello\"')")
    assertEquals(result, JSValue.fromString("hello"))
  }

  test("JSON.parse() - number") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.parse('42')"), JSValue.fromInt(42))
    assertEquals(eval("JSON.parse('-42')"), JSValue.fromInt(-42))
    assertEquals(eval("JSON.parse('3.14')"), JSValue.fromDouble(3.14))
    assertEquals(eval("JSON.parse('-3.14')"), JSValue.fromDouble(-3.14))
    assertEquals(eval("JSON.parse('1e5')"), JSValue.fromDouble(100000.0))
  }

  test("JSON.parse() - boolean") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.parse('true')"), JSValue.fromBoolean(true))
    assertEquals(eval("JSON.parse('false')"), JSValue.fromBoolean(false))
  }

  test("JSON.parse() - null") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.parse('null')"), JSValue.Null)
  }

  test("JSON.parse() - empty object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('{}')")
    // Should be an object
    assert(result.isObject)
  }

  test("JSON.parse() - simple object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('{\"name\": \"John\", \"age\": 30}')")
    assert(result.isObject)
  }

  test("JSON.parse() - nested object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result =
      eval("JSON.parse('{\"person\": {\"name\": \"John\", \"age\": 30}}')")
    assert(result.isObject)
  }

  test("JSON.parse() - empty array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('[]')")
    assert(result.isObject) // Arrays are objects in our representation
  }

  test("JSON.parse() - simple array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('[1, 2, 3]')")
    assert(result.isObject)
  }

  test("JSON.parse() - array with mixed types") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('[1, \"hello\", true, null]')")
    assert(result.isObject)
  }

  test("JSON.parse() - nested array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.parse('[[1, 2], [3, 4]]')")
    assert(result.isObject)
  }

  test("JSON.parse() - complex nested structure") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      JSON.parse('{\
        "users": [\
          {"name": "John", "age": 30},\
          {"name": "Jane", "age": 25}\
        ],\
        "count": 2\
      }')
    """)
    assert(result.isObject)
  }

  test("JSON.parse() - escaped characters in string") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(
      eval("JSON.parse('\"Hello\\\\nWorld\"')").toString,
      "Hello\nWorld"
    )
    assertEquals(eval("JSON.parse('\"\\\\t\\\\r\\\\n\"')").toString, "\t\r\n")
  }

  test("JSON.stringify() - string") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify('hello')")
    assertEquals(result, JSValue.fromString("\"hello\""))
  }

  test("JSON.stringify() - number") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.stringify(42)"), JSValue.fromString("42"))
    assertEquals(eval("JSON.stringify(3.14)"), JSValue.fromString("3.14"))
    assertEquals(eval("JSON.stringify(-42)"), JSValue.fromString("-42"))
  }

  test("JSON.stringify() - boolean") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.stringify(true)"), JSValue.fromString("true"))
    assertEquals(eval("JSON.stringify(false)"), JSValue.fromString("false"))
  }

  test("JSON.stringify() - null") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    assertEquals(eval("JSON.stringify(null)"), JSValue.fromString("null"))
  }

  test("JSON.stringify() - empty object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify({})")
    assertEquals(result, JSValue.fromString("{}"))
  }

  test("JSON.stringify() - simple object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify({name: 'John', age: 30})")
    // Result should be valid JSON
    assert(result.isString)
    val str = result.toString
    assert(str.contains("name"))
    assert(str.contains("age"))
  }

  test("JSON.stringify() - nested object") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify({person: {name: 'John', age: 30}})")
    assert(result.isString)
  }

  test("JSON.stringify() - empty array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify([])")
    assertEquals(result, JSValue.fromString("[]"))
  }

  test("JSON.stringify() - simple array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify([1, 2, 3])")
    assertEquals(result, JSValue.fromString("[1,2,3]"))
  }

  test("JSON.stringify() - array with mixed types") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify([1, 'hello', true, null])")
    assert(result.isString)
    val str = result.toString
    assert(str.startsWith("["))
    assert(str.endsWith("]"))
  }

  test("JSON.stringify() - nested array") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify([[1, 2], [3, 4]])")
    assert(result.isString)
  }

  test("JSON.stringify() - complex nested structure") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      JSON.stringify({
        users: [
          {name: 'John', age: 30},
          {name: 'Jane', age: 25}
        ],
        count: 2
      })
    """)
    assert(result.isString)
  }

  test("JSON.stringify() - with space parameter") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify({a: 1, b: 2}, null, 2)")
    assert(result.isString)
    // Should contain newlines with 2-space indentation
    val str = result.toString
    assert(str.contains("\n"))
  }

  test("JSON.stringify() - undefined becomes null in arrays") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("JSON.stringify([1, undefined, 3])")
    assert(result.isString)
    // undefined in arrays should become null
  }

  test("JSON.stringify() - round-trip") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      var obj = {name: 'John', age: 30, tags: ['js', 'scala']};
      var json = JSON.stringify(obj);
      JSON.parse(json);
    """)
    assert(result.isObject)
  }

  test("JSON.parse() and JSON.stringify() round-trip with numbers") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      var obj = {int: 42, float: 3.14, negative: -10, exp: 1e5};
      var json = JSON.stringify(obj);
      JSON.parse(json);
    """)
    assert(result.isObject)
  }

  test("JSON - real world config example") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      var config = JSON.parse('{\
        "server": {\
          "host": "localhost",\
          "port": 8080\
        },\
        "database": {\
          "url": "mongodb://localhost:27017",\
          "name": "mydb"\
        }\
      }');
      config.server.host;
    """)
    assertEquals(result, JSValue.fromString("localhost"))
  }

  test("JSON - parse API response") {
    given JSRuntime = JSRuntime()
    given ctx: JSContext = JSContext(summon[JSRuntime])

    JSON.initialize()

    val result = eval("""
      var response = JSON.parse('{\
        "status": 200,\
        "data": {\
          "users": [\
            {"id": 1, "name": "John"},\
            {"id": 2, "name": "Jane"}\
          ]\
        }\
      }');
      response.data.users.length;
    """)
    assertEquals(result, JSValue.fromInt(2))
  }
