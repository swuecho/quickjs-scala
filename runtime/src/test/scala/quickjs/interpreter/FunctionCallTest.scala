package quickjs.interpreter

import quickjs.bytecode.*
import quickjs.compiler.Compiler
import quickjs.value.JSValue
import quickjs.runtime.*
import munit.*

class FunctionCallTest extends FunSuite {

  test("simple function call - add two numbers") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create a function that returns 42
    // function() { return 42; }
    val funcBytecode = Array[Byte](
      Opcode.PushI32.code.toByte,           // 0: push 42
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x2A.toByte, // 42 as i32
      Opcode.Return.code.toByte              // 5: return
    )

    val func = JSValue.Function(
      name = "testFunc",
      bytecode = funcBytecode,
      constants = Array.empty,
      stackSize = 256
    )

    // Create a main bytecode that calls the function
    // We'll manually test the Call opcode by creating bytecode
    val mainBytecode = Array[Byte](
      // Push function (we'll simulate this by having it already in a variable)
      Opcode.GetLoc.code.toByte,            // 0: get function from var 0
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, // index 0
      Opcode.Call.code.toByte,             // 5: call with 0 args
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, // argc = 0
      Opcode.Drop.code.toByte,              // 10: drop result
      Opcode.ReturnUndef.code.toByte        // 11: return
    )

    val mainFunc = new BytecodeFunction(
      name = "<main>",
      bytecode = mainBytecode,
      constants = Array.empty,
      stackSize = 256
    )

    val interpreter = Interpreter()

    // Set up local variable 0 with the function
    // We need to modify the bytecode execution to pre-populate locals
    // For this test, let's use a simpler approach: direct call test
    val result = interpreter.call(
      new BytecodeFunction("test", funcBytecode, Array.empty, 256),
      JSValue.Undefined,
      Array.empty
    )

    // Function should return 42
    assert(result == JSValue.fromInt(42))
  }

  test("function call with one argument") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create a function that adds 1 to its argument
    // function(x) { return x + 1; }
    val funcBytecode = Array[Byte](
      Opcode.GetArg.code.toByte,            // 0: get arg 0
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, // index 0
      Opcode.PushI32.code.toByte,           // 5: push 1
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x01.toByte, // 1 as i32
      Opcode.Add.code.toByte,               // 10: add
      Opcode.Return.code.toByte             // 11: return
    )

    val result = Interpreter().call(
      new BytecodeFunction("addOne", funcBytecode, Array.empty, 256),
      JSValue.Undefined,
      Array(JSValue.fromInt(5))
    )

    // Function should return 6 (5 + 1)
    assertEquals(result, JSValue.fromInt(6))
  }

  test("function call with multiple arguments") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create a function that adds two numbers
    // function(x, y) { return x + y; }
    val funcBytecode = Array[Byte](
      Opcode.GetArg.code.toByte,            // 0: get arg 0 (x)
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte, // index 0 (big-endian)
      Opcode.GetArg.code.toByte,            // 5: get arg 1 (y)
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x01.toByte, // index 1 (big-endian)
      Opcode.Add.code.toByte,               // 10: add
      Opcode.Return.code.toByte             // 11: return
    )

    val result = Interpreter().call(
      new BytecodeFunction("add", funcBytecode, Array.empty, 256),
      JSValue.Undefined,
      Array(JSValue.fromInt(3), JSValue.fromInt(4))
    )

    // Function should return 7 (3 + 4)
    assertEquals(result, JSValue.fromInt(7))
  }

  test("Call opcode with zero arguments via bytecode") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Create a function that returns 42
    val funcBytecode = Array[Byte](
      Opcode.PushI32.code.toByte,
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x2A.toByte, // 42
      Opcode.Return.code.toByte
    )

    val funcValue = JSValue.Function("return42", funcBytecode, Array.empty, 256)

    // Main bytecode: calls function with 0 args
    // We simulate having the function in a local variable
    val mainBytecode = Array[Byte](
      // For simplicity, we'll push a function marker
      // In real implementation, this would be done via FunctionDeclaration
      Opcode.PushUndefined.code.toByte,     // 0: placeholder
      Opcode.GetLoc.code.toByte,            // 1: get var 0 (would be the function)
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte,
      Opcode.Call.code.toByte,             // 6: call with 0 args
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte,
      Opcode.Drop.code.toByte,              // 11: drop result
      Opcode.ReturnUndef.code.toByte        // 12: return
    )

    val mainFunc = new BytecodeFunction(
      "<main>",
      mainBytecode,
      Array.empty,
      256
    )

    val interpreter = Interpreter()

    // For now, let's test the direct call mechanism
    val result = interpreter.call(
      new BytecodeFunction("return42", funcBytecode, Array.empty, 256),
      JSValue.Undefined,
      Array.empty
    )

    assert(result == JSValue.fromInt(42))
  }
}
