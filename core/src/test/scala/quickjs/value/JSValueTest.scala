package quickjs.value

import munit.*

class JSValueTest extends FunSuite:

  test("JSValue.fromInt") {
    assert(JSValue.fromInt(42) == JSValue.Int32(42))
    assert(JSValue.fromInt(-10) == JSValue.Int32(-10))
    assert(JSValue.fromInt(0) == JSValue.Int32(0))
  }

  test("JSValue.fromDouble - integer conversion") {
    assert(JSValue.fromDouble(42.0) == JSValue.Int32(42))
    assert(JSValue.fromDouble(-10.0) == JSValue.Int32(-10))
  }

  test("JSValue.fromDouble - floating point") {
    assert(JSValue.fromDouble(3.14) == JSValue.Float64(3.14))
    assert(JSValue.fromDouble(1.5) == JSValue.Float64(1.5))
  }

  test("JSValue.fromBoolean") {
    assert(JSValue.fromBoolean(true) == JSValue.Bool(true))
    assert(JSValue.fromBoolean(false) == JSValue.Bool(false))
  }

  test("JSValue.add - Int32") {
    val result = JSValue.add(JSValue.fromInt(1), JSValue.fromInt(2))
    assert(result == JSValue.fromInt(3))
  }

  test("JSValue.add - overflow to Float64") {
    val result = JSValue.add(JSValue.fromInt(Int.MaxValue), JSValue.fromInt(Int.MaxValue))
    assert(result.isNumber)
  }

  test("JSValue.subtract") {
    val result = JSValue.subtract(JSValue.fromInt(5), JSValue.fromInt(3))
    assert(result == JSValue.fromInt(2))
  }

  test("JSValue.multiply") {
    val result = JSValue.multiply(JSValue.fromInt(3), JSValue.fromInt(4))
    assert(result == JSValue.fromInt(12))
  }

  test("JSValue.divide") {
    val result = JSValue.divide(JSValue.fromInt(10), JSValue.fromInt(2))
    // Division produces Float64, but 5.0 gets optimized to Int32(5) by our smart constructor
    assert(result.toNumber == 5.0)
  }

  test("JSValue.divide - division by zero") {
    val result = JSValue.divide(JSValue.fromInt(10), JSValue.fromInt(0))
    assert(result == JSValue.Float64(Double.PositiveInfinity))
  }

  test("JSValue.toBoolean") {
    assert(JSValue.Undefined.toBoolean == false)
    assert(JSValue.Null.toBoolean == false)
    assert(JSValue.Bool(true).toBoolean == true)
    assert(JSValue.Bool(false).toBoolean == false)
    assert(JSValue.fromInt(0).toBoolean == false)
    assert(JSValue.fromInt(1).toBoolean == true)
    assert(JSValue.fromInt(-1).toBoolean == true)
  }

  test("JSValue.toNumber") {
    assert(JSValue.Undefined.toNumber.isNaN)
    assert(JSValue.Null.toNumber == 0.0)
    assert(JSValue.Bool(true).toNumber == 1.0)
    assert(JSValue.Bool(false).toNumber == 0.0)
    assert(JSValue.fromInt(42).toNumber == 42.0)
  }

  test("1 + 2 = 3") {
    val result = JSValue.add(
      JSValue.fromInt(1),
      JSValue.fromInt(2)
    )
    assertEquals(result, JSValue.fromInt(3))
  }
