package quickjs.tracing

import quickjs.runtime.{JSContext, JSRuntime}
import munit.*

class TraceSessionTest extends FunSuite {

  test("trace session builds json and bytecode") {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    val result = TraceSession.run("1 + 2")

    assert(result.events.nonEmpty)
    assert(result.instructions.nonEmpty)
    assert(result.json.contains("\"type\":\"instruction\""))
    assert(result.json.contains("\"type\":\"return\""))
    assert(result.bytecodeLength > 0)
    assert(result.bytecodeHex.nonEmpty)
  }
}
