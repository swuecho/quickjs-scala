package quickjs.stdlib

import quickjs.runtime.{JSRuntime, JSContext}
import quickjs.repl.REPL

/** Main entry point for QuickJS-Scala REPL with full standard library support.
  *
  * This is the recommended way to run the REPL with all standard library features enabled.
  */
object Main:
  def main(args: Array[String]): Unit =
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library
    ArrayStatics.initialize()
    JSON.initialize()

    // Start REPL
    val repl = new REPL(summon[JSRuntime], summon[JSContext])
    repl.run()
