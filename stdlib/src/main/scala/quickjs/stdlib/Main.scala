package quickjs.stdlib

import quickjs.runtime.{JSRuntime, JSContext, StdLib}
import quickjs.repl.REPL

/** Main entry point for QuickJS-Scala REPL with full standard library support.
  *
  * This is the recommended way to run the REPL with all standard library
  * features enabled. The initialization mirrors `Runner`: the runtime built-ins
  * (`StdLib`), JSON, console and the host globals are all installed before the
  * REPL starts.
  */
object Main {
  /** Install the standard library and host globals. Called once at startup and
    * again after `.reset` clears the configurable global properties.
    */
  private def installStdLib()(using ctx: JSContext): Unit = {
    StdLib.initialize(ctx)
    JSON.initialize()
    Console.initialize()
    Globals.initialize()
  }

  def main(args: Array[String]): Unit = {
    given JSRuntime = JSRuntime()
    given JSContext = JSContext(summon[JSRuntime])

    // Initialize standard library (same set as the script runner).
    installStdLib()

    // Start REPL
    val repl =
      new REPL(summon[JSRuntime], summon[JSContext], () => installStdLib())
    repl.run()
  }
}
