package quickjs.module

import quickjs.value.JSValue
import quickjs.runtime.JSContext

/** Drives the evaluation promise of an async-compiled module body.
  *
  * Module bodies are compiled with `isAsync = true` so top-level `await`
  * suspends the frame. Callers that need the module's exports (the file
  * loader, the standalone Runner, the test262 runner) use this helper to
  * pump microtasks and host work until the evaluation promise settles.
  *
  * If no host work remains while the promise is still pending, the promise
  * can never settle; the helper returns and leaves the module partially
  * evaluated (matching Node's behavior of exiting once the event loop drains).
  */
object ModuleEvaluation {

  /** If `value` is the promise object returned by an async invocation, drive
    * it to settlement. Returns the underlying promise when there is one.
    */
  def settle(value: JSValue)(using ctx: JSContext): Option[JSValue.Promise] =
    value match {
      case JSValue.Object(obj) =>
        obj.getOwnProperty("__promise") match {
          case Some(promise: JSValue.Promise) =>
            if promise.state == JSValue.PromiseState.Pending then {
              ctx.runMicrotasks()
              if promise.state == JSValue.PromiseState.Pending then
                ctx.rt.driveHostAwait(() =>
                  promise.state != JSValue.PromiseState.Pending
                )
            }
            Some(promise)
          case _ => None
        }
      case _ => None
    }

  /** Drive `value` to settlement and rethrow a rejection. */
  def settleAndCheck(value: JSValue)(using ctx: JSContext): Unit =
    settle(value).foreach { promise =>
      if promise.state == JSValue.PromiseState.Rejected then
        ctx.throwException(promise.result)
    }
}
