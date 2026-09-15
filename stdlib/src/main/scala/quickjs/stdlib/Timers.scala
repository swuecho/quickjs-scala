package quickjs.stdlib

import quickjs.value.{JSValue, NativeFunction}
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers
import scala.collection.mutable

/** Minimal host timer support (`setTimeout`, `setInterval`, ...).
  *
  * Timers are collected during script execution and drained by [[runPending]]
  * after the main script completes (microtasks are drained between callbacks),
  * which mirrors how the `qjs` command line runs scripts.
  */
object Timers {

  private final case class Task(
      var due: Long,
      order: Long,
      id: Int,
      callback: JSValue,
      args: Array[JSValue],
      interval: Long
  )

  final class State {
    private[Timers] val queue = mutable.PriorityQueue.empty[Task](
      Ordering.by[Task, (Long, Long)](t => (t.due, t.order)).reverse
    )
    private[Timers] val cancelled = mutable.HashSet.empty[Int]
    private[Timers] var nextId = 1
    private[Timers] var nextOrder = 0L
  }

  def newState(): State = new State()

  private def schedule(
      state: State,
      callback: JSValue,
      delay: Double,
      args: Array[JSValue],
      repeat: Boolean
  ): JSValue = {
    val ms =
      if delay.isNaN || delay <= 0.0 then 0L
      else if delay.isInfinite || delay > 2147483647.0 then 2147483647L
      else delay.toLong
    val id = state.nextId
    state.nextId += 1
    val task = Task(
      due = System.currentTimeMillis + ms,
      order = state.nextOrder,
      id = id,
      callback = callback,
      args = args,
      interval = if repeat then math.max(ms, 1L) else 0L
    )
    state.nextOrder += 1
    state.queue.enqueue(task)
    JSValue.fromInt(id)
  }

  /** Install the timer globals on `ctx`. */
  def initialize(state: State)(using ctx: JSContext): Unit = {
    def asCallback(value: JSValue)(using JSContext): JSValue =
      value match {
        case JSValue.JSStr(code) =>
          // String bodies are compiled and evaluated in global scope when the
          // timer fires.
          JSValue.Native(
            NativeFunction(
              name = "<setTimeout>",
              length = 0,
              impl = (_, fireCtx) => {
                given JSContext = fireCtx
                try {
                  val tokens = quickjs.lexer.Lexer(code).tokenize()
                  val ast = quickjs.parser.Parser(tokens).parseScript()
                  val bytecode = quickjs.compiler.Compiler().compileScript(ast)
                  quickjs.interpreter
                    .Interpreter()
                    .call(bytecode, JSValue.Undefined, Array.empty)
                } catch case _: Throwable => ()
                JSValue.Undefined
              }
            )
          )
        case other => other
      }

    def delayOf(args: Array[JSValue])(using JSContext): Double =
      if args.length > 1 then BuiltinHelpers.toNumber(args(1)) else 0.0

    val setTimeoutFn = NativeFunction(
      name = "setTimeout",
      length = 2,
      impl = (args, ctx) => {
        given JSContext = ctx
        val callback = asCallback(args.headOption.getOrElse(JSValue.Undefined))
        val rest = if args.length > 2 then args.drop(2) else Array.empty[JSValue]
        schedule(state, callback, delayOf(args), rest, repeat = false)
      }
    )
    val setIntervalFn = NativeFunction(
      name = "setInterval",
      length = 2,
      impl = (args, ctx) => {
        given JSContext = ctx
        val callback = asCallback(args.headOption.getOrElse(JSValue.Undefined))
        val rest = if args.length > 2 then args.drop(2) else Array.empty[JSValue]
        schedule(state, callback, delayOf(args), rest, repeat = true)
      }
    )
    def clearImpl(args: Array[JSValue]): JSValue = {
      args.headOption.foreach {
        case JSValue.Int32(id)   => state.cancelled += id
        case JSValue.Float64(d)  => state.cancelled += d.toInt
        case _                   => ()
      }
      JSValue.Undefined
    }
    val clearTimeoutFn = NativeFunction(
      name = "clearTimeout",
      length = 1,
      impl = (args, _) => clearImpl(args)
    )
    val clearIntervalFn = NativeFunction(
      name = "clearInterval",
      length = 1,
      impl = (args, _) => clearImpl(args)
    )

    ctx.global.set("setTimeout", JSValue.Native(setTimeoutFn))
    ctx.global.set("setInterval", JSValue.Native(setIntervalFn))
    ctx.global.set("clearTimeout", JSValue.Native(clearTimeoutFn))
    ctx.global.set("clearInterval", JSValue.Native(clearIntervalFn))
  }

  /** Run all pending timers until the queue is empty. Intervals keep the loop
    * alive (like Node); `clearInterval`/`clearTimeout` stop them.
    */
  def runPending(state: State)(using ctx: JSContext): Unit = {
    while state.queue.nonEmpty do {
      val task = state.queue.dequeue()
      if !state.cancelled.remove(task.id) then {
        val now = System.currentTimeMillis
        if task.due > now then {
          val wait = math.min(task.due - now, 1000L)
          try Thread.sleep(math.max(wait, 0L))
          catch case _: InterruptedException => ()
        }
        try
          BuiltinHelpers.callFunctionWithThis(
            task.callback,
            JSValue.Undefined,
            task.args
          )
        catch case _: Throwable => ()
        ctx.runMicrotasks()
        if task.interval > 0L && !state.cancelled.contains(task.id) then {
          task.due = System.currentTimeMillis + task.interval
          state.queue.enqueue(task)
        }
      }
    }
  }
}
