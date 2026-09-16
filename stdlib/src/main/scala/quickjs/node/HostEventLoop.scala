package quickjs.node

import quickjs.value.JSValue
import quickjs.runtime.JSContext
import quickjs.runtime.builtins.BuiltinHelpers

import java.util.concurrent.{ConcurrentLinkedQueue, Executors, ThreadFactory}
import scala.collection.mutable

/** Host event loop for Node compatibility mode.
  *
  * Owns timers, immediate callbacks, microtask draining and a thread-safe queue
  * of completions posted from JVM threads. Blocking host operations run on a
  * small daemon pool (`execute`) and are expected to `post` their completion
  * back onto the loop's thread.
  *
  * The loop runs until there is no pending work: no live timers, no in-flight
  * host operations and no queued microtasks/tasks.
  */
final class HostEventLoop {

  private final case class TimerTask(
      var due: Long,
      order: Long,
      id: Int,
      callback: JSValue,
      args: Array[JSValue],
      interval: Long,
      immediate: Boolean
  )

  private val timers = mutable.PriorityQueue.empty[TimerTask](
    Ordering.by[TimerTask, (Long, Long)](task => (task.due, task.order)).reverse
  )
  private val cancelled = mutable.HashSet.empty[Int]
  private var nextId = 1
  private var nextOrder = 0L
  private var pendingOps = 0
  private val lock = new Object
  private val hostTasks = new ConcurrentLinkedQueue[() => Unit]()
  private var stopped = false

  private val pool = Executors.newCachedThreadPool(new ThreadFactory {
    private val counter = new java.util.concurrent.atomic.AtomicInteger(0)
    def newThread(runnable: Runnable): Thread = {
      val thread = new Thread(runnable, s"quickjs-host-${counter.incrementAndGet()}")
      thread.setDaemon(true)
      thread
    }
  })

  // =========================================================================
  // Timers
  // =========================================================================

  /** Schedule a timer; `interval == 0` means one-shot. */
  def schedule(
      callback: JSValue,
      delayMs: Double,
      args: Array[JSValue],
      interval: Boolean
  ): Int = {
    val ms =
      if delayMs.isNaN || delayMs <= 0.0 then 0L
      else if delayMs.isInfinite || delayMs > 2147483647.0 then 2147483647L
      else delayMs.toLong
    val task = TimerTask(
      due = System.currentTimeMillis + ms,
      order = nextOrder,
      id = nextId,
      callback = callback,
      args = args,
      interval = if interval then math.max(ms, 1L) else 0L,
      immediate = false
    )
    nextOrder += 1
    nextId += 1
    timers.enqueue(task)
    lock.synchronized(lock.notifyAll())
    task.id
  }

  /** Schedule a `setImmediate` callback. */
  def scheduleImmediate(callback: JSValue, args: Array[JSValue]): Int = {
    val task = TimerTask(
      due = 0L,
      order = nextOrder,
      id = nextId,
      callback = callback,
      args = args,
      interval = 0L,
      immediate = true
    )
    nextOrder += 1
    nextId += 1
    timers.enqueue(task)
    lock.synchronized(lock.notifyAll())
    task.id
  }

  def clear(id: Int): Unit = {
    cancelled += id
    lock.synchronized(lock.notifyAll())
  }

  // =========================================================================
  // Host operations
  // =========================================================================

  /** Keep the loop alive for an in-flight host operation. */
  def retain(): Unit = lock.synchronized {
    pendingOps += 1
    lock.notifyAll()
  }

  /** Release a retain once the operation has settled. */
  def release(): Unit = lock.synchronized {
    pendingOps -= 1
    lock.notifyAll()
  }

  /** Queue a task to run on the loop thread (callable from any thread). */
  def post(task: () => Unit): Unit = {
    hostTasks.add(task)
    lock.synchronized(lock.notifyAll())
  }

  /** Run a blocking operation on the host pool. The loop stays alive until the
    * operation completes; completions should be delivered with `post`.
    */
  def execute(body: => Unit): Unit = {
    lock.synchronized { pendingOps += 1 }
    try
      pool.execute { () =>
        try body
        finally lock.synchronized {
          pendingOps -= 1
          lock.notifyAll()
        }
      }
    catch {
      case _: java.util.concurrent.RejectedExecutionException =>
        lock.synchronized { pendingOps -= 1 }
        throw new NodeExit(1)
    }
  }

  // =========================================================================
  // Main loop
  // =========================================================================

  /** Run until no work remains. When `propagateErrors` is true an exception
    * thrown by a timer callback escapes (Node's fatal behavior).
    */
  def run(ctx: JSContext, propagateErrors: Boolean): Unit =
    runLoop(ctx, propagateErrors, () => false)

  /** Run until `until` returns true or no work remains. Returns true when
    * `until` held as the loop stopped. Used to drive top-level await while a
    * module waits for timers / async I/O.
    */
  def runUntil(
      ctx: JSContext,
      propagateErrors: Boolean,
      until: () => Boolean
  ): Boolean = {
    runLoop(ctx, propagateErrors, until)
    until()
  }

  private def runLoop(
      ctx: JSContext,
      propagateErrors: Boolean,
      until: () => Boolean
  ): Unit = {
    var continue = !until()
    while continue do {
      ctx.runMicrotasks()
      if until() then continue = false
      else {
        pruneCancelled()

        var ranSomething = false
        var now = System.currentTimeMillis
        while continue && timers.nonEmpty && (timers.head.immediate || timers.head.due <= now) do {
          val task = timers.dequeue()
          if !cancelled.remove(task.id) then {
            ranSomething = true
            runTimer(task, ctx, propagateErrors)
            now = System.currentTimeMillis
            if until() then continue = false
          }
        }

        if continue then {
          var host = hostTasks.poll()
          while continue && host != null do {
            ranSomething = true
            host()
            if until() then continue = false
            else host = hostTasks.poll()
          }
        }

        if continue then {
          ctx.runMicrotasks()
          pruneCancelled()

          if until() then continue = false
          else if !hasWork(ctx) then continue = false
          else if !ranSomething then {
            val timeout =
              if timers.nonEmpty then
                math.max(1L, math.min(timers.head.due - System.currentTimeMillis, 1000L))
              else 1000L
            lock.synchronized {
              if hostTasks.isEmpty && !hasDueTimer() && !stopped then
                try lock.wait(timeout)
                catch case _: InterruptedException => continue = false
            }
          }
        }
      }
    }
  }

  /** Wake a loop that is waiting without necessarily running the loop. */
  def wake(): Unit = lock.synchronized(lock.notifyAll())

  /** Ask the loop to stop after the current task. */
  def stop(): Unit = {
    stopped = true
    lock.synchronized(lock.notifyAll())
  }

  private def pruneCancelled(): Unit =
    if cancelled.nonEmpty then {
      val kept = timers.iterator
        .filterNot(task => cancelled.contains(task.id))
        .toVector
      timers.clear()
      timers.enqueue(kept*)
      cancelled.clear()
    }

  private def hasDueTimer(): Boolean =
    timers.nonEmpty &&
      (timers.head.immediate || timers.head.due <= System.currentTimeMillis)

  private def hasWork(ctx: JSContext): Boolean =
    timers.nonEmpty || pendingOps > 0 || !hostTasks.isEmpty ||
      ctx.hasPendingMicrotasks

  private def runTimer(
      task: TimerTask,
      ctx: JSContext,
      propagateErrors: Boolean
  ): Unit = {
    given JSContext = ctx
    try
      BuiltinHelpers.callFunctionWithThis(
        task.callback,
        JSValue.Undefined,
        task.args
      )
    catch {
      case error: Throwable =>
        if propagateErrors then throw error
    }
    if task.interval > 0L && !cancelled.contains(task.id) then {
      task.due = System.currentTimeMillis + task.interval
      timers.enqueue(task)
    }
  }
}
