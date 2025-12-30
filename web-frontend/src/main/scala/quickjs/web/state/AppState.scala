package quickjs.web.state

import com.raquo.laminar.api.L.*
import scala.scalajs.js
import quickjs.web.models.TraceMeta

object AppState:
  // UI State
  val endpointVar = Var("/trace")
  val replModeVar = Var(false)
  val runningVar = Var(false)
  val errorVar = Var(Option.empty[String])
  val pingStatusVar = Var("unknown")
  
  // Content State
  val sourceVar = Var(
    """// Try it:
      |const add = (a, b) => a + b;
      |let total = 0;
      |for (let i = 0; i < 3; i++) {
      |  total = add(total, i);
      |}
      |total;""".stripMargin
  )
  
  // Trace Data
  val metaVar = Var(Option.empty[TraceMeta])
  val eventsVar = Var(js.Array[js.Dynamic]())
  val bytecodeVar = Vector(Var(Vector.empty[String]))
  val instructionsVar = Var(js.Array[js.Dynamic]())
  
  // Selection State
  val selectedIndexVar = Var(Option.empty[Int])
  val lastStackDepthVar = Var(0)
  val stackDeltaVar = Var("none")
  
  // Computed signals
  val selectedPcSignal = selectedIndexVar.signal
    .combineWith(eventsVar.signal)
    .map {
      case (Some(idx), events) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" then
          Some(event.pc.asInstanceOf[Int])
        else
          None
      case _ => None
    }
  
  val hasSelectionSignal = selectedIndexVar.signal.map(_.isDefined)
  val canStepPrevSignal = selectedIndexVar.signal.map(_.forall(_ <= 0))
  val canStepNextSignal = selectedIndexVar.signal
    .combineWith(eventsVar.signal)
    .map {
      case (Some(idx), events) => idx >= events.length - 1
      case _ => true
    }
  
  def resetTraceData(): Unit =
    metaVar.set(None)
    eventsVar.set(js.Array())
    bytecodeVar.head.set(Vector.empty)
    instructionsVar.set(js.Array())
    selectedIndexVar.set(None)
    lastStackDepthVar.set(0)
    stackDeltaVar.set("none")
  
  def setError(message: String, stack: Option[String] = None): Unit =
    val combined = stack match
      case Some(s) if s.nonEmpty => s"$message\n$s"
      case _ => message
    errorVar.set(Some(combined))
    resetTraceData()
  
  def clearError(): Unit =
    errorVar.set(None)
  
  def updateStackDepth(idxOpt: Option[Int], events: js.Array[js.Dynamic]): Unit =
    val depth = currentStackDepth(idxOpt, events)
    if depth >= 0 then
      val previous = lastStackDepthVar.now()
      if depth > previous then stackDeltaVar.set("push")
      else if depth < previous then stackDeltaVar.set("pop")
      else stackDeltaVar.set("same")
      lastStackDepthVar.set(depth)
    else
      stackDeltaVar.set("none")
      lastStackDepthVar.set(0)
  
  private def currentStackDepth(idxOpt: Option[Int], events: js.Array[js.Dynamic]): Int =
    idxOpt match
      case Some(idx) if idx >= 0 && idx < events.length =>
        val event = events(idx)
        if event.`type`.toString == "instruction" && js.typeOf(event.stack) != "undefined" then
          event.stack.asInstanceOf[js.Array[js.Dynamic]].length
        else
          -1
      case _ => -1