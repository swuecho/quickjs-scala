package quickjs.web.state

import quickjs.web.models.{EditorState, TraceData}

enum Action:
  case UpdateSource(value: String)
  case SetReplMode(enabled: Boolean)
  case RunTrace
  case TraceLoaded(data: TraceData)
  case TraceFailed(error: String)
  case SelectIndex(index: Option[Int])
  case StepPrev
  case StepNext
