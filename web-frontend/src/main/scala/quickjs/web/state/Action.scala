package quickjs.web.state

import quickjs.web.features.editor.EditorFeature
import quickjs.web.features.trace.TraceFeature

enum AppAction:
  case Editor(action: EditorFeature.Action)
  case Trace(action: TraceFeature.Action)
  case RunTrace
  case TraceFailed(error: String)
