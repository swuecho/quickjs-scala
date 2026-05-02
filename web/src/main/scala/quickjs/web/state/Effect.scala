package quickjs.web.state

import quickjs.web.models.EditorState

enum Effect {
  case FetchTrace(editor: EditorState)
}
