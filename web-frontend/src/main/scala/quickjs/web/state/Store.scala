package quickjs.web.state

import com.raquo.laminar.api.L.*
import quickjs.web.client.TraceApiClient

final class Store(endpoint: String):
  private val stateVar = Var(AppState.empty)

  val state: Signal[AppState] = stateVar.signal

  val actions: Observer[Action] = Observer(action => handle(action))

  private def handle(action: Action): Unit =
    val (nextState, effects) = Reducer.reduce(stateVar.now(), action)
    stateVar.set(nextState)
    effects.foreach(runEffect)

  private def runEffect(effect: Effect): Unit =
    effect match
      case Effect.FetchTrace(editorState) =>
        TraceApiClient.fetchTrace(endpoint, editorState, {
          case Right(data) => actions.onNext(Action.TraceLoaded(data))
          case Left(error) => actions.onNext(Action.TraceFailed(error))
        })
