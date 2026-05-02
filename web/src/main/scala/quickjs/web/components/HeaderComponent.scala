package quickjs.web.components

import com.raquo.laminar.api.L.*

object HeaderComponent:
  def apply(): HtmlElement =
    div(
      cls := "hero",
      div(
        span(cls := "eyebrow", "QuickJS-Scala Educational Platform"),
        h1("Execution Trace Studio"),
        p(
          "Paste JavaScript, run it against the QuickJS-Scala trace server, and inspect every instruction, stack shape, and call/return event."
        )
      )
    )
