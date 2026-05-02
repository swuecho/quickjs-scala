package quickjs.web.components

import com.raquo.laminar.api.L.*
import scala.scalajs.js

case class BytecodeProps(
    instructions: js.Array[js.Dynamic],
    bytecode: Vector[String],
    selectedPc: Option[Int]
)

object BytecodeComponent {
  def apply(props: BytecodeProps): HtmlElement =
    div(
      cls := "panel bytecode-panel",
      div(
        cls := "panel-header",
        div(cls := "panel-title", "Bytecode"),
        div(
          cls := "meta",
          s"${props.bytecode.length} bytes"
        )
      ),
      div(
        cls := "instructions",
        props.instructions.zipWithIndex.map { case (inst, _) =>
          div(
            cls := "instruction",
            cls.toggle("highlight") := props.selectedPc.contains(
              inst.pc.asInstanceOf[Int]
            ),
            span(cls := "instr-pc", f"${inst.pc.asInstanceOf[Int]}%03d"),
            span(cls := "instr-op", inst.opcode.toString),
            span(
              cls := "instr-opnd",
              inst.operand.asInstanceOf[js.UndefOr[String]].getOrElse("")
            )
          )
        }
      ),
      div(
        cls := "bytecode",
        props.bytecode.zipWithIndex.map { case (value, idx) =>
          div(
            cls := "byte",
            cls.toggle("highlight") := props.selectedPc.contains(idx),
            span(cls := "byte-idx", f"$idx%03d"),
            span(cls := "byte-val", value)
          )
        }
      )
    )
}
