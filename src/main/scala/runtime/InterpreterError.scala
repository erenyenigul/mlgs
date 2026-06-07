package runtime
import lang.*
import util.Diagnostic.formatLine

case class InterpreterErrorAt(error: InterpreterError, line: Int, col: Int, sourceLine: String, notes: List[String] = Nil) extends Exception:
  override def getMessage: String =
    val notesStr = if notes.isEmpty then "" else "\n" + notes.map(n => s"  note: $n").mkString("\n")
    s"${error.reason} Triggered at:\n${formatLine(line, col, sourceLine)}\n${error.details}$notesStr"

enum InterpreterError extends Exception:
  case BlameError(p: BlameLabel)

  def reason: String = this match
    case BlameError(_) => "Runtime Error: Security violation."

  def details: String = this match
    case BlameError(p) =>
      val sources = p.map { b =>
        if b.line > 0 then s"  ${b.id}\n${formatLine(b.line, b.col, b.sourceLine)}"
        else s"  ${b.id}"
      }.mkString("\n")
      s"Possible sources:\n$sources"

  override def getMessage: String = s"${reason}\n${details}"
