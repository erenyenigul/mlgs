package runtime
import lang.*

case class InterpreterErrorAt(error: InterpreterError, line: Int, col: Int, sourceLine: String) extends Exception:
  override def getMessage: String =
    val caret = " " * (col - 1) + "^"
    s"${error.getMessage}\n${line} |${sourceLine}\n   ${caret}"

enum InterpreterError extends Exception:
  case BlameError(p: BlameLabel)

  override def getMessage: String = this match
    case BlameError(p) =>
      s"Runtime Security Error: Security violation. Blame assigned to cast(s): ${p.map(_.id).mkString(", ")}."