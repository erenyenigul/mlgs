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
      val casts = p.map { b =>
        if b.line > 0 then
          val caret = " " * (b.col - 1) + "^"
          s"${b.id}\n  ${b.line} |${b.sourceLine}\n     $caret"
        else b.id
      }.mkString("\n  ")
      s"Runtime Security Error: Security violation. Possible sources:\n  $casts"