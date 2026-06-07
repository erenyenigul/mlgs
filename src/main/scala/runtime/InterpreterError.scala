package runtime
import lang.*

case class InterpreterErrorAt(error: InterpreterError, line: Int, col: Int, sourceLine: String) extends Exception:
  override def getMessage: String =
    val caret = " " * (col - 1) + "^"
    s"Runtime Error: Triggered at:\n  $line |$sourceLine\n     $caret\n${error.getMessage}\n"

enum InterpreterError extends Exception:
  case BlameError(p: BlameLabel)

  override def getMessage: String = this match
    case BlameError(p) =>
      val sources = p.map { b =>
        if b.line > 0 then
          val bCaret = " " * (b.col - 1) + "^"
          s"  ${b.id}\n  ${b.line} |${b.sourceLine}\n     $bCaret"
        else s"  ${b.id}"
      }.mkString("\n")
      s"Security violation.\nPossible sources:\n$sources"
