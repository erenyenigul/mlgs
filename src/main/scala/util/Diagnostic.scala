package util

object Diagnostic:
  def formatLine(line: Int, col: Int, sourceLine: String): String =
    val caret = " " * (col - 1) + "^"
    s"  $line |$sourceLine\n     $caret"
