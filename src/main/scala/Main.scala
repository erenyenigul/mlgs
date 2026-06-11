import static.{Context, TypeChecker}

import pprint.pprintln
import runtime.Interpreter

@main
def main(): Unit = {
    val source =
      """
        |let infoH = 42[H];
        |42[H] as int[?] as int[L]
        |""".stripMargin

    val result = for
      parsed <- Parser.run(source)
      (program, _) = parsed
      _      <- TypeChecker(program, source).run()
      res    <- Interpreter(program, source).run()
    yield res

    result match
      case Left(e)    => throw (e)
      case Right(res) => pprintln(res)
}
