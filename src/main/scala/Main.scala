import static.{Context, TypeChecker}

import pprint.pprintln
import runtime.Interpreter

@main
def main(): Unit = {
    val source =
      """
        |  let z = (new(int@?, low) 5@low) in
        |  z := 5@high;
        |  !z
        |""".stripMargin

    val result = for
      parsed <- Parser.run(source)
      (program, _) = parsed
      _      <- TypeChecker(program, source).run()
      res    <- Interpreter(program, source).run()
    yield res

    result match
      case Left(e)    => println(e.getMessage)
      case Right(res) => pprintln(res)
}
