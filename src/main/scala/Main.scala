import static.{Context, TypeChecker}

import pprint.pprintln
import runtime.Interpreter

@main
def main(): Unit = {
    val source =
      """
        |let infoH = new(int@high, low) 42@high in
        |let addPrivileged = fn isPrivileged : int@? @ ? ->
        |  fn worker : ref(int@?)@? @ ? ->
        |    fn report : ref(int@?)@? @ ? ->
        |      (report := !infoH)
        |in
        |let buf = new(int@low, low) 0@low in
        |((addPrivileged 0@low) ({ref(int@?)@? <= ref(int@low)@low} buf)) ({ref(int@?)@? <= ref(int@low)@low} buf);
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
