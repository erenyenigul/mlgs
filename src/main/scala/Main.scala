import static.{Context, TypeChecker}

import scala.util.parsing.combinator.*
import pprint.pprintln
import runtime.Interpreter

@main
def main(): Unit = {
    val (program, source) = Parser.run(
      """
        |  let z = (new(int@?, low) 5@low) in
        |  let y = {int@low <= int@?}@asdfas 5@high in
        |  z := y;
        |  !z
        |""".stripMargin)
    TypeChecker(program, source).run()
    val res = Interpreter(program, source).run()

    pprintln(res)
}