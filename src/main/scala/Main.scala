import static.{Context, TypeChecker}

import scala.util.parsing.combinator.*
import pprint.pprintln

@main
def main(): Unit = {
  try {
    val (program, source) = Parser.run(
      """
        |let z = 5@high in
        |  new(int@high, low) z
        |""".stripMargin)
    val res = TypeChecker(program, source).run()

    pprintln(res)
  } catch {
    case e: Throwable => println(e.getMessage)
  }
}