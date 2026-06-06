import static.{Context, TypeChecker}

import scala.util.parsing.combinator.*
import pprint.pprintln

@main
def main(): Unit = {
  try {
    val (program, source) = Parser.run("let z = 5^H \n in new ^ (int ^ L, L) z")
    val res = TypeChecker(program, source).run()

    pprintln(res)
  } catch {
    case e: Throwable => println(e.getMessage)
  }
}