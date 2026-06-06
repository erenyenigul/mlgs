import lang.Parser
import static.{Context, TypeChecker}

import scala.util.parsing.combinator.*
import pprint.pprintln

@main
def main(): Unit = {
  val program = Parser.run("let z = 5^H in new ^ (int ^ H, L) z")

  pprintln(TypeChecker.infer(program, Context()))
}