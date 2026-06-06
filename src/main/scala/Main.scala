import lang.Parser
import static.{Context, TypeChecker}

import scala.util.parsing.combinator.*

@main
def main(): Unit = {
  val program = Parser.run("let z = (fn x : int^L @ H => 5) ^ ? in z")

  println(TypeChecker.infer(program, Context()))
}