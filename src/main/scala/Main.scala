import lang.Parser
import scala.util.parsing.combinator._

@main
def main(): Unit = {
  val parser = Parser()

  parser.run("let x = 0^H in 5")
}