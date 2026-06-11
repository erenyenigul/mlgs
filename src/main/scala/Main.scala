import static.TypeChecker
import runtime.{Interpreter, State}

import java.nio.file.{Files, Paths}

@main
def main(args: String*): Unit =
  args.toList match
    case Nil       => Repl.start()
    case path :: _ => runFile(path)

def evalWith(bindings: List[String], input: String): Either[Exception, (lang.Value, lang.Type, State)] =
  val source = (bindings :+ input).mkString(";\n")
  for
    parsed       <- Parser.run(source)
    (program, _) = parsed
    t            <- TypeChecker(program, source).run()
    res          <- Interpreter(program, source).run()
  yield (res._1, t, res._2)

def runFile(path: String): Unit =
  val source =
    try Files.readString(Paths.get(path))
    catch case e: Exception =>
      System.err.println(s"Error reading file: ${e.getMessage}")
      sys.exit(1)
  evalWith(Nil, source) match
    case Left(e)          => System.err.println(e.getMessage); sys.exit(1)
    case Right((v, _, _)) => pprint.pprintln(v)
