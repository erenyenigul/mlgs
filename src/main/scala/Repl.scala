import scala.io.StdIn

object Repl:

  private val logo = """
▄▄▄      ▄▄▄ ▄▄▄       ▄▄▄▄▄▄▄   ▄▄▄▄▄▄▄
████▄  ▄████ ███      ███▀▀▀▀▀  █████▀▀▀
███▀████▀███ ███      ███        ▀████▄
███  ▀▀  ███ ███      ███  ███▀    ▀████
███      ███ ████████ ▀██████▀  ███████▀

"""

  def start(): Unit =
    println(logo)
    println(":clear reset history   \n:ctx  show history   \n:quit  exit")
    println()
    var history: List[String] = Nil

    def prompt(): Unit = print(if history.isEmpty then ">> " else s"(${history.length}) >> ")

    while true do
      prompt()
      val line = StdIn.readLine()
      if line == null || line.trim == ":quit" then
        println("bye"); sys.exit(0)
      else line.trim match
        case ":clear" =>
          history = Nil
          println("history cleared")
        case ":ctx" =>
          if history.isEmpty then println("(empty)")
          else history.foreach(h => println(s"  $h"))
        case input if input.nonEmpty =>
          evalWith(history, input) match
            case Left(e) =>
              println(s"error: ${e.getMessage}")
            case Right((v, t, _)) =>
              print(s"  : $t = ")
              pprint.pprintln(v)
              history = history :+ input
        case _ =>
