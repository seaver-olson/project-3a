import Statement.*

@main def hello(): Unit =
  val input =
    """x = 5;
      |if (1) { x = 6; } else { x = 7; }
      |while (x) { x = x + x; x = x - 1; }
      |""".stripMargin

  MiniJSParser.parseAll(MiniJSParser.repl, input) match
    case MiniJSParser.Success(result, _) =>
      println("PARSED AST:")
      println(result)
      println("JSON:")
      println(replToJSON(result))
    case failure =>
      println("Parse failed:")
      println(failure)