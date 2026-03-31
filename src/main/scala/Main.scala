import Statement.*
import org.jline.terminal.TerminalBuilder
import org.jline.reader.{LineReader, LineReaderBuilder, UserInterruptException, EndOfFileException}

@main def hello(): Unit =
  val terminal = TerminalBuilder.builder().system(true).build()
  val reader: LineReader = LineReaderBuilder.builder().terminal(terminal).build()
  println("MiniJS REPL -- Team 14 -- enter statements followed by a blank line, :quit to exit")
  replLoop(reader)

def replLoop(reader: LineReader): Unit =
  parseLines(reader) match
    case None =>
      // user tried to quit
      println("GOODBYE!")
    case Some(input) if input.isBlank =>
      replLoop(reader)
    case Some(input) =>
      println(s": $input")
      MiniJSParser.parseAll(MiniJSParser.repl, input) match
        case MiniJSParser.Success(result, _) =>
          println("\nThe parsed statements are:")
          println(result)
          println("\nJSON:")
          println(replToJSON(result))
        case MiniJSParser.Failure(msg, next) =>
          println(s"Parse error at '${next.pos}': $msg")
        case MiniJSParser.Error(msg, next) =>
          println(s"Fatal parse error at '${next.pos}': $msg")
      println()// whitespace
      replLoop(reader)
def parseLines(reader: LineReader): Option[String] =
  try
    val first = reader.readLine("MiniJS >>> ")
    if first == null || first == ":quit" then None
    else if first.isBlank then Some(first)
    else
      val rest = parseContLines(reader)
      rest.map(r => (first + "\n" + r).trim)
  catch
    case _: UserInterruptException => 
      println("^C")
      Some("") //Ctrl+C cancel out of multiLine
    case _: EndOfFileException => None // Ctrl+D
def parseContLines(reader: LineReader): Option[String] =
  try
    val line = reader.readLine("  | ")//shows difference between one liner and cont lines
    if line == null || line == ":quit" then None
    else if line.isBlank then Some("")
    else
      parseContLines(reader).map { rest =>
        if rest.isBlank then line else line + "\n" + rest
      }
  catch
    case _: UserInterruptException => 
      println("^C")
      Some("")
    case _: EndOfFileException     => Some("")