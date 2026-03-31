import Statement.*
import scala.io.StdIn.readLine

@main def hello(): Unit =
  println("MiniJS REPL -- Team 14 -- enter statements followed by a blank line, :quit to exit")
  replLoop()

def replLoop(): Unit =
  val lines = parseLines()
  lines match
    case None =>
      // user tried to quit
      println("GOODBYE!")
    case Some(input) if input.isBlank =>
      replLoop()
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
      replLoop()
def parseLines(): Option[String] =
  print("MiniJS >>> ")
  readLine() match
    case null      => None
    case ":quit"   => None
    case first if first.isBlank => Some(first)
    case first =>
      val rest = parseContLines()
      rest.map(r => (first + "\n" + r).trim)//combine lines
    
def parseContLines(): Option[String] =
  print("  | ")//shows difference between one liner and cont lines
  readLine() match
    case null      => Some("")
    case ":quit"   => None
    case line if line.isBlank => Some("")
    case line =>
      parseContLines().map(rest =>
        if rest.isBlank then line
        else line + "\n" + rest
      )
      