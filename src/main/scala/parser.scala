import scala.util.parsing.combinator.JavaTokenParsers
import Statement.*

trait MiniJSExprParser[Result] extends JavaTokenParsers:

  /**
   * Enable missing typesafe equality for `~`.
   * TODO remove once the combinator parser library provides this.
   */
  given [A, B](using CanEqual[A, A], CanEqual[B, B]): CanEqual[A ~ B, A ~ B] = CanEqual.derived
  
  // ident      ::= letter { letter | digit | "_" }*
  def identifier: Parser[String] = 
    """[a-zA-Z][a-zA-Z0-9_]*""".r 
  
  // number     ::= digit { digit }*
  def number: Parser[String] =
    """\d+""".r

  // expression ::= term { ("+" | "-") term }*
  def expr: Parser[Result] = 
    term ~! rep(("+" | "-") ~ term) ^^ onExpr

  // term       ::= factor { ("*" | "/" | "%") factor }* (corrected expression and term to allow for multiple terms and exps)
  def term: Parser[Result] = 
    factor ~! rep(("*" | "/" | "%") ~ factor) ^^ onTerm

  // factor ::= number | ident | "+" factor | "-" factor | "(" expr ")"
  def factor: Parser[Result] = 
    number ^^ onNumber
    | identifier ^^ onIdentifier
    | "+" ~> factor ^^ onPlusFactor
    | "-" ~> factor ^^ onMinusFactor
    | "(" ~> expr <~ ")" ^^ onParenExpr

  def onExpr: Result ~ List[String ~ Result] => Result
  def onTerm: Result ~ List[String ~ Result] => Result
  def onIdentifier: String => Result
  def onNumber: String => Result
  def onPlusFactor: Result => Result
  def onMinusFactor: Result => Result
  def onParenExpr: Result => Result
  
end MiniJSExprParser

object MiniJSParser extends MiniJSExprParser[Statement]:

  // repl ::= statement*
  def repl: Parser[Repl] = 
    rep(statement) ^^ onRepl

  // statement ::= expression ";" | conditional | loop | assignment | block
  def statement: Parser[Statement] =
    conditional | loop | assignment | block | exprStatement

  // assignment ::= identifier "=" expression ";"
  def assignment: Parser[Statement] =
    identifier ~ ("=" ~> expr) <~ ";" ^^ onAssignment

  // expression ";"
  def exprStatement: Parser[Statement] =
    expr <~ ";" ^^ onExprStatement
  // conditional ::= "if" "(" expression ")" block [ "else" block ]
  def conditional: Parser[Statement] =
    ("if" ~> "(" ~> expr <~ ")") ~ block ~ opt("else" ~> block) ^^ onConditional
  // loop ::= "while" "(" expression ")" block
  def loop: Parser[Statement] =
    ("while" ~> "(" ~> expr <~ ")") ~ block ^^ onLoop
  // block ::= "{" statement* "}"
  def block: Parser[Statement] =
    "{" ~> rep(statement) <~ "}" ^^ onBlock

  override def onExpr: Statement ~ List[String ~ Statement] => Statement =
    case first ~ rest => rest.foldLeft(first):
        case (acc, "+" ~ rhs) => Plus(acc, rhs)
        case (acc, "-" ~ rhs) => Minus(acc, rhs)

  override def onTerm: Statement ~ List[String ~ Statement] => Statement =
    case first ~ rest =>
      rest.foldLeft(first):
        case (acc, "*" ~ rhs) => Times(acc, rhs)
        case (acc, "/" ~ rhs) => Div(acc, rhs)
        case (acc, "%" ~ rhs) => Mod(acc, rhs)

  override def onNumber: String => Statement =
    s => Constant(s.toInt)

  override def onIdentifier: String => Statement =
    name => Variable(name)

  override def onPlusFactor: Statement => Statement =
    e => UPlus(e)

  override def onMinusFactor: Statement => Statement =
    e => UMinus(e)

  override def onParenExpr: Statement => Statement =
    e => e
  
  def onExprStatement: Statement => Statement =
    e => ExprStatement(e)

  def onAssignment: String ~ Statement => Statement =
    case name ~ value => Assignment(Variable(name), value)

  def onConditional: Statement ~ Statement ~ Option[Statement] => Statement =
    case cond ~ thenBlock ~ elseBlock => If(cond, thenBlock, elseBlock)

  def onLoop: Statement ~ Statement => Statement =
    case guard ~ body => While(guard, body)

  def onBlock: List[Statement] => Statement =
    statements => Block(statements)

  def onRepl: List[Statement] => Repl =
    statements => Repl(statements)

end MiniJSParser