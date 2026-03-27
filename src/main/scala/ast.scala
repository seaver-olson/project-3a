enum Statement derives CanEqual:
    case Constant(value: Int)
    case Variable(name: String)
    case UPlus(expr: Statement)
    case UMinus(expr: Statement)
    case Plus(left: Statement, right: Statement)
    case Minus(left: Statement, right: Statement)
    case Times(left: Statement, right: Statement)
    case Div(left: Statement, right: Statement)
    case Mod(left: Statement, right: Statement)

    case ExprStmt(expr: Statement)
    case Assignment(left: Statement, right: Statement)
    case If(cond: Statement, thenBlock: Block, elseBlock: Option[Block])
    case While(guard: Statement, body: Block)

case class Block(statements: List[Statement]) derives CanEqual
case class Repl(statements: List[Statement]) derives CanEqual