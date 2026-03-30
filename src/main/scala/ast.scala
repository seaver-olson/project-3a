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

    case ExprStatement(expr: Statement)
    case Assignment(left: Statement, right: Statement)
    case If(cond: Statement, thenBlock: Statement, elseBlock: Option[Statement])
    case While(guard: Statement, body: Statement)
    case Block(statements: List[Statement])

case class Repl(statements: List[Statement]) derives CanEqual