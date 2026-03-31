import Statement.*

def indent(level: Int): String = " " * level

def unparseExpr(expr: Statement): String = expr match
  case Constant(value) => value.toString
  case Variable(name) => name
  case UPlus(inner) => s"+${unparseExpr(inner)}"
  case UMinus(inner) => s"-${unparseExpr(inner)}"
  case Plus(left, right) => s"(${unparseExpr(left)} + ${unparseExpr(right)})"
  case Minus(left, right) => s"(${unparseExpr(left)} - ${unparseExpr(right)})"
  case Times(left, right) => s"(${unparseExpr(left)} * ${unparseExpr(right)})"
  case Div(left, right) => s"(${unparseExpr(left)} / ${unparseExpr(right)})"
  case Mod(left, right) => s"(${unparseExpr(left)} % ${unparseExpr(right)})"
  case other =>
    throw new RuntimeException(s"Cannot unparse expression node: $other")

def unparseBlock(stmt: Statement, indentLevel: Int): String = stmt match
  case Block(statements) =>
    val inner = statements.map(unparseStatement(_, indentLevel + 2)).mkString("\n")
    val pad = indent(indentLevel)
    if inner.isBlank then
      s"${pad}{\n${pad}}"
    else
      s"${pad}{\n${inner}\n${pad}}"
  case other =>
    val pad = indent(indentLevel)
    s"${pad}{\n${unparseStatement(other, indentLevel + 2)}\n${pad}}"

def unparseStatement(stmt: Statement, indentLevel: Int = 0): String =
  val pad = indent(indentLevel)
  stmt match
    case ExprStatement(expr) => s"${pad}${unparseExpr(expr)};"
    case Assignment(Variable(name), value) => s"${pad}${name} = ${unparseExpr(value)};"
    case Assignment(left, _) =>
      throw new RuntimeException(s"Invalid assignment left-hand side: $left")
    case If(cond, thenBlock, elseBlock) =>
      val thenText = unparseBlock(thenBlock, indentLevel)
      val elseText = elseBlock match
        case Some(b) => s" else ${unparseBlock(b, indentLevel)}"
        case None => ""
      s"${pad}if (${unparseExpr(cond)}) ${thenText}${elseText}"
    case While(guard, body) =>
      s"${pad}while (${unparseExpr(guard)}) ${unparseBlock(body, indentLevel)}"
    case Block(statements) =>
      val inner = statements.map(unparseStatement(_, indentLevel + 2)).mkString("\n")
      if inner.isBlank then
        s"${pad}{\n${pad}}"
      else
        s"${pad}{\n${inner}\n${pad}}"
    case other =>
      throw new RuntimeException(s"Cannot unparse statement node: $other")

def unparseRepl(repl: Repl): String =
  repl.statements.map(unparseStatement(_, 0)).mkString("\n")
