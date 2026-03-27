import Statement.*

def toJSON(stmt: Statement, indent: Int = 0): String =
  val pad = " " * indent
  val pad2 = " " * (indent + 2)
  stmt match
    case Constant(value) =>
      s"""${pad}{ "type": "Constant", "value": $value }"""
    case Variable(name) =>
      s"""${pad}{ "type": "Variable", "name": "$name" }"""
    case UPlus(expr) =>
      s"""${pad}{\n${pad2}"type": "UPlus",\n${pad2}"expr": \n${toJSON(expr, indent + 2)}\n${pad}}"""
    case UMinus(expr) =>
      s"""${pad}{\n${pad2}"type": "UMinus",\n${pad2}"expr": \n${toJSON(expr, indent + 2)}\n${pad}}"""
    case Plus(left, right) =>
      s"""${pad}{\n${pad2}"type": "Plus",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case Minus(left, right) =>
      s"""${pad}{\n${pad2}"type": "Minus",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case Times(left, right) =>
      s"""${pad}{\n${pad2}"type": "Times",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case Div(left, right) =>
      s"""${pad}{\n${pad2}"type": "Div",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case Mod(left, right) =>
      s"""${pad}{\n${pad2}"type": "Mod",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case ExprStmt(expr) =>
      s"""${pad}{\n${pad2}"type": "ExprStmt",\n${pad2}"expr": \n${toJSON(expr, indent + 2)}\n${pad}}"""
    case Assignment(left, right) =>
      s"""${pad}{\n${pad2}"type": "Assignment",\n${pad2}"left": \n${toJSON(left, indent + 2)},\n${pad2}"right": \n${toJSON(right, indent + 2)}\n${pad}}"""
    case If(cond, thenBlock, elseBlock) =>
      val elseStr = elseBlock match
        case Some(b) => s",\n${pad2}\"else\": \n${blockToJSON(b, indent + 2)}"
        case None => ""
      s"""${pad}{\n${pad2}"type": "If",\n${pad2}"cond": \n${toJSON(cond, indent + 2)},\n${pad2}"then": \n${blockToJSON(thenBlock, indent + 2)}${elseStr}\n${pad}}"""
    case While(guard, body) =>
      s"""${pad}{\n${pad2}"type": "While",\n${pad2}"guard": \n${toJSON(guard, indent + 2)},\n${pad2}"body": \n${blockToJSON(body, indent + 2)}\n${pad}}"""

def blockToJSON(block: Block, indent: Int = 0): String =
  val pad = " " * indent
  val pad2 = " " * (indent + 2)
  val stmts = block.statements.map(s => toJSON(s, indent + 2)).mkString(",\n")
  s"""${pad}{\n${pad2}"type": "Block",\n${pad2}"statements": [\n${stmts}\n${pad2}]\n${pad}}"""

def replToJSON(repl: Repl, indent: Int = 0): String =
  val pad = " " * indent
  val pad2 = " " * (indent + 2)
  val stmts = repl.statements.map(s => toJSON(s, indent + 2)).mkString(",\n")
  s"""${pad}{\n${pad2}"type": "Repl",\n${pad2}"statements": [\n${stmts}\n${pad2}]\n${pad}}"""
