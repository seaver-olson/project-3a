import Statement.*
import scala.collection.mutable

enum Value derives CanEqual:
  case Num(value: Int)

object Evaluator:
  
  type Memory = mutable.Map[String, Value]

  def emptyMem: Memory = mutable.Map.empty[String, Value]

  def valueToInt(value: Value): Int = value match
    case Value.Num(n) => n

  def isTrue(value: Value): Boolean = value match
    case Value.Num(n) => n != 0
  
  def eval(stmt: Statement, mem: Memory): Value = stmt match
    case Constant(value) => Value.Num(value)
    case Variable(name) => mem.getOrElse(name, throw new NoSuchFieldException(s"Variable $name not found"))
    case UPlus(expr) => Value.Num(valueToInt(eval(expr, mem)))
    case UMinus(expr) => Value.Num(-valueToInt(eval(expr, mem)))
    case Plus(left, right) => Value.Num(valueToInt(eval(left, mem)) + valueToInt(eval(right, mem)))
    case Minus(left, right) => Value.Num(valueToInt(eval(left, mem)) - valueToInt(eval(right, mem)))
    case Times(left, right) => Value.Num(valueToInt(eval(left, mem)) * valueToInt(eval(right, mem)))
    case Div(left, right) => Value.Num(valueToInt(eval(left, mem)) / valueToInt(eval(right, mem)))
    case Mod(left, right) => Value.Num(valueToInt(eval(left, mem)) % valueToInt(eval(right, mem)))
    case ExprStatement(expr) => eval(expr, mem)
    case Assignment(left, right) =>
      left match
        case Variable(name) =>
            mem.update(name, eval(right, mem))
            Value.Num(0)
        case _ => throw new IllegalArgumentException("Left-hand side of assignment must be a variable")
    case If(cond, thenBlock, elseBlock) =>
      if isTrue(eval(cond, mem)) then eval(thenBlock, mem)
        else elseBlock match
            case Some(stmt) => eval(stmt, mem)
            case None => Value.Num(0)
    case While(guard, body) =>
      while isTrue(eval(guard, mem)) do eval(body, mem)
      Value.Num(0)
    case Block(statements) =>
      var result: Value = Value.Num(0)
      for stmt <- statements do
        result = eval(stmt, mem)
      result
  def evalRepl(repl: Repl, mem: Memory): Value =
    var result: Value = Value.Num(0)
    for stmt <- repl.statements do
      result = eval(stmt, mem)
    result