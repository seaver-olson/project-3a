import Statement.*
import Evaluator.*

class MySuite extends munit.FunSuite {

  def parse(input: String): Repl =
    MiniJSParser.parseAll(MiniJSParser.repl, input) match
      case MiniJSParser.Success(result, _) => result
      case f => fail(s"Expected parse success but got: $f")

  def evalFresh(input: String): (Value, Memory) =
    val mem = emptyMem
    (evalRepl(parse(input), mem), mem)

  // ─── Parser ───────────────────────────────────────────────────────────────

  test("parse simple assignment") {
    assertEquals(parse("x = 5;"), Repl(List(Assignment(Variable("x"), Constant(5)))))
  }

  test("parse if-else block") {
    assertEquals(
      parse("if (1) { x = 2; } else { x = 3; }"),
      Repl(List(If(
        Constant(1),
        Block(List(Assignment(Variable("x"), Constant(2)))),
        Some(Block(List(Assignment(Variable("x"), Constant(3)))))
      )))
    )
  }

  test("parse while loop") {
    assertEquals(
      parse("while (x) { x = x - 1; }"),
      Repl(List(While(
        Variable("x"),
        Block(List(Assignment(Variable("x"), Minus(Variable("x"), Constant(1)))))
      )))
    )
  }

  test("parse operator precedence: multiplication before addition") {
    assertEquals(
      parse("2 + 3 * 4;"),
      Repl(List(ExprStatement(Plus(Constant(2), Times(Constant(3), Constant(4))))))
    )
  }

  test("parse multiple statements") {
    assertEquals(
      parse("x = 1; y = 2;"),
      Repl(List(Assignment(Variable("x"), Constant(1)), Assignment(Variable("y"), Constant(2))))
    )
  }

  test("reject missing semicolon") {
    MiniJSParser.parseAll(MiniJSParser.repl, "x = 5") match
      case MiniJSParser.Success(_, _) => fail("expected parser to reject malformed input")
      case _ => assert(true)
  }

  test("reject if without block") {
    MiniJSParser.parseAll(MiniJSParser.repl, "if (1) x = 2;") match
      case MiniJSParser.Success(_, _) => fail("expected parser to reject if without block")
      case _ => assert(true)
  }

  // ─── Unparser ─────────────────────────────────────────────────────────────

  test("unparse simple assignment") {
    assertEquals(unparseRepl(Repl(List(Assignment(Variable("x"), Constant(5))))), "x = 5;")
  }

  test("unparse if-else block") {
    val repl = Repl(List(If(
      Constant(1),
      Block(List(Assignment(Variable("x"), Constant(2)))),
      Some(Block(List(Assignment(Variable("x"), Constant(3)))))
    )))
    assertEquals(unparseRepl(repl), "if (1) {\n  x = 2;\n} else {\n  x = 3;\n}")
  }

  test("unparse while loop") {
    val repl = Repl(List(While(
      Variable("x"),
      Block(List(Assignment(Variable("x"), Minus(Variable("x"), Constant(1)))))
    )))
    assertEquals(unparseRepl(repl), "while (x) {\n  x = (x - 1);\n}")
  }

  // ─── Evaluator ────────────────────────────────────────────────────────────

  test("eval arithmetic") {
    val (v, _) = evalFresh("2 + 3 * 4;")
    assertEquals(v, Value.Num(14))
  }

  test("eval assignment stores and reads variable") {
    val (v, mem) = evalFresh("x = 7; x;")
    assertEquals(mem("x"), Value.Num(7))
    assertEquals(v, Value.Num(7))
  }

  test("eval if-else true branch") {
    val (_, mem) = evalFresh("if (1) { x = 10; } else { x = 20; }")
    assertEquals(mem("x"), Value.Num(10))
  }

  test("eval while loop counts down") {
    val (_, mem) = evalFresh("x = 3; y = 0; while (x) { y = y + x; x = x - 1; }")
    assertEquals(mem("y"), Value.Num(6))
    assertEquals(mem("x"), Value.Num(0))
  }

  test("eval undefined variable throws") {
    intercept[NoSuchFieldException] {
      evalFresh("z;")
    }
  }
}