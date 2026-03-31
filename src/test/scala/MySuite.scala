import Statement.*

class MySuite extends munit.FunSuite {
  test("parse simple assignment") {
    val input = "x = 5;"

    MiniJSParser.parseAll(MiniJSParser.repl, input) match
      case MiniJSParser.Success(result, _) =>
        assertEquals(result, Repl(List(Assignment(Variable("x"), Constant(5)))))
      case _ => fail("expected parser to succeed")
  }

  test("unparse simple assignment") {
    val repl = Repl(List(Assignment(Variable("x"), Constant(5))))
    assertEquals(unparseRepl(repl), "x = 5;")
  }

  test("parse if-else block") {
    val input = "if (1) { x = 2; } else { x = 3; }"

    MiniJSParser.parseAll(MiniJSParser.repl, input) match
      case MiniJSParser.Success(result, _) =>
        assertEquals(result, Repl(List(
          If(
            Constant(1),
            Block(List(Assignment(Variable("x"), Constant(2)))),
            Some(Block(List(Assignment(Variable("x"), Constant(3)))))
          )
        )))
      case _ => fail("expected parser to succeed")
  }

  test("unparse if-else block") {
    val repl = Repl(List(
      If(
        Constant(1),
        Block(List(Assignment(Variable("x"), Constant(2)))),
        Some(Block(List(Assignment(Variable("x"), Constant(3)))))
      )
    ))
    val expected = "if (1) {\n  x = 2;\n} else {\n  x = 3;\n}"
    assertEquals(unparseRepl(repl), expected)
  }

  test("reject missing semicolon") {
    val input = "x = 5"

    MiniJSParser.parseAll(MiniJSParser.repl, input) match
      case MiniJSParser.Success(_, _) => fail("expected parser to reject malformed input")
      case _ => assert(true)
  }
}
