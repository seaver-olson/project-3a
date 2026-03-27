import Statement.*

@main def hello(): Unit =
  // Test 1: simple constant
  val t1 = Constant(5)
  println("Test 1:")
  println(toJSON(t1))

  // Test 2: x = 5
  val t2 = Assignment(Variable("x"), Constant(5))
  println("\nTest 2:")
  println(toJSON(t2))

  // Test 3: if (1) { x = 2; } else { x = 3; }
  val t3 = If(
    Constant(1),
    Block(List(Assignment(Variable("x"), Constant(2)))),
    Some(Block(List(Assignment(Variable("x"), Constant(3)))))
  )
  println("\nTest 3:")
  println(toJSON(t3))

  // Test 4: while (y) { r = r + x; }
  val t4 = While(
    Variable("y"),
    Block(List(Assignment(Variable("r"), Plus(Variable("r"), Variable("x")))))
  )
  println("\nTest 4:")
  println(toJSON(t4))
