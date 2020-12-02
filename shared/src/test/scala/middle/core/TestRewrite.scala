package middle.core

import org.junit.Test
import org.junit.Assert._

import interface.out.smt.{getSmtTermString}

class TestRewrite {

  @Test def testInline1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(Array(head, body, arg, plus, integer))

    // call it
    val one = new Program(Array(TheoryMacro("1")))
    val fone = f(one)

    val inlined = rewrite.inlineApplication(fone, 0)

    assert(fone.toString != inlined.toString(), inlined)

    val answerterm = "(+ 1 1)"
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )
  }

  @Test def testLetify1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(Array(head, body, arg, plus, integer))
    val one = new Program(Array(TheoryMacro("1")))
    val fone = f(one)

    val leted = rewrite.letify(fone, "test")

    val answerterm = "(f 1)"
    assert(
      getSmtTermString(leted) == answerterm,
      s"${leted}\n${getSmtTermString(leted)}"
    )

    assert(
      leted.stmts.length == fone.stmts.length + 2,
      s"${leted}\n${getSmtTermString(leted)}"
    )
  }

  @Test def testReduceDuplicates1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(Array(head, body, arg, plus, integer))

    val yplusy = new Program(
      Array(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      )
    )
    val fapp = f(yplusy)
    val inlined = rewrite.inlineApplication(fapp, 0)

    rewrite.reduceDuplicates(inlined)

    val answerterm = "(+ (+ y y) (+ y y))"
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )
  }

  @Test def testReduceIndirection1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(Array(head, body, arg, plus, integer))

    val yplusy = new Program(
      Array(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      )
    )
    val fapp = f(yplusy)
    val inlined = rewrite.inlineApplication(fapp, 0)

    rewrite.reduceDuplicates(inlined)

    val answerterm = "(+ (+ y y) (+ y y))"
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )

    rewrite.reduceIndirection(inlined)
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )
  }

  @Test def testReduceThenCollect1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(Array(head, body, arg, plus, integer))

    val yplusy = new Program(
      Array(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      )
    )
    val fapp = f(yplusy)
    val inlined = rewrite.inlineApplication(fapp, 0)

    rewrite.reduceDuplicates(inlined)

    val answerterm = "(+ (+ y y) (+ y y))"
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )

    rewrite.reduceIndirection(inlined)
    assert(
      getSmtTermString(inlined) == answerterm,
      s"${inlined}\n${getSmtTermString(inlined)}"
    )

    val cleaned = garbage.collectGarbage(inlined)
    assert(
      getSmtTermString(cleaned) == answerterm,
      s"${cleaned}\n${getSmtTermString(cleaned)}"
    )

    assert(
      cleaned.stmts.length < inlined.stmts.length,
      s"${cleaned}\n${getSmtTermString(cleaned)}"
    )
  }
}
