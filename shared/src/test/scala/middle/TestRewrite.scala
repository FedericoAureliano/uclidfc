package middle

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.ArrayBuffer

class TestRewrite {

  @Test def testInline1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(ArrayBuffer(head, body, arg, plus, integer), 0)

    // call it
    val one = new Program(ArrayBuffer(TheoryMacro("1")), 0)
    f(one)

    val inlined = Rewriter.inlineApplication(f, 0)

    assert(f.toString != inlined.toString(), inlined)

    val answerterm = "(+11)"
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )
  }

  @Test def testLetify1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(ArrayBuffer(head, body, arg, plus, integer), 0)
    val one = new Program(ArrayBuffer(TheoryMacro("1")), 0)
    f(one)

    val leted = Rewriter.letify(f, "test")

    val answerterm = "(f1)"
    assert(
      Interface
        .programToQueryTerm(leted, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${leted}\n${Interface.programToQueryTerm(leted, 0).replaceAll("( |\t|\n)+", "")}"
    )

    assert(
      leted.stmts.length == f.stmts.length + 2,
      s"${leted}\n${Interface.programToQueryTerm(leted, 0).replaceAll("( |\t|\n)+", "")}"
    )
  }

  @Test def testReduceDuplicates1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(ArrayBuffer(head, body, arg, plus, integer), 0)

    val yplusy = new Program(
      ArrayBuffer(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      ),
      0
    )
    f(yplusy)
    val inlined = Rewriter.inlineApplication(f, 0)

    Rewriter.reduceDuplicates(inlined)

    val answerterm = "(+(+yy)(+yy))"
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )
  }

  @Test def testReduceIndirection1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(ArrayBuffer(head, body, arg, plus, integer), 0)

    val yplusy = new Program(
      ArrayBuffer(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      ),
      0
    )
    f(yplusy)
    val inlined = Rewriter.inlineApplication(f, 0)

    Rewriter.reduceDuplicates(inlined)

    val answerterm = "(+(+yy)(+yy))"
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )

    Rewriter.reduceIndirection(inlined)
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )
  }

  @Test def testReduceThenCollect1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new Program(ArrayBuffer(head, body, arg, plus, integer), 0)

    val yplusy = new Program(
      ArrayBuffer(
        Application(Ref(1), List(Ref(2), Ref(2))),
        TheoryMacro("+"),
        UserFunction("y", Ref(3)),
        TheorySort("Int")
      ),
      0
    )
    f(yplusy)
    val inlined = Rewriter.inlineApplication(f, 0)

    Rewriter.reduceDuplicates(inlined)

    val answerterm = "(+(+yy)(+yy))"
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )

    Rewriter.reduceIndirection(inlined)
    assert(
      Interface
        .programToQueryTerm(inlined, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${inlined}\n${Interface.programToQueryTerm(inlined, 0).replaceAll("( |\t|\n)+", "")}"
    )

    val cleaned = Garbage.collectGarbage(inlined)
    assert(
      Interface
        .programToQueryTerm(cleaned, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"${cleaned}\n${Interface.programToQueryTerm(cleaned, 0).replaceAll("( |\t|\n)+", "")}"
    )

    assert(
      cleaned.stmts.length < inlined.stmts.length,
      s"${cleaned}\n${Interface.programToQueryTerm(cleaned, 0).replaceAll("( |\t|\n)+", "")}"
    )
  }
}
