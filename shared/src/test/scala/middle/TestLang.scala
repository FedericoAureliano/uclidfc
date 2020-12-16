package middle

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.ArrayBuffer

class TestLang {

  @Test def testFunctionApplication1(): Unit = {
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

    val answerterm = "(f1)"
    val answerdef = "(define-funf((xInt))Int(+xx))"

    assert(
      Interface
        .programToQueryTerm(f, 0)
        .replaceAll("( |\t|\n)+", "") == answerterm,
      s"\n1\n${f}\n${Interface.programToQueryTerm(f, 0).replaceAll("( |\t|\n)+", "")}\n"
    )
    assert(
      Interface.programToQueryCtx(f).replaceAll("( |\t|\n)+", "") == answerdef,
      s"\n2\n${f}\n${Interface.programToQueryCtx(f).replaceAll("( |\t|\n)+", "")}\n"
    )
  }
}
