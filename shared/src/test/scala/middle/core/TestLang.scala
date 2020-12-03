package middle.core

import org.junit.Test
import org.junit.Assert._
import scala.collection.mutable.ArrayBuffer

import interface.out.smt.{getSmtCtxString, getSmtTermString, toSmtString}

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

    val answerterm = "(f 1)"
    val answerdef = "(define-fun f ((x Int)) Int (+ x x))"

    assert(
      getSmtTermString(f) == answerterm,
      s"\n1\n${f}\n${toSmtString(f)}\n"
    )
    assert(
      getSmtCtxString(f) == answerdef,
      s"\n2\n${f}\n${toSmtString(f)}\n"
    )
  }
}
