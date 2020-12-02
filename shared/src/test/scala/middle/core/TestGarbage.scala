package middle.core

import org.junit.Test
import org.junit.Assert._

import interface.out.smt.toSmtString

class TestGarbage {

  @Test def testMark1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+", List()) // 3
    val integer = TheorySort("Int", List()) // 4

    val f = new Program(Array(head, body, arg, plus, integer))

    val marks = garbage.mark(f)

    val msg = marks.zipWithIndex.mkString("\n")

    marks.foreach(m => assert(m, msg))
  }

  @Test def testMark3(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+", List()) // 3
    val integer = TheorySort("Int", List()) // 4
    val string = TheorySort("String", List()) // 5

    val f = new Program(Array(head, body, arg, plus, integer, string))

    val marks = garbage.mark(f)

    val msg = marks.zipWithIndex.mkString("\n")

    marks.slice(0, marks.length - 1).foreach(m => assert(m, msg))

    assert(!marks(marks.length - 1), msg)
  }

  @Test def testSweep1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4
    val string = TheorySort("String") // 5

    val f = new Program(Array(head, body, arg, plus, integer, string))

    val marks = garbage.mark(f)

    val sweeped = garbage.sweep(f, marks)

    assert(toSmtString(sweeped) == toSmtString(f), sweeped)
    assert(sweeped.toString != f.toString, sweeped)
  }
}
