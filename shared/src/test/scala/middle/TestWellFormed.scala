package middle

import middle.RefOutOfBoundsError

import org.junit.Test
import scala.collection.mutable.ArrayBuffer

class TestWellFormed {

  @Test(expected = classOf[RefOutOfBoundsError])
  def testCheckBounds1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4, None), Ref(1, None), List(Ref(2, None))) // 0
    val body = Application(Ref(3, None), List(Ref(2, None), Ref(2, None))) // 1
    val arg = FunctionParameter("x", Ref(-1, None)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()

  }

  @Test(expected = classOf[RefOutOfBoundsError])
  def testCheckBounds2(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4, None), Ref(1, None), List(Ref(2, None))) // 0
    val body = Application(Ref(3, None), List(Ref(5, None), Ref(2, None))) // 1
    val arg = FunctionParameter("x", Ref(4, None)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()

  }

  @Test def testCheckBounds3(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4, None), Ref(1, None), List(Ref(2, None))) // 0
    val body = Application(Ref(3, None), List(Ref(2, None), Ref(2, None))) // 1
    val arg = FunctionParameter("x", Ref(4, None)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()
  }
}
