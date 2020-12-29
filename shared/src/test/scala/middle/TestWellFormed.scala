package middle

import middle.RefOutOfBoundsError

import org.junit.Test
import scala.collection.mutable.ArrayBuffer

class TestWellFormed {

  @Test(expected = classOf[RefOutOfBoundsError])
  def testCheckBounds1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(-1)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()

  }

  @Test(expected = classOf[RefOutOfBoundsError])
  def testCheckBounds2(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(5), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()

  }

  @Test def testCheckBounds3(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", Ref(4), Ref(1), List(Ref(2))) // 0
    val body = Application(Ref(3), List(Ref(2), Ref(2))) // 1
    val arg = FunctionParameter("x", Ref(4)) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val f = new WellFormed(ArrayBuffer(head, body, arg, plus, integer))

    f.checkRefBounds()
  }
}
