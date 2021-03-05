package com.uclid.termgraph

import org.junit.Test

import scala.collection.mutable.ArrayBuffer

class TestWellFormed {

  @Test def testCheckBounds1(): Unit = {
    // declare a function f(x) = x + x
    val head = UserMacro("f", 4, 1, List(2)) // 0
    val body = Application(3, List(2, 2)) // 1
    val arg = FunctionParameter("x", 4) // 2
    val plus = TheoryMacro("+") // 3
    val integer = TheorySort("Int") // 4

    val tg = new TermGraph()
    tg.getStmts().addAll(ArrayBuffer(head, body, arg, plus, integer))
    tg.checkRefBounds()
  }
}
