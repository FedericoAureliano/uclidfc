package uclid

import org.junit.Test
import org.junit.Assert.assertEquals

class TestEndToEnd {

  def endToEnd(filename: String): Int =
    UclidMain.main(UclidMain.parseOptions(Array(filename)).get)._2

  @Test def testExamplefib(): Unit =
    assertEquals(0, endToEnd("examples/fib.ucl"))

  @Test def testExamplefib2safety(): Unit =
    assertEquals(0, endToEnd("examples/fib2safety.ucl"))

  @Test def testExamplefibarraysafety(): Unit =
    assertEquals(0, endToEnd("examples/fibarraysafety.ucl"))
}
