package uclid

import org.junit.Test
import org.junit.Assert.assertEquals
import back.ProofResult

class TestEndToEnd {

  def endToEnd(filename: String, solver: String): List[ProofResult] =
    UclidMain.main(UclidMain.parseOptions(Array(filename, "-s", solver)).get)

  @Test def testExamplefib(): Unit =
    assertEquals(Some(true), endToEnd("examples/fib.ucl", "cvc4").last.result)

  @Test def testExamplefib2safety(): Unit =
    assert(endToEnd("examples/fib2safety.ucl", "cvc4").isEmpty)

  @Test def testExamplefibarraysafety(): Unit =
    assertEquals(
      Some(false),
      endToEnd("examples/fibarraysafety.ucl", "z3").last.result
    )
}
