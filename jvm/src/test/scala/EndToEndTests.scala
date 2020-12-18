package uclid

import org.junit.Test
import org.junit.Assert.assertEquals
import back.ProofResult

class TestEndToEnd {

  def endToEnd(filename: String, solver: String): ProofResult =
    UclidMain.main(UclidMain.parseOptions(Array(filename, "-s", solver)).get)

  @Test def testExamplefib(): Unit =
    assertEquals(Some(true), endToEnd("examples/fib.ucl", "cvc4").result)

  @Test def testExamplefib2safety(): Unit =
    assertEquals(
      Some(false),
      endToEnd("examples/fib2safety.ucl", "cvc4").result
    )

  @Test def testExamplefibarraysafety(): Unit =
    assertEquals(
      Some(false),
      endToEnd("examples/fibarraysafety.ucl", "z3").result
    )
}
