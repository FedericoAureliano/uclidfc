package uclid

import org.junit.Test
import org.junit.Assert.assertEquals
import back.ProofResult

class TestEndToEnd {
  // TODO: add control blocks and Z3/CVC4 in CI

  def endToEnd(filename: String): List[ProofResult] =
    UclidMain.main(UclidMain.parseOptions(Array(filename)).get)

  @Test def testExamplefib(): Unit =
    assertEquals(Some(true), endToEnd("examples/fib.ucl").last.result)

  @Test def testExamplefib2safety(): Unit =
    assert(endToEnd("examples/fib2safety.ucl").isEmpty)

  @Test def testExamplefibarraysafety(): Unit =
    assert(endToEnd("examples/fibarraysafety.ucl").isEmpty)
}
