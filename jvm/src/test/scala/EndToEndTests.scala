package uclid

import org.junit.Test
import org.junit.Assert.assertEquals
import middle.ProofState

class TestEndToEnd {
  // TODO: add control blocks and Z3/CVC4 in CI

  def endToEnd(filename: String): ProofState =
    UclidMain.main(UclidMain.parseOptions(Array(filename)).get)

  @Test def testExamplefib(): Unit =
    assertEquals(Some(false), endToEnd("examples/fib.ucl").result)

  @Test def testExamplefib2safety(): Unit =
    assertEquals(Some(false), endToEnd("examples/fib2safety.ucl").result)

  @Test def testExamplefibarraysafety(): Unit =
    assertEquals(Some(false), endToEnd("examples/fibarraysafety.ucl").result)
}
