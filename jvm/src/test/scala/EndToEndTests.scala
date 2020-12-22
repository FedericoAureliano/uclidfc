package uclid

import org.junit.Test
import org.junit.Assert.assertEquals
import back.ProofResult

class TestEndToEnd {

  def endToEnd(filename: String, solver: String): ProofResult =
    UclidMain.main(UclidMain.parseOptions(Array(filename, "-s", solver)).get)

  @Test def testInstance(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/instance.ucl", "cvc4").result
    )

  @Test def testInstanceArray(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/instancearray.ucl", "z3").result
    )

  @Test def testUSort(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/usort.ucl", "z3").result
    )

  @Test def testUFunc(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/ufunc.ucl", "z3").result
    )

  @Test def testConstLit(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/constlit.ucl", "z3").result
    )

  @Test def testDefine(): Unit =
    assertEquals(
      Some(false),
      endToEnd("models/features/define.ucl", "z3").result
    )

  @Test def testKInduction(): Unit =
    assertEquals(
      Some(true),
      endToEnd("models/features/kinduction.ucl", "cvc4").result
    )
}
