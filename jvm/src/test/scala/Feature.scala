package uclid

import TestHelper._

import org.junit.Test
import java.io.File

class Feature {

  @Test def testFeatures(): Unit = {
    val tests = (new File("models/tests/features").listFiles ++ new File(
      "models/examples"
    ).listFiles)
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running:", t._1)
      val answer = endToEnd(t._1, t._2, t._8)
      assert(
        t._3 == answer.presult.result,
        s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.presult.result}\nOutput: ${answer.presult.messages}"
      )
    }
  }
}
