package com.uclid.commandline

import org.junit.Test
import java.io.File

class Rewrite {

  @Test def testRewrites(): Unit = {
    val tests = (new File("models/tests/rewrites").listFiles)
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running: " + t._1)
      val answerWithout = endToEnd(t._1, t._2, List.empty)
      val answerWith = endToEnd(t._1, t._2, t._8)
      assert(
        answerWithout.presult.result == answerWith.presult.result,
        s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answerWithout.presult.result} and ${answerWith.presult.result}\nOutput: ${answerWith.presult.messages} and ${answerWithout.presult.messages}"
      )
    }
  }
}
