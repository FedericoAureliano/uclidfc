package com.uclid.commandline

import org.junit.Test
import java.io.File

class Feature {

  @Test def testFeatures(): Unit = {
    val tests = (new File("models/tests/features").listFiles)
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running: " + t._1)
      endToEnd(t._1, t._2, t._8).foreach(answer => {
        assert(
          t._3 == answer.presult.result,
          s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.presult.result}\nOutput: ${answer.presult.messages}"
        )
      })
    }
  }
}
