package com.uclid.commandline

import org.junit.Test
import java.io.File

class Error {

  @Test def testSyntaxErrors(): Unit = {
    val tests = new File("models/tests/errors/syntactic").listFiles
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

  @Test def testSemanticErrors(): Unit = {
    val tests = new File("models/tests/errors/semantic").listFiles
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

  @Test def testNotSupportedYetErrors(): Unit = {
    val tests = new File("models/tests/unsupported").listFiles
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
