package com.uclid.commandline

import org.junit.Test

import java.io.File

class Performance {

  @Test def testPerformance(): Unit = {
    val tests = new File("models/tests/performance").listFiles
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running: " + t._1)
      endToEnd(t._1, t._2, t._8).foreach { answer =>
        assert(
          t._3 == answer.presult.result,
          s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.presult.result}\nOutput: ${answer.presult.messages}"
        )
        assert(
          !t._4.isDefined || t._4.get >= answer.parseTime,
          s"Failed: ${t._1}\nExpected parse time less than: ${t._4.get}\nGot parse time: ${answer.parseTime}\nOutput: ${answer.presult.messages}"
        )
        assert(
          !t._5.isDefined || t._5.get >= answer.processTime,
          s"Failed: ${t._1}\nExpected process time less than: ${t._5.get}\nGot process time: ${answer.processTime}\nOutput: ${answer.presult.messages}"
        )
        assert(
          !t._6.isDefined || t._6.get >= answer.generationTime,
          s"Failed: ${t._1}\nExpected generation time less than: ${t._6.get}\nGot generation time: ${answer.generationTime}\nOutput: ${answer.presult.messages}"
        )
        assert(
          !t._7.isDefined || t._7.get >= answer.solveTime,
          s"Failed: ${t._1}\nExpected solve time less than: ${t._7.get}\nGot solve time: ${answer.solveTime}\nOutput: ${answer.presult.messages}"
        )
      }
    }
  }
}
