package uclid

import org.junit.Test
import org.junit.Assert.fail
import back.ProofResult
import scala.io.Source
import java.io.File

class TestEndToEnd {

  def readTestFile(file: File): Option[(String, String, Option[Boolean])] = {
    val lines = Source.fromFile(file).getLines()
    if (lines.next().contains("UCLID TEST")) {
      val solver = "Solver=(.*)".r.findFirstIn(lines.next()) match {
        case Some("Solver=z3")   => "z3"
        case Some("Solver=cvc4") => "cvc4"
        case _ => {
          fail(s"Skipping ${file}: couldn't parse solver")
          return None
        }
      }
      val result = "Result=(.*)".r.findFirstIn(lines.next()) match {
        case Some("Result=Some(true)")  => Some(true)
        case Some("Result=Some(false)") => Some(false)
        case Some("Result=None")        => None
        case _ => {
          fail(s"Skipping ${file}: couldn't parse result")
          return None
        }
      }
      Some((file.getAbsolutePath(), solver, result))
    } else {
      println(s"Skipping ${file}: not declared as test")
      return None
    }
  }

  def endToEnd(filename: String, solver: String): ProofResult =
    UclidMain.main(UclidMain.parseOptions(Array(filename, "-s", solver)).get)

  @Test def testFeatures(): Unit = {
    val tests = new File("models/tests/features").listFiles
      .filter(_.isFile)
      .map(f => readTestFile(f))
      .flatten
    tests.foreach { t =>
      val answer = endToEnd(t._1, t._2)
      assert(
        t._3 == answer.result,
        s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.result}\nOutput: ${answer.messages}"
      )
    }
  }

  @Test def testSyntaxErrors(): Unit = {
    val tests = new File("models/tests/errors/syntactic").listFiles
      .filter(_.isFile)
      .map(f => readTestFile(f))
      .flatten
    tests.foreach { t =>
      val answer = endToEnd(t._1, t._2)
      assert(
        t._3 == answer.result,
        s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.result}\nOutput: ${answer.messages}"
      )
    }
  }

  @Test def testSemanticErrors(): Unit = {
    val tests = new File("models/tests/errors/semantic").listFiles
      .filter(_.isFile)
      .map(f => readTestFile(f))
      .flatten
    tests.foreach { t =>
      val answer = endToEnd(t._1, t._2)
      assert(
        t._3 == answer.result,
        s"Failed: ${t._1}\nExpected: ${t._3}\nGot: ${answer.result}\nOutput: ${answer.messages}"
      )
    }
  }
}
