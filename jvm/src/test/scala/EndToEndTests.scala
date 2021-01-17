package uclid

import org.junit.Test
import scala.io.Source
import java.io.File

class TestEndToEnd {

  def readTestFile(file: File): (
    String,
    Option[String],
    Option[Boolean],
    Option[Double],
    Option[Double],
    Option[Double],
    Option[Double],
    List[String]
  ) = {
    val lines = Source.fromFile(file).getLines().mkString("\n")
    val solver = "(?<=Solver=)(.*)".r.findFirstIn(lines) match {
      case Some("z3")   => Some("z3")
      case Some("cvc4") => Some("cvc4")
      case _            => None
    }
    val result = "(?<=Result=)(.*)".r.findFirstIn(lines) match {
      case Some("Some(true)")  => Some(true)
      case Some("Some(false)") => Some(false)
      case _                   => None
    }
    val maxParseTime = "(?<=MaxParseTime=)(.*)".r.findFirstIn(lines) match {
      case Some(v) => Some(v.toDouble)
      case _       => None
    }
    val maxProcessTime = "(?<=MaxProcessTime=)(.*)".r.findFirstIn(lines) match {
      case Some(v) => Some(v.toDouble)
      case _       => None
    }
    val maxGenerationTime =
      "(?<=MaxGenerationTime=)(.*)".r.findFirstIn(lines) match {
        case Some(v) => Some(v.toDouble)
        case _       => None
      }
    val maxSolveTime = "(?<=MaxSolveTime=)(.*)".r.findFirstIn(lines) match {
      case Some(v) => Some(v.toDouble)
      case _       => None
    }
    val rewrites =
      "(?<=Rewrite=)(.*)".r.findAllIn(lines).map(r => "--" + r).toList
    (
      (
        file.getAbsolutePath(),
        solver,
        result,
        maxParseTime,
        maxProcessTime,
        maxGenerationTime,
        maxSolveTime,
        rewrites
      )
    )
  }

  def endToEnd(
    filename: String,
    solver: Option[String],
    rewrites: List[String]
  ): UclidResult =
    solver match {
      case Some(value) =>
        UclidMain.main(
          UclidMain.parseOptions(Array(filename, "-s", value) ++ rewrites).get
        )
      case None =>
        UclidMain.main(
          UclidMain.parseOptions(Array(filename, "-r", "false") ++ rewrites).get
        )
    }

  @Test def testCorrectness(): Unit = {
    val tests = (new File("models/tests/correctness").listFiles ++ new File(
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

  @Test def testRewrites(): Unit = {
    val tests = (new File("models/tests/rewrites").listFiles ++ new File(
      "models/examples"
    ).listFiles)
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running:", t._1)
      val answerWithRewrite = endToEnd(t._1, t._2, t._8)
      val answerWithoutRewrite = endToEnd(t._1, t._2, List.empty)
      assert(
        answerWithoutRewrite.presult.result == answerWithRewrite.presult.result,
        s"Failed: ${t._1}\nWithout Rewrite: ${answerWithoutRewrite.presult.result}\nWith Rewrite: ${answerWithRewrite.presult.result}\nOutput on Rewrite: ${answerWithRewrite.presult.messages}"
      )
    }
  }

  @Test def testSyntaxErrors(): Unit = {
    val tests = new File("models/tests/errors/syntactic").listFiles
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

  @Test def testSemanticErrors(): Unit = {
    val tests = new File("models/tests/errors/semantic").listFiles
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

  @Test def testNotSupportedYetErrors(): Unit = {
    val tests = new File("models/tests/unsupported").listFiles
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

  @Test def testPerformance(): Unit = {
    val tests = new File("models/tests/performance").listFiles
      .filter(_.isFile)
      .map(f => readTestFile(f))
    tests.foreach { t =>
      println("Running:", t._1)
      val answer = endToEnd(t._1, t._2, t._8)
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
