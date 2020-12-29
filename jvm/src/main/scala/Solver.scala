package back

import middle._

import sys.process._
import java.io.{File, PrintWriter}

class Solver(
  cmnd: String
) {

  def runProcess(in: String): (List[String], List[String], Int) = {
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)

    (out.reverse, err.reverse, exit)
  }

  def writeQueryToFile(
    query: String,
    outFile: Option[String],
    suffix: String
  ): File = {
    val f = outFile match {
      case Some(value) => new File(value)
      case None => {
        val tempFi = File.createTempFile("tmp", suffix)
        tempFi.deleteOnExit()
        tempFi
      }
    }

    new PrintWriter(f) {
      try {
        write(query)
      } finally {
        close()
      }
    }
    f
  }

  def solve(
    program: Interfaced,
    run: Boolean,
    outFile: Option[String]
  ): ProofResult = {

    val query = program.programToQuery()

    val suffix = if (program.isSynthesisQuery) { ".sl" }
    else { ".smt2" }
    val qfile =
      writeQueryToFile(query, outFile, suffix).getAbsolutePath()

    if (!run) {
      return new ProofResult(
        None,
        List("User opted out of running the solver.")
      )
    }

    val result = runProcess(s"$cmnd ${qfile}")
    val results = result._1 ++ result._2
    val answer = " " + (result._1 ++ result._2).mkString("\n")

    if (answer.contains("error") || answer.contains(
          "unknown"
        )) {
      new ProofResult(None, results)
    } else {
      if ("(\\ssat)".r.findFirstIn(answer).isDefined) {
        new ProofResult(Some(true), results)
      } else {
        new ProofResult(Some(false), results)
      }
    }
  }
}

class ProofResult(
  var result: Option[Boolean] = None,
  var messages: List[String] = List.empty
) {

  override def toString(): String = {
    val answer = result match {
      case Some(true)  => "Counterexample!"
      case Some(false) => "Verified!"
      case None        => "Problem!"
    }
    (List("*********\n" + answer + "\n*********\n\nSolver Output:") ++ messages)
      .mkString("\n")
  }
}
