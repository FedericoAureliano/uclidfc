package back

import middle._

import sys.process._
import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object Solver {

  def run(in: String): (List[String], List[String], Int) = {
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)

    (out.reverse, err.reverse, exit)
  }

  def writeQueryToTmpFile(
    query: String
  ): File = {
    val tempFi = File.createTempFile("tmp", ".smt2")
    tempFi.deleteOnExit()
    new PrintWriter(tempFi) {
      try {
        write(query)
      } finally {
        close()
      }
    }
    tempFi
  }

  def check(
    program: TermGraph,
    assertion: Ref,
    solver: String
  ): ProofResult = {
    val query = Printer.programToQuery(program)
    val qfile = writeQueryToTmpFile(query).getAbsolutePath()
    val result = run(s"$solver ${qfile}")

    val answer = result._1.mkString("\n")

    if (answer.contains("unsat")) {
      new ProofResult(Some(false), None, result._1)
    } else if (answer.contains("sat")) {
      new ProofResult(Some(true), None, result._1)
    } else {
      new ProofResult(None, None, result._1)
    }
  }

  def solve(ob: TermGraph, solver: String): List[ProofResult] = {
    val results = new ListBuffer[ProofResult]()

    ob.assertions.foreach { o =>
      results.addOne(check(ob, o, solver))

      // failed verification so quit
      if (results.last.result != Some(false)) {
        return results.toList
      }
    }

    results.toList
  }
}
