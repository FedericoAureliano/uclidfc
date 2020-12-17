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
    program: Program,
    name: String,
    solver: String
  ): ProofResult = {
    val query =
      "(set-logic ALL)\n(set-option :produce-models true)\n" ++ Interface
        .programToQuery(program) ++ "\n(check-sat)\n(get-model)"
    val qfile = writeQueryToTmpFile(query).getAbsolutePath()
    val result = run(s"$solver ${qfile}")

    val answer = result._1.mkString("\n")

    if (answer.contains("unsat")) {
      new ProofResult(program, name, Some(false), None, result._1)
    } else if (answer.contains("sat")) {
      new ProofResult(program, name, Some(true), None, result._1)
    } else {
      new ProofResult(program, name, None, None, result._1)
    }
  }

  def solve(ob: ProofTask, solver: String): List[ProofResult] = {
    val results = new ListBuffer[ProofResult]()

    ob.obligations.foreach { o =>
      ob.program.head = o._2.loc
      results.addOne(check(ob.program, o._1, solver))

      // failed verification so quit
      if (results.last.result != Some(false)) {
        return results.toList
      }
    }

    results.toList
  }
}
