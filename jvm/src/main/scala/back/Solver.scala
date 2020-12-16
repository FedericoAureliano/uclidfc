package back

import middle._

import sys.process._
import java.io.{File, PrintWriter}

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
    program: Program
  ): (Option[Boolean], Option[Ref], Option[String]) = {
    val query =
      Interface.programToQuery(program) ++ "\n(check-sat)\n(get-model)"
    val qfile = writeQueryToTmpFile(query).getAbsolutePath()
    val result = run(s"z3 ${qfile}")

    // TODO parse the output
    val answer = result._1.mkString("\n")

    if (answer.contains("unsat")) {
      (Some(false), None, Some(answer))
    } else if (answer.contains("sat")) {
      (Some(true), None, Some(answer))
    } else {
      (None, None, Some(answer))
    }
  }
}
