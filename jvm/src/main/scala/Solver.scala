package back

import middle._

import sys.process._
import java.io.{File, PrintWriter}

class Solver(
  cmnd: String,
  val extraOpts: List[(String, String)]
) {

  val options =
    extraOpts ++ List(("produce-models", "true"), ("dump-models", "true"))

  def runProcess(in: String): (List[String], List[String], Int) = {
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)

    (out.reverse, err.reverse, exit)
  }

  def writeQueryToFile(
    query: String,
    outFile: Option[String]
  ): File = {
    val f = outFile match {
      case Some(value) => new File(value)
      case None => {
        val tempFi = File.createTempFile("tmp", ".smt2")
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
    val logic = s"(set-logic ${program.inferLogic()})"
    val queryOpts = options ++ program.options
    val query = program.programToQuery()

    val processedQuery =
      (List(logic) ++ queryOpts.map(o => s"(set-option :${o._1} ${o._2})") ++ List(
        query
      )).mkString("\n")

    val qfile = writeQueryToFile(processedQuery, outFile).getAbsolutePath()

    if (!run) {
      return new ProofResult(
        None,
        List("User opted out of running the solver.")
      )
    }

    val result = runProcess(s"$cmnd ${qfile}")

    val answer = " " + result._1.mkString("\n")

    if (!result._2.isEmpty || answer.contains("error") || answer.contains(
          "unknown"
        )) {
      new ProofResult(None, result._1)
    } else {
      if ("(\\ssat)".r.findFirstIn(answer).isDefined) {
        new ProofResult(Some(true), result._1)
      } else {
        new ProofResult(Some(false), result._1)
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
      case Some(true)  => "Counterexample:"
      case Some(false) => "Verification Passes:"
      case None        => "Not Solved:"
    }
    (List(answer) ++ messages)
      .mkString("\n")
  }
}
