package back

import uclid.SolverMismatchError
import middle._

import sys.process._
import java.io.{File, PrintWriter}

abstract class Solver() {

  def runProcess(in: String): (List[String], List[String], Int, Double) = {
    print("Running solver ... ")
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val t1 = System.nanoTime
    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Solver terminated in ${duration} seconds.\n")

    (out.reverse, err.reverse, exit, duration)
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

  def generateQuery(program: Interfaceable): String
  def getCommand(): String
  def parseAnswer(answer: String): String

  def solve(
    program: Interfaceable,
    run: Boolean,
    outFile: Option[String]
  ): (ProofResult, Double, Double) = {

    // need to call this first before checking if it is a synthesis query
    print("Generating query ... ")
    val t1 = System.nanoTime
    val query = generateQuery(program)
    val generationDuration = (System.nanoTime - t1) / 1e9d
    println(s"Query generated in ${generationDuration} seconds.")

    val suffix = if (program.isSynthesisQuery) { ".sl" }
    else { ".smt2" }
    val qfile =
      writeQueryToFile(query, outFile, suffix).getAbsolutePath()

    if (!run || (!program.checkQuery && !program.traceQuery)) {
      return (
        new ProofResult(
          None,
          " Opted out of running the solver."
        ),
        generationDuration,
        0
      )
    }

    val result = runProcess(s"${getCommand()} ${qfile}")
    val answer = parseAnswer(" " + (result._1 ++ result._2).mkString("\n"))

    if (answer.contains("error") || answer.contains(
          "unknown"
        )) {
      (new ProofResult(None, answer), generationDuration, result._4)
    } else {
      if (program.traceQuery) {
        (new ProofResult(None, answer), generationDuration, result._4)
      } else {
        if ("(\\ssat)".r.findFirstIn(answer).isDefined) {
          (new ProofResult(Some(true), answer), generationDuration, result._4)
        } else {
          if (program.isSynthesisQuery) {
            (
              new ProofResult(Some(false), answer),
              generationDuration,
              result._4
            )
          } else {
            (
              new ProofResult(Some(false), " unsat"),
              generationDuration,
              result._4
            )
          }
        }
      }
    }
  }
}

class CVC4Solver() extends Solver() {
  def getCommand(): String = "cvc4"

  def generateQuery(program: Interfaceable): String = {
    val query = program.programToQuery()
    query
  }

  def parseAnswer(answer: String): String =
    answer
      .split("\n")
      .filter(p =>
        !p.startsWith(
          "(error \"Cannot get the current model unless immediately preceded by SAT/INVALID or UNKNOWN response.\")"
        ) && !p.startsWith(
          "(error \"Cannot get the current assignment unless immediately preceded by SAT/INVALID or UNKNOWN response.\")"
        )
      )
      .mkString("\n")
}

class Z3Solver() extends Solver() {
  def getCommand(): String = "z3"

  def generateQuery(program: Interfaceable): String = {
    // get the query but remove the set logic command
    val query = program
      .programToQuery()
      .split("\n")
      .filter(p => !p.startsWith("(set-logic"))
      .mkString("\n")
    if (program.isSynthesisQuery) {
      throw new SolverMismatchError("Z3 does not support synthesis")
    }
    query
  }

  def parseAnswer(answer: String): String =
    answer
      .split("\n")
      .filter(p =>
        !p.contains(
          "model is not available"
        )
      )
      .mkString("\n")
}

class AltErgoSolver() extends Solver() {
  def getCommand(): String = "alt-ergo -enable-adts-cs"

  def generateQuery(program: Interfaceable): String = {
    // get the query but remove the set logic and set-option commands
    val query = program
      .programToQuery()
      .split("\n")
      .filter(p =>
        !(p.startsWith("(set-logic") || p.startsWith("(set-option") || p
          .startsWith("(get-"))
      )
      .mkString("\n")
    if (program.isSynthesisQuery) {
      throw new SolverMismatchError("Alt-Ergo does not support synthesis")
    }
    query
  }

  def parseAnswer(answer: String): String =
    answer
}

class VampireSolver() extends Solver() {

  val supportedLogics = List(
    "ALIA",
    "AUFDTLIA",
    "AUFDTLIRA",
    "AUFDTNIRA",
    "AUFLIA",
    "AUFLIRA",
    "AUFNIA",
    "AUFNIRA",
    "LIA",
    "LRA",
    "NIA",
    "NRA",
    "UF",
    "UFDT",
    "UFDTLIA",
    "UFDTLIRA",
    "UFDTNIA",
    "UFDTNIRA",
    "UFIDL",
    "UFLIA",
    "UFLRA",
    "UFNIA"
  )

  def getCommand(): String =
    "vampire --mode smtcomp --input_syntax smtlib2 --term_algebra_acyclicity light --term_algebra_rules on --fmb_enumeration_strategy smt"

  def generateQuery(program: Interfaceable): String = {
    val query = program.programToQuery()

    // find the set logic command
    val pattern = "(?<=\\(set-logic )(.*)(?=\\))".r
    val logic = pattern.findFirstIn(query)

    if (logic.isDefined && !supportedLogics.contains(logic.get)) {
      throw new SolverMismatchError(s"Vampire does not support ${logic.get}")
    }

    if (program.isSynthesisQuery) {
      throw new SolverMismatchError("Vampire does not support synthesis")
    }

    query
  }

  def parseAnswer(answer: String): String =
    answer
}

class ProofResult(
  var result: Option[Boolean] = None,
  var messages: String = ""
) {

  override def toString(): String = {
    val extra = "Detailed Output:" + messages
    val answer = result match {
      case Some(true)  => "Rejected!"
      case Some(false) => "Verified!"
      case None        => "Neither Verified Nor Rejected."
    }
    List(
      s"\n${"*" * answer.length()}\n" + answer + s"\n${"*" * answer.length()}\n",
      extra
    ).mkString("\n")
  }
}
