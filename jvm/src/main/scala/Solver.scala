package back

import uclid.SolverMismatchError
import middle._

import sys.process._
import java.io.{File, PrintWriter}

abstract class Solver() {

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

  def generateQuery(program: Interfaced): String
  def getCommand(): String

  def solve(
    program: Interfaced,
    run: Boolean,
    outFile: Option[String]
  ): ProofResult = {

    // need to call this first before checking if it is a synthesis query
    val query = generateQuery(program)

    val suffix = if (program.isSynthesisQuery) { ".sl" }
    else { ".smt2" }
    val qfile =
      writeQueryToFile(query, outFile, suffix).getAbsolutePath()

    if (!run) {
      return new ProofResult(
        None,
        List("Opted out of running the solver.")
      )
    }

    val result = runProcess(s"${getCommand()} ${qfile}")
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

class CVC4Solver() extends Solver() {
  def getCommand(): String = "cvc4 --dump-models"

  def generateQuery(program: Interfaced): String = {
    val query = program.programToQuery()
    query
  }
}

class Z3Solver() extends Solver() {
  def getCommand(): String = "z3 dump_models=true"

  def generateQuery(program: Interfaced): String = {
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
}

class AltErgoSolver() extends Solver() {
  def getCommand(): String = "alt-ergo -enable-adts-cs"

  def generateQuery(program: Interfaced): String = {
    // get the query but remove the set logic and set-option commands
    val query = program
      .programToQuery()
      .split("\n")
      .filter(p => !(p.startsWith("(set-logic") || p.startsWith("(set-option")))
      .mkString("\n")
    if (program.isSynthesisQuery) {
      throw new SolverMismatchError("Alt-Ergo does not support synthesis")
    }
    println("Warning: Alt-Ergo does not generate models.")
    query
  }
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

  def generateQuery(program: Interfaced): String = {
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

    println("Warning: Vampire does not generate models.")
    query
  }
}
