package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

class Vampire() extends Solver() {

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

  def getCommand(ctx: Context): String =
    "vampire --mode smtcomp --input_syntax smtlib2 --term_algebra_acyclicity light --term_algebra_rules on --fmb_enumeration_strategy smt"

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    val queries = ctx.toQueries(prettyPrint)

    queries.map(query => {
      // find the set logic command
      val pattern = "(?<=\\(set-logic )(.*)(?=\\))".r
      val logic = pattern.findFirstIn(query)
  
      if (logic.isDefined && !supportedLogics.contains(logic.get)) {
        throw new SolverMismatchError(s"Vampire does not support ${logic.get}")
      }
  
      if (ctx.termgraph.isSynthesisQuery()) {
        throw new SolverMismatchError("Vampire does not support synthesis")
      }
    })

    queries
  }

  def parseAnswer(answer: String): String =
    answer
}
