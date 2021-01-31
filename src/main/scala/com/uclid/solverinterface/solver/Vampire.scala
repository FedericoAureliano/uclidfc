package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}

class Vampire(ctx: Context) extends Solver(ctx) {

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

  def generateQuery(): String = {
    val query = ctx.toQuery()

    // find the set logic command
    val pattern = "(?<=\\(set-logic )(.*)(?=\\))".r
    val logic = pattern.findFirstIn(query)

    if (logic.isDefined && !supportedLogics.contains(logic.get)) {
      throw new SolverMismatchError(s"Vampire does not support ${logic.get}")
    }

    if (ctx.isSynthesisQuery) {
      throw new SolverMismatchError("Vampire does not support synthesis")
    }

    query
  }

  def parseAnswer(answer: String): String =
    answer
}
