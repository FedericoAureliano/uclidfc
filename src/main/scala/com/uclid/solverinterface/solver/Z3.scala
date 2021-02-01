package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}

class Z3() extends Solver() {
  def getCommand(): String = "z3"

  def generateQuery(ctx: Context): String = {
    // get the query but remove the set logic command
    val query = ctx
      .toQuery()
      .split("\n")
      .filter(p => !p.startsWith("(set-logic"))
      .mkString("\n")
    if (ctx.termgraph.isSynthesisQuery) {
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
