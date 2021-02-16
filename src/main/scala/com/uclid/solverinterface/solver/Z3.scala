package com.uclid.solverinterface.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

class Z3() extends Solver() {
  def getCommand(): String = "z3"

  def generateQuery(ctx: Context, prettyPrint: Boolean): String = {
    // get the query but remove the set logic command
    val query = ctx
      .toQuery(prettyPrint)
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
