package com.uclid.solverinterface.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

class AltErgo() extends Solver() {
  def getCommand(): String = "alt-ergo -enable-adts-cs"

  def generateQuery(ctx: Context, prettyPrint: Int): String = {
    // get the query but remove the set logic and set-option commands
    val query = ctx
      .toQuery(prettyPrint)
      .split("\n")
      .filter(p =>
        !(p.startsWith("(set-logic") || p.startsWith("(set-option") || p
          .startsWith("(get-"))
      )
      .mkString("\n")
    if (ctx.termgraph.isSynthesisQuery) {
      throw new SolverMismatchError("Alt-Ergo does not support synthesis")
    }
    query
  }

  def parseAnswer(answer: String): String =
    answer
}
