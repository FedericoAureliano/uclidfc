package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}

class AltErgo(ctx: Context) extends Solver(ctx) {
  def getCommand(): String = "alt-ergo -enable-adts-cs"

  def generateQuery(): String = {
    // get the query but remove the set logic and set-option commands
    val query = ctx
      .toQuery()
      .split("\n")
      .filter(p =>
        !(p.startsWith("(set-logic") || p.startsWith("(set-option") || p
          .startsWith("(get-"))
      )
      .mkString("\n")
    if (ctx.isSynthesisQuery) {
      throw new SolverMismatchError("Alt-Ergo does not support synthesis")
    }
    query
  }

  def parseAnswer(answer: String): String =
    answer
}
