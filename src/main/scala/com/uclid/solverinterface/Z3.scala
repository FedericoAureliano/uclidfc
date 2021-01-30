package com.uclid.solverinterface



import sys.process._
import java.io.{File, PrintWriter}

class Z3(ctx: Context) extends Solver(ctx) {
  def getCommand(): String = "z3"

  def generateQuery(): String = {
    // get the query but remove the set logic command
    val query = ctx
      .programToQuery()
      .split("\n")
      .filter(p => !p.startsWith("(set-logic"))
      .mkString("\n")
    if (ctx.isSynthesisQuery) {
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