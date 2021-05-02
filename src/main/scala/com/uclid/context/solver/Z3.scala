package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

case class Z3() extends Solver() {
  def getName() : String = "z3"
  def getCommand(ctx: Context): String = "z3"

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    // get the query but remove the set logic command
    val query = ctx
      .toQueries(prettyPrint)
      .map { q =>
        q.split("\n")
          .filter(p => !p.startsWith("(set-logic"))
          .mkString("\n")
      }
    if ctx.isSynthesisQuery() then {
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
