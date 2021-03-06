package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

case class AltErgo() extends Solver() {
  def getCommand(ctx: Context): String = "alt-ergo -enable-adts-cs"

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    // get the query but remove the set logic and set-option commands
    val query = ctx
      .toQueries(prettyPrint)
      .map { q =>
        q.split("\n")
          .filter(p =>
            !(p.startsWith("(set-logic") || p.startsWith("(set-option") || p
              .startsWith("(get-"))
          )
          .mkString("\n")
      }
    if ctx.isSynthesisQuery() then {
      throw new SolverMismatchError("Alt-Ergo does not support synthesis")
    }
    query
  }

  def parseAnswer(answer: String): String =
    answer
}
