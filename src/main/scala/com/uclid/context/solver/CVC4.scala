package com.uclid.context.solver

import com.uclid.context.Context
import com.uclid.termgraph.TheoryMacro

import java.io.{File, PrintWriter}
import scala.sys.process._

case class CVC4() extends Solver() {
  def getName() : String = "cvc4"
  def getCommand(ctx: Context): String = {
    var command = if !ctx.isSynthesisQuery() then "cvc4 --incremental" else "cvc4"

    ctx.termgraph.mark(ctx.entryPoints()).zipWithIndex.foreach((b, i) => {
      if b then {
        ctx.termgraph.getStmt(i) match {
          case TheoryMacro(n, _) if n.startsWith("\"") && n.endsWith("\"") => return command + " --strings-exp"
          case _ =>
        }
      }
    })

    command
  }

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    val query = ctx.toQueries(prettyPrint)
    query
  }

  def parseAnswer(answer: String): String =
    answer
      .split("\n")
      .filter(p =>
        !p.startsWith(
          "(error \"Cannot get the current model unless immediately preceded by SAT/INVALID or UNKNOWN response.\")"
        ) && !p.startsWith(
          "(error \"Cannot get the current assignment unless immediately preceded by SAT/INVALID or UNKNOWN response.\")"
        ) && !p.startsWith(
          "(error \"Cannot get value unless immediately preceded by SAT/INVALID or UNKNOWN response.\")"
        )
      )
      .mkString("\n")
}
