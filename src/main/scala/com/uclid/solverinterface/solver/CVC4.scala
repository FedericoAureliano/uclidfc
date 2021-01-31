package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}

class CVC4(ctx: Context) extends Solver(ctx) {
  def getCommand(): String = "cvc4"

  def generateQuery(): String = {
    val query = ctx.toQuery()
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
