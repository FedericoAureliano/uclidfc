package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

class CVC4() extends Solver() {
  def getCommand(ctx: Context): String = "cvc4 --strings-exp --incremental"

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
