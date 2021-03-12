package com.uclid.context.solver

import com.uclid.context.Context
import com.uclid.smtcompiler.SmtCompiler

import java.io.{File, PrintWriter}
import scala.sys.process._
import scala.util.Random

class Medley(choices: List[Solver]) extends Solver() {
  val random = Random

  def pickSolver(features: Map[String, String]) : Solver = {
    // Very silly rule to demo. 
    // TODO: do something better
    val z3 = Z3()
    if (features("NIA").toInt > 0 && choices.contains(z3)) {
      z3
    } else {
      choices(random.nextInt(choices.length))
    }
  }

  def getCommand(ctx: Context): String = {
    pickSolver(ctx.termgraph.featureMap(ctx.entryPoints())).getCommand(ctx)
  }
  
  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    val query = ctx.toQueries(prettyPrint)
    query
  }

  def parseAnswer(answer: String): String = SmtCompiler.removeComments(answer).replace("unsupported", "")
}
