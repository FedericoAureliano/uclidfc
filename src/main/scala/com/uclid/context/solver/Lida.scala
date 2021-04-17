package com.uclid.context.solver

import com.uclid.context.Context
import com.uclid.smtcompiler.SmtCompiler
import com.uclid.idiolect.WCFG

import java.io.{File, PrintWriter}
import scala.sys.process._
import scala.util.Random

// http://parnaseo.uv.es/Celestinesca/Numeros/1982/VOL%206/NUM%202/2_articulo1.pdf
class Lida(choices: Map[Solver, WCFG]) extends Solver() {
  val random = Random

  def pickSolver(features: Map[String, String]): Solver = {
      choices.keys.toList(random.nextInt(choices.size))
  }

  def getCommand(ctx: Context): String =
    pickSolver(ctx.termgraph.featureMap(ctx.entryPoints())).getCommand(ctx)

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    val query = ctx.toQueries(prettyPrint)
    query
  }

  def parseAnswer(answer: String): String =
    SmtCompiler.removeComments(answer).replace("unsupported", "")
}
