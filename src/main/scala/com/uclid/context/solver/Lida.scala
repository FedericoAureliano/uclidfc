package com.uclid.context.solver

import com.uclid.context.Context
import com.uclid.smtcompiler.SmtCompiler
import com.uclid.idiolect.WCFG
import com.uclid.utility.SimulationTable

import java.io.{File, PrintWriter}
import scala.sys.process._
import scala.util.Random

// http://parnaseo.uv.es/Celestinesca/Numeros/1982/VOL%206/NUM%202/2_articulo1.pdf
class Lida(choices: List[(WCFG, Solver)]) extends Solver() {
  def getName() : String = "lida"

  val random = Random

  def getCommand(ctx: Context): String = {
    var bestScore = choices(0)._1.likelihood(ctx)
    var bestSolver = choices(0)._2

    choices.tail.foreach((wcfg, solver) => {
      val tmpScore = wcfg.likelihood(ctx)
      if tmpScore > bestScore then {
        bestSolver = solver
        bestScore = tmpScore
      }
    })

    bestSolver.getCommand(ctx)
  }

  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = {
    val query = ctx.toQueries(prettyPrint)
    query
  }

  def parseAnswer(answer: String): String =
    SmtCompiler.removeComments(answer).replace("unsupported", "")


  def train(
    run: Boolean,
    timeout: Int,
    ctx: Context,
    outFile: Option[String],
    prettyPrint: Int,
    simulationData: Option[SimulationTable],
    unmodifiedSMTFile: Option[File] = None
  ): (ProofResult, Double, Double) = {
    val results = choices.map((wcfg, solver) => solver.solve(run, timeout, ctx, outFile, prettyPrint, simulationData, unmodifiedSMTFile))

    var bestResult = results(0)._1 
    var bestScore = results(0)._3
    var bestSolverIndex = 0

    results.zipWithIndex.tail.foreach((result, index) => {
      if result._1.result.isDefined && result._3 < bestScore then {
        bestResult = result._1
        bestScore = result._3
        bestSolverIndex = index
      }
    })

    if bestResult.result.isDefined then choices(bestSolverIndex)._1.update(ctx)

    results(bestSolverIndex)
  }

  def save(folder: String) : Unit = {
    choices.foreach((wcfg, solver) => {
      wcfg.save(folder + "/" + solver.getName() + ".csv")
    })
  }
}
