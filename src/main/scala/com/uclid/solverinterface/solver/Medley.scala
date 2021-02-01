package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}
import scala.util.Random

class Medley(ctx: Context, choices: List[Solver]) extends Solver(ctx) {
  val random = Random

  val solver = choices(
      random.nextInt(choices.length)
  )

  def getCommand(): String = solver.getCommand()
  def generateQuery(): String = solver.generateQuery()
  def parseAnswer(answer: String): String = solver.parseAnswer(answer)
}
