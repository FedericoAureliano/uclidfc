package com.uclid.solverinterface.solver

import com.uclid.context.Context

import sys.process._
import java.io.{File, PrintWriter}
import scala.util.Random

class Medley(choices: List[Solver]) extends Solver() {
  val random = Random

  var solver = choices(
      random.nextInt(choices.length)
  )

  def getCommand(): String = solver.getCommand()
  def generateQuery(ctx: Context): String = solver.generateQuery(ctx)
  def parseAnswer(answer: String): String = {
    val parsed = solver.parseAnswer(answer)

    // update the solver for the next round
    solver = choices(
          random.nextInt(choices.length)
      )

    parsed
  }
}
