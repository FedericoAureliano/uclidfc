package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._
import scala.util.Random

class Medley(choices: List[Solver]) extends Solver() {
  val random = Random

  var solver = choices(
      random.nextInt(choices.length)
  )

  def getCommand(): String = solver.getCommand()
  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = solver.generateQueries(ctx, prettyPrint)
  def parseAnswer(answer: String): String = {
    val parsed = solver.parseAnswer(answer)

    // update the solver for the next round
    solver = choices(
          random.nextInt(choices.length)
      )

    parsed
  }
}
