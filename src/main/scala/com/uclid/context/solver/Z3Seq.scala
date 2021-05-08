package com.uclid.context.solver

import com.uclid.context.Context

import java.io.{File, PrintWriter}
import scala.sys.process._

case class Z3Seq() extends Solver() {
  def getName() : String = "z3"
  def getCommand(ctx: Context): String = "z3str4-seq"
  def generateQueries(ctx: Context, prettyPrint: Int): List[String] = ctx.toQueries(prettyPrint)
  def parseAnswer(answer: String): String = answer
}
