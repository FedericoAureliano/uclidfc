package com.uclid.solverinterface.solver

import com.uclid.termgraph
import com.uclid.solverinterface.solver.ProofResult
import com.uclid.context.Context

import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.sys.process._
import java.io.{File, PrintWriter}

abstract class Solver() {

  def runProcess(in: String, timeout: Int): (List[String], List[String], Int, Double) = {
    print("Running solver ... ")
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val t1 = System.nanoTime
    val exit = qb.run(ProcessLogger(s => out ::= s, s => err ::= s))

    val f = Future(blocking(exit.exitValue())) // wrap in Future
    try {
      Await.result(f, duration.Duration(timeout, "sec"))
    } catch {
      case e: TimeoutException => 
      exit.destroy() // kill the process, but then propagate the error so that UclidMain knows about it
      throw e
    }
    val timetaken = (System.nanoTime - t1) / 1e9d
    println(s"Solver terminated in ${timetaken} seconds.")

    (out.reverse, err.reverse, exit.exitValue(), timetaken)
  }

  def writeQueryToFile(
    query: String,
    outFile: Option[String],
    suffix: String
  ): File = {
    val f = outFile match {
      case Some(value) => new File(value)
      case None =>
        val tempFi = File.createTempFile("tmp", suffix)
        tempFi.deleteOnExit()
        tempFi
    }

    new PrintWriter(f) {
      try write(query)
      finally close()
    }
    f
  }

  def generateQuery(ctx: Context, prettyPrint: Boolean): String
  def getCommand(): String
  def parseAnswer(answer: String): String

  def solve(
    run: Boolean,
    timeout: Int,
    ctx: Context,
    outFile: Option[String],
    prettyPrint: Boolean
  ): (ProofResult, Double, Double) = {

    // need to call this first before checking if it is a synthesis query
    print("Generating query ... ")
    val t1 = System.nanoTime
    val query = generateQuery(ctx, prettyPrint)
    val generationDuration = (System.nanoTime - t1) / 1e9d
    println(s"Query generated in ${generationDuration} seconds.")

    val suffix = if (ctx.termgraph.isSynthesisQuery) { ".sl" }
    else { ".smt2" }
    val qfile =
      writeQueryToFile(query, outFile, suffix).getAbsolutePath()

    if (!run) {
      return (
        new ProofResult(
          None,
          " Opted out of running the solver."
        ),
        generationDuration,
        0
      )
    }

    val result = runProcess(s"${getCommand()} ${qfile}", timeout)
    val answer = parseAnswer(" " + (result._1 ++ result._2).mkString("\n"))

    if (
      answer.contains("error") || answer.contains(
        "unknown"
      )
    ) {
      (new ProofResult(None, answer), generationDuration, result._4)
    } else {
      if ("(\\ssat)".r.findFirstIn(answer).isDefined) {
        (new ProofResult(Some(true), answer), generationDuration, result._4)
      } else {
        if (ctx.termgraph.isSynthesisQuery) {
          (
            new ProofResult(Some(false), answer),
            generationDuration,
            result._4
          )
        } else {
          (
            new ProofResult(Some(false), " unsat"),
            generationDuration,
            result._4
          )
        }
      }
    }
  }
}
