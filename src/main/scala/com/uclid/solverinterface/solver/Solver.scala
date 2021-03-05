package com.uclid.solverinterface.solver

import com.uclid.context.Context
import com.uclid.solverinterface.solver.ProofResult
import com.uclid.termgraph

import java.io.{File, PrintWriter}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.sys.process._

val INFO = "(set-info :smt-lib-version 2.6)\n(set-info :category \"industrial\")\n(set-info :source |Generator: Uclid5.|)\n(set-info :status unknown)\n\n"

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

  def generateQuery(ctx: Context, prettyPrint: Int): String
  def getCommand(): String
  def parseAnswer(answer: String): String

  def solve(
    run: Boolean,
    timeout: Int,
    ctx: Context,
    outFile: Option[String],
    prettyPrint: Int,
    unmodifiedSMTFile: Option[File] = None
  ): (ProofResult, Double, Double) = {

    print("Generating query ... ")
    val t1 = System.nanoTime
    val qfile = unmodifiedSMTFile match {
      case Some(value) => value
      case None => {
        // need to call this first before checking if it is a synthesis query
        var query = generateQuery(ctx, prettyPrint)
        assert(query != "", "Empty query!")
        val suffix = if (ctx.termgraph.isSynthesisQuery) { ".sl" }
        else { 
          query = INFO + query
          ".smt2" 
        }
        writeQueryToFile(query, outFile, suffix).getAbsolutePath()
      }
    }
    val generationDuration = (System.nanoTime - t1) / 1e9d
    println(s"Query generated in ${generationDuration} seconds.")

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
