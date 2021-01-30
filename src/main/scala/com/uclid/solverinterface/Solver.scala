package com.uclid.solverinterface

import com.uclid.termgraph


import sys.process._
import java.io.{File, PrintWriter}

abstract class Solver(ctx: Context) {

  def runProcess(in: String): (List[String], List[String], Int, Double) = {
    print("Running solver ... ")
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()

    val t1 = System.nanoTime
    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    val duration = (System.nanoTime - t1) / 1e9d
    println(s"Solver terminated in ${duration} seconds.")

    (out.reverse, err.reverse, exit, duration)
  }

  def writeQueryToFile(
    query: String,
    outFile: Option[String],
    suffix: String
  ): File = {
    val f = outFile match {
      case Some(value) => new File(value)
      case None => {
        val tempFi = File.createTempFile("tmp", suffix)
        tempFi.deleteOnExit()
        tempFi
      }
    }

    new PrintWriter(f) {
      try {
        write(query)
      } finally {
        close()
      }
    }
    f
  }

  def generateQuery(): String
  def getCommand(): String
  def parseAnswer(answer: String): String

  def solve(
    run: Boolean,
    outFile: Option[String]
  ): (ProofResult, Double, Double) = {

    // need to call this first before checking if it is a synthesis query
    print("Generating query ... ")
    val t1 = System.nanoTime
    val query = generateQuery()
    val generationDuration = (System.nanoTime - t1) / 1e9d
    println(s"Query generated in ${generationDuration} seconds.")
    if (ctx.isSynthesisQuery) {
      println("-- Query requires program synthesis.")
    }
    println(s"-- Logic is ${ctx.logic}.")

    val suffix = if (ctx.isSynthesisQuery) { ".sl" }
    else { ".smt2" }
    val qfile =
      writeQueryToFile(query, outFile, suffix).getAbsolutePath()

    if (!run || (!ctx.checkQuery && !ctx.traceQuery)) {
      return (
        new ProofResult(
          None,
          " Opted out of running the solver."
        ),
        generationDuration,
        0
      )
    }

    val result = runProcess(s"${getCommand()} ${qfile}")
    val answer = parseAnswer(" " + (result._1 ++ result._2).mkString("\n"))

    if (answer.contains("error") || answer.contains(
          "unknown"
        )) {
      (new ProofResult(None, answer), generationDuration, result._4)
    } else {
      if (ctx.traceQuery) {
        (new ProofResult(None, answer), generationDuration, result._4)
      } else {
        if ("(\\ssat)".r.findFirstIn(answer).isDefined) {
          (new ProofResult(Some(true), answer), generationDuration, result._4)
        } else {
          if (ctx.isSynthesisQuery) {
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
}