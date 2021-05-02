package com.uclid.context.solver

import com.uclid.context.Context
import com.uclid.context.solver.ProofResult
import com.uclid.termgraph
import com.uclid.utility.SimulationTable

import java.io.{File, PrintWriter}
import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.sys.process._
import collection.parallel.CollectionConverters.IterableIsParallelizable

private var fileCount = 0

abstract class Solver() {

  def runProcess(ctx: Context, file: File, timeout: Int, simulationData: Option[SimulationTable]): (List[String], List[String], Int, Double) = {
    val in = s"${getCommand(ctx)} ${file}"

    if simulationData.isDefined then {
      val table = simulationData.get
      val (res, time) = table.simulate(getName(), file.getAbsolutePath())
      val timetaken = if time < timeout then time else timeout.toDouble
      println(s"-- ${in.split(" ").head} terminated in ${"%.3f".toString.format(timetaken)} seconds.")
      return (List(res), List(), 0, timetaken)
    }

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
      exit.destroy() // kill the process
      out = List("timeout")
    }
    val timetaken = (System.nanoTime - t1) / 1e9d
    println(s"-- ${in.split(" ").head} terminated in ${"%.3f".toString.format(timetaken)} seconds.")

    (out.reverse, err.reverse, exit.exitValue(), timetaken)
  }

  def writeQueryToFile(
    query: String,
    outFile: Option[String],
    suffix: String
  ): File = {
    val f = outFile match {
      case Some(value) => {
        val fileName = Paths.get(value).getFileName
        val extension = fileName.toString.split("\\.").last
        val newName = s"${value.toString.split("\\.").dropRight(1).mkString("")}$fileCount.$extension"
        new File(newName)
      }
      case None =>
        val tempFi = File.createTempFile("tmp", suffix)
        tempFi.deleteOnExit()
        tempFi
    }
    fileCount += 1

    new PrintWriter(f) {
      try write(query)
      finally close()
    }
    f
  }

  def generateQueries(ctx: Context, prettyPrint: Int): List[String]
  def getCommand(ctx: Context): String
  def getName(): String
  def parseAnswer(answer: String): String

  def solve(
    run: Boolean,
    timeout: Int,
    ctx: Context,
    outFile: Option[String],
    prettyPrint: Int,
    simulationData: Option[SimulationTable],
    unmodifiedSMTFile: Option[File] = None
  ): (ProofResult, Double, Double) = {

    print("Generating query ... ")
    val t1 = System.nanoTime
    val qfiles : List[File] = unmodifiedSMTFile match {
      case Some(value) if prettyPrint == 0 => List(value)
      case _ => {
        // need to call this first before checking if it is a synthesis query
        var queries : List[File] = generateQueries(ctx, prettyPrint).map(q => {
          var query = q
          val suffix = if ctx.isSynthesisQuery() then { ".sl" }
          else { 
            ".smt2" 
          }
          writeQueryToFile(query, outFile, suffix)
        })
        queries
      }
    }

    val generationDuration = (System.nanoTime - t1) / 1e9d
    println(s"Query generated in ${generationDuration} seconds.")

    if !run then {
      return (
        new ProofResult(
          None,
          "Opted out of running the solver."
        ),
        generationDuration,
        0
      )
    }

    println("Running solver processes ... ")
    val results = qfiles.par.map(qfile => runProcess(ctx, qfile, timeout, simulationData))
    val answers = results.map(result => parseAnswer(" " ++ (result._1 ++ result._2).mkString("\n")))

    def ternaryCombine(a : Option[Boolean], b : Option[Boolean]) : Option[Boolean] = {
      if !a.isDefined || !b.isDefined then {
        None
      } else {
        Some(List(a, b).flatten.exists(p => p))
      }
    }

    def combineStrs(a : String, b: String) : String = {
      if a == "" then {
        b
      } else {
        List(a, b).mkString(", ")
      }
    }

    answers.zip(results).foldLeft((new ProofResult(Some(false), ""), generationDuration, 0.0))((acc, pair) => {
      val answer = pair._1
      val result = pair._2

      answer match {
        case _ if answer.contains("error") || answer.contains("unknown") => {
          (new ProofResult(None,  combineStrs(acc._1.messages, answer)), generationDuration, acc._3 + result._4)
        }
        case _ if "(\\ssat)".r.findFirstIn(answer).isDefined => {
          (new ProofResult(ternaryCombine(acc._1.result, Some(true)),  combineStrs(acc._1.messages, answer)), generationDuration, acc._3 + result._4)
        }
        case _ if "(\\sunsat)".r.findFirstIn(answer).isDefined => {
          if ctx.isSynthesisQuery() then {
            (
              new ProofResult(ternaryCombine(acc._1.result, Some(false)),  combineStrs(acc._1.messages, answer)),
              generationDuration,
              acc._3 + result._4
            )
          } else {
            (
              new ProofResult(ternaryCombine(acc._1.result, Some(false)), combineStrs(acc._1.messages, "unsat")),
              generationDuration,
              acc._3 + result._4
            )
          }
        }
        case _ => (new ProofResult(None,  combineStrs(acc._1.messages, answer)), generationDuration, acc._3 + result._4)
      }
    })
  }
}
