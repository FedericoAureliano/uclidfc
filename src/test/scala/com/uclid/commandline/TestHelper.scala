package com.uclid.commandline

import com.uclid.termgraph

import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

def readTestFile(file: File): Tuple8[
  String,
  List[String],
  Option[Boolean],
  Option[Double],
  Option[Double],
  Option[Double],
  Option[Double],
  List[String]
  ] = {
  val lines = Source.fromFile(file).getLines().mkString("\n")
  val solvers = "(?<=Solver=)(.*)".r.findAllIn(lines).map(s => s match {
    case "z3"   => Some("z3")
    case "cvc4" => Some("cvc4")
    case _      => None
  }).flatten.toList
  val result = "(?<=Result=)(.*)".r.findFirstIn(lines) match {
    case Some("Some(true)")  => Some(true)
    case Some("Some(false)") => Some(false)
    case _                   => None
  }
  val maxParseTime = "(?<=MaxParseTime=)(.*)".r.findFirstIn(lines) match {
    case Some(v) => Some(v.toDouble)
    case _       => None
  }
  val maxProcessTime = "(?<=MaxProcessTime=)(.*)".r.findFirstIn(lines) match {
    case Some(v) => Some(v.toDouble)
    case _       => None
  }
  val maxGenerationTime =
    "(?<=MaxGenerationTime=)(.*)".r.findFirstIn(lines) match {
      case Some(v) => Some(v.toDouble)
      case _       => None
    }
  val maxSolveTime = "(?<=MaxSolveTime=)(.*)".r.findFirstIn(lines) match {
    case Some(v) => Some(v.toDouble)
    case _       => None
  }
  val rewrites =
    "(?<=Rewrite=)(.*)".r.findAllIn(lines).map(r => "--" + r).toList
  val options =
    "(?<=Option=)(.* .*)".r.findAllIn(lines).map(r => {
      val pair = r.split(" ")
      List("--" + pair(0), pair(1))
    }).flatten.toList
  (
    (
      file.getAbsolutePath(),
      solvers,
      result,
      maxParseTime,
      maxProcessTime,
      maxGenerationTime,
      maxSolveTime,
      rewrites ++ options
    )
  )
}

def endToEnd(
  filename: String,
  solvers: List[String],
  rewrites: List[String]
): List[UclidResult] = {
  println(rewrites)
  solvers match {
    case Nil =>
      UclidMain.main(
        UclidMain.parseOptions(Array(filename, "--skip-solver") ++ rewrites).get
      )
    case ls =>
      val options : ArrayBuffer[String] = new ArrayBuffer[String]()
      
      options.addOne(filename)
      ls.foreach(s => {
        options.addOne("-s")
        options.addOne(s)
      })
      options.addAll(rewrites)

      UclidMain.main(
        UclidMain.parseOptions(options.toArray).get
      )
  }
}