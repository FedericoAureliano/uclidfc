package com.uclid.commandline

import com.uclid.termgraph

import scala.io.Source
import java.io.File

  def readTestFile(file: File): Tuple8[
    String,
    Option[String],
    Option[Boolean],
    Option[Double],
    Option[Double],
    Option[Double],
    Option[Double],
    List[String]
   ] = {
    val lines = Source.fromFile(file).getLines().mkString("\n")
    val solver = "(?<=Solver=)(.*)".r.findFirstIn(lines) match {
      case Some("z3")   => Some("z3")
      case Some("cvc4") => Some("cvc4")
      case _            => None
    }
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
    (
      (
        file.getAbsolutePath(),
        solver,
        result,
        maxParseTime,
        maxProcessTime,
        maxGenerationTime,
        maxSolveTime,
        rewrites
      )
    )
  }

  def endToEnd(
    filename: String,
    solver: Option[String],
    rewrites: List[String]
  ): UclidResult = {
    solver match {
      case Some(value) =>
        UclidMain.main(
          UclidMain.parseOptions(Array(filename, "-s", value) ++ rewrites).get
        )
      case None =>
        UclidMain.main(
          UclidMain.parseOptions(Array(filename, "-r", "false") ++ rewrites).get
        )
    }
  }