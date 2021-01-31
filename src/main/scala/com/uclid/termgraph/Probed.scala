package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

trait Probed() extends AbstractTermGraph {

  def featuresList(entryPoints: List[Int]) : List[String] = {
    List(
      "Query logic: " + queryLogic(entryPoints),
      "Requires synthesis: " + isSynthesisQuery,
      "Term graph size: " + numberOfNodes().toString,
      "Memoization map size: " + numberOfMemoEntries().toString,
      "Largest integer literal: " + largestIntegerLiteral(entryPoints).toString
    )
  }

  def numberOfNodes(): Int = stmts.length
  def numberOfMemoEntries(): Int = memo.keys.toList.length

  def largestIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var max : Option[Int] = None 
    stmts
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) => name.toIntOption match {
            case Some(value) => {
              if (value >= max.getOrElse(value)) {
                max = Some(value)
              }
            }
            case None => 
          }
          case _ =>
        }
      )
    max
  }

  def queryLogic(entryPoints: List[Int]): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true

    val marks = mark(entryPoints)

    marks.zip(stmts)
      .foreach((marked, inst) =>
        if(marked) {
          inst match {
            case _: AbstractDataType => dt = true
            case Application(caller, args) =>
              stmts(caller) match {
                case TheoryMacro("*", _) =>
                  if (
                    args.filter { a =>
                      stmts(a) match {
                        case TheoryMacro(name, _) =>
                          name.toIntOption.isDefined
                        case _ => false
                      }
                    }.length < args.length - 1
                  ) {
                    linear = false
                  }
                case _ =>
              }
            case TheoryMacro("exists", _) => qf = false
            case TheoryMacro("forall", _) => qf = false
            case TheoryMacro(name, _) =>
              if (name.toIntOption.isDefined) { i = true }
            case UserFunction(_, _, params) =>
              if (params.length > 0) { uf = true }
            case TheorySort("Array", _) => a = true
            case TheorySort("Int", _)   => i = true
            case Synthesis(_, _, _)     => isSynthesisQuery = true
            case _                      =>
          }
        }
      )

    s"${if (qf && !isSynthesisQuery) { "QF_" }
    else { "" }}${if (uf) { "UF" }
    else { "" }}${if (a) { "A" }
    else { "" }}${if (dt) { "DT" }
    else { "" }}${if (linear && i) { "L" }
    else if (!linear && i) { "N" }
    else { "" }}${if (i) { "IA" }
    else { "" }}"
  }
}
