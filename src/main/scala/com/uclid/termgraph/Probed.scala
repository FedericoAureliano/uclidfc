package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

trait Probed() extends AbstractTermGraph {

  def featuresList(entryPoints: List[Int]): List[String] =
    List(
      "Term graph size: " + numberOfNodes().toString,
      "Requires synthesis: " + isSynthesisQuery,
      "Number of bound variables: " + numberOfVariables().toString,
      "Largest integer literal: " + largestIntegerLiteral(entryPoints).toString,
      "Logic components:\n" + logicComponents(entryPoints)
        .map((logic, fraction) => s"---- $logic: $fraction")
        .mkString("\n")
    )

  def numberOfNodes(): Int = stmts.length
  def numberOfMemoEntries(): Int = memo.keys.toList.length
  def numberOfVariables(): Int = numberOfNodes() - numberOfMemoEntries()

  def largestIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    stmts
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            name.toIntOption match {
              case Some(value) =>
                if (value >= max.getOrElse(value)) {
                  max = Some(value)
                }
              case None =>
            }
          case _ =>
        }
      )
    max
  }

  def logicComponents(entryPoints: List[Int]): List[(String, Int)] = {
    var q = 0
    var uf = 0
    var a = 0
    var dt = 0
    var lia = 0
    var nia = 0

    val marks = mark(entryPoints)

    marks
      .zip(stmts)
      .foreach((marked, inst) =>
        if (marked) {
          inst match {
            case Application(caller, args) =>
              (caller :: args).foreach(pos => {
                stmts(pos) match {
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
                      nia += 1
                    } else {
                      lia += 1
                    }
                  case TheoryMacro("+", _) => lia += 1
                  case TheoryMacro("-", _) => lia += 1
                  case TheoryMacro("forall", _) => q += 1
                  case TheoryMacro("exists", _) => q += 1
                  case TheoryMacro("select", _) => a += 1
                  case TheoryMacro("store", _) => a += 1
                  case UserFunction(_, _, args) => if (args.length > 0) uf += 1
                  case Constructor(_, _, _) => dt += 1
                  case Selector(_, _) => dt += 1
                  case _                   =>
                }
              })
            case _ =>
          }
        }
      )

    List(
      ("Q", q),
      ("UF", uf),
      ("A", a),
      ("DT", dt),
      ("LIA", lia),
      ("NIA", nia)
    )
  }

  def queryLogic(entryPoints: List[Int]): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true

    val marks = mark(entryPoints)

    marks
      .zip(stmts)
      .foreach((marked, inst) =>
        if (marked) {
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
