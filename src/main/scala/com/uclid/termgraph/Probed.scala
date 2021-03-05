package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

trait Probed() extends AbstractTermGraph {

  def featuresList(entryPoints: List[Int]): List[String] =
    List(
      "Term graph size: " + numberOfNodes().toString,
      "Requires synthesis: " + isSynthesisQuery,
      "Number of variables: " + numberOfVariables().toString,
      "Largest integer literal: " + largestIntegerLiteral(entryPoints).toString,
      "Logic components:\n" + logicComponents(entryPoints)
        .map((logic, fraction) => s"---- $logic: $fraction")
        .mkString("\n")
    )

  def numberOfNodes(): Int = getStmts().length
  def numberOfMemoEntries(): Int = memo.keys.toList.length
  def numberOfVariables(): Int = getStmts().filter(p => p.isInstanceOf[UserFunction]).length

  def largestIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    getStmts()
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
    var s = 0

    val marks = mark(entryPoints)

    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if (marked) {
          inst match {
            case Application(caller, args) =>
              (caller :: args).foreach(pos => {
                getStmt(pos) match {
                  case TheoryMacro("*", _) =>
                    if (
                      args.filter { a =>
                        getStmt(a) match {
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
                  case TheoryMacro("str.++", _) => s += 1
                  case TheoryMacro("str.indexof", _) => s += 1
                  case TheoryMacro("str.substr", _) => s += 1
                  case TheoryMacro("str.len", _) => s += 1
                  case TheoryMacro("str.contains", _) => s += 1
                  case TheoryMacro("str.prefixof", _) => s += 1
                  case TheoryMacro("str.suffixof", _) => s += 1
                  case TheoryMacro("str.replace", _) => s += 1
                  case TheoryMacro("str.at", _) => s += 1
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
      ("NIA", nia),
      ("S", s)
    )
  }

  def queryLogic(entryPoints: List[Int]): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true
    var s = false

    val marks = mark(entryPoints)

    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if (marked) {
          inst match {
            case _: AbstractDataType => dt = true
            case Application(caller, args) =>
              getStmt(caller) match {
                case TheoryMacro("*", _) =>
                  if (
                    args.filter { a =>
                      getStmt(a) match {
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
            case TheorySort("String", _)   => s = true
            case Synthesis(_, _, _)     => isSynthesisQuery = true
            case UserSort(_, _) => uf = true
            case _                      =>
          }
        }
      )

    val out = s"${if (qf && !isSynthesisQuery) { "QF_" }
    else { "" }}${if (uf) { "UF" }
    else { "" }}${if (s) { "S" }
    else { "" }}${if (a) { "A" }
    else { "" }}${if (dt) { "DT" }
    else { "" }}${if (linear && i) { "L" }
    else if (!linear && i) { "N" }
    else { "" }}${if (i) { "IA" }
    else { "" }}"

    if (out == "QF_") {
      "QF_UF"
    } else {
      out
    }
  }
}
