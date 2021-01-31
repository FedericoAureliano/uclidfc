package com.uclid.context

import com.uclid.uclidinterface.compiler.parser._

import com.uclid.termgraph._

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.immutable.Nil

abstract class Context(val termgraph: TermGraph) {
  def toQuery(): String
  
  var isSynthesisQuery = false

  def getLogic(): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true

    termgraph.stmts
      .foreach(inst =>
        inst match {
          case _: AbstractDataType => dt = true
          case Application(caller, args) =>
            termgraph.stmts(caller) match {
              case TheoryMacro("*", _) =>
                if (
                  args.filter { a =>
                    termgraph.stmts(a) match {
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
