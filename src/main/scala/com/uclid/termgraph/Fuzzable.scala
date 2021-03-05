package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer
import com.uclid.termgraph.Util

trait Fuzzable() extends AbstractTermGraph {

  def fuzz(position: Int): Int =
    getStmt(position) match {
      case d: DataType   => fuzzDatatype(d)
      case t: TheorySort => fuzzTheorysort(t)
      case u: UserSort   => fuzzUsersort(u)
      case m: Module     => fuzzModule(m)
    }

  private def fuzzModule(mod: Module): Int = {
    // apply constructor to random instances of all its selectors
    val components =
      getStmt(mod.ct).asInstanceOf[Constructor].selectors.map { s =>
        val sel = getStmt(s).asInstanceOf[Selector]
        fuzz(sel.sort)
      }
    val body = memoAddInstruction(Application(mod.ct, components))
    val um = memoAddInstruction(
      UserMacro(Util.freshSymbolName(), memoGetInstruction(mod), body, List.empty)
    )
    memoAddInstruction(Application(um, List.empty))
  }

  private def fuzzDatatype(d: DataType): Int = {
    // pick a random constructor
    val ctr = d.constructors(Util.random.nextInt(d.constructors.length))
    // apply constructor to random instances of all its selectors
    val components = getStmt(ctr).asInstanceOf[Constructor].selectors.map { s =>
      val sel = getStmt(s).asInstanceOf[Selector]
      fuzz(sel.sort)
    }
    val body = memoAddInstruction(Application(ctr, components))
    val um = memoAddInstruction(
      UserMacro(Util.freshSymbolName(), memoGetInstruction(d), body, List.empty)
    )
    memoAddInstruction(Application(um, List.empty))
  }

  private def fuzzTheorysort(t: TheorySort): Int =
    t match {
      case TheorySort("Bool", _) =>
        memoAddInstruction(Application(memoAddInstruction(TheoryMacro(Util.random.nextBoolean().toString())), List.empty))
      case TheorySort("Int", _) =>
        memoAddInstruction(Application(memoAddInstruction(TheoryMacro(Util.random.nextInt().toString())), List.empty))
      case TheorySort("Array", params) =>
        val out = fuzz(params.last)
        val asConstAppRef = memoAddInstruction(TheoryMacro("as const", List(memoGetInstruction(t))))
        memoAddInstruction(
          Application(asConstAppRef, List(out))
        )
    }

  private def fuzzUsersort(u: UserSort): Int =
    memoAddInstruction(Application(memoAddInstruction(UserFunction(Util.freshSymbolName(), memoGetInstruction(u))), List.empty))
}
