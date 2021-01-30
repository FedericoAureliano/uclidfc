package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait Fuzzable() extends AbstractTermGraph {
  val random = new Random

  def fuzz(position: Int): Int =
    stmts(position) match {
      case d: DataType   => fuzzDatatype(d)
      case t: TheorySort => fuzzTheorysort(t)
      case u: UserSort   => fuzzUsersort(u)
      case m: Module     => fuzzModule(m)
    }

  private def fuzzModule(mod: Module): Int = {
    // apply constructor to random instances of all its selectors
    val components =
      stmts(mod.ct).asInstanceOf[Constructor].selectors.map { s =>
        val sel = stmts(s).asInstanceOf[Selector]
        fuzz(sel.sort)
      }
    val body = memoAddInstruction(Application(mod.ct, components))
    memoAddInstruction(
      UserMacro(freshSymbolName(), memoGetInstruction(mod), body, List.empty)
    )
  }

  private def fuzzDatatype(d: DataType): Int = {
    // pick a random constructor
    val ctr = d.constructors(random.nextInt(d.constructors.length))
    // apply constructor to random instances of all its selectors
    val components = stmts(ctr).asInstanceOf[Constructor].selectors.map { s =>
      val sel = stmts(s).asInstanceOf[Selector]
      fuzz(sel.sort)
    }
    val body = memoAddInstruction(Application(ctr, components))
    memoAddInstruction(
      UserMacro(freshSymbolName(), memoGetInstruction(d), body, List.empty)
    )
  }

  private def fuzzTheorysort(t: TheorySort): Int =
    t match {
      case TheorySort("Bool", _) =>
        memoAddInstruction(TheoryMacro(random.nextBoolean().toString()))
      case TheorySort("Int", _) =>
        memoAddInstruction(TheoryMacro(random.nextInt().toString()))
      case TheorySort("Array", params) =>
        val out = fuzz(params.last)
        val asConstAppRef = {
          val asConstRef = memoAddInstruction(TheoryMacro("as const"))
          memoAddInstruction(
            Application(asConstRef, List(memoGetInstruction(t)))
          )
        }
        memoAddInstruction(
          Application(asConstAppRef, List(out))
        )
    }

  private def fuzzUsersort(u: UserSort): Int =
    memoAddInstruction(UserFunction(freshSymbolName(), memoGetInstruction(u)))
}
