package com.uclid.context

import com.uclid.termgraph._

import scala.collection.mutable.ListBuffer

class UclidContext(termgraph: TermGraph) extends Context(termgraph) {
  protected val assertionRefs = new ListBuffer[Int]()
  var getValues: Option[List[Int]] = None

  def addAssertion(ass: Int): Unit =
    assertionRefs.addOne(ass)

  var checkQuery = false
  var singleQuery = false

  protected val options: ListBuffer[(String, String)] =
    ListBuffer(("produce-assignments", "true"))

  def addOption(option: String, value: String): Unit =
    options.addOne((option, value))

  override def ignoreResult() = false

  override def entryPoints() =
    assertionRefs.toList ++ getValues.getOrElse(List.empty)

  override def isSynthesisQuery() = termgraph.isSynthesisQuery(entryPoints())

  override def toQueries(pp: Int): List[String] = {
    if assertionRefs.length == 0 then {
      val falseRef = termgraph.memoAddInstruction(TheoryMacro("false"))
      assertionRefs.addOne(
        termgraph.memoAddInstruction(Application(falseRef, List.empty))
      )
    }

    if isSynthesisQuery() || singleQuery then {
      // combine all the queries
      val orRef = termgraph.memoAddInstruction(TheoryMacro("or"))
      val asserts =
        termgraph.memoAddInstruction(Application(orRef, assertionRefs.toList))
      val innerCtx = new SyMTContext(termgraph)
      options.foreach(o => innerCtx.addOption(o._1, o._2))
      innerCtx.addAssertion(asserts)
      if checkQuery then {
        innerCtx.checkSat()
      }
      innerCtx.toQueries(pp)
    } else {
      assertionRefs.foldLeft(List.empty: List[String]) { (acc, ass) =>
        val innerCtx = new SyMTContext(termgraph)
        options.foreach(o => innerCtx.addOption(o._1, o._2))
        innerCtx.addAssertion(ass)
        if checkQuery then {
          innerCtx.checkSat()
        }
        acc ++ innerCtx.toQueries(pp)
      }
    }
  }
}
