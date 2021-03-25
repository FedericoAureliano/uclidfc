package com.uclid.context

import com.uclid.termgraph._

import scala.collection.mutable.ListBuffer

class UclidContext(termgraph: TermGraph) extends Context(termgraph) {
  protected val assertionRefs = new ListBuffer[Int]()
  var getValues: Option[List[Int]] = None

  def addAssertion(ass: Int): Unit =
    assertionRefs.addOne(ass)

  var checkQuery = false
  var negateQuery = false
  var traceQuery = false
  var singleQuery = false

  protected val options: ListBuffer[(String, String)] =
    ListBuffer(("produce-assignments", "true"))

  def addOption(option: String, value: String): Unit =
    options.addOne((option, value))

  override def ignoreResult() = traceQuery

  override def entryPoints() =
    assertionRefs.toList ++ getValues.getOrElse(List.empty)

  override def toQueries(pp: Int): List[String] = {
    if assertionRefs.length == 0 then {
      val falseRef = termgraph.memoAddInstruction(TheoryMacro("false"))
      assertionRefs.addOne(
        termgraph.memoAddInstruction(Application(falseRef, List.empty))
      )
    }

    val isSynthesis = termgraph.isSynthesisQuery(entryPoints())

    assert(!(negateQuery && isSynthesis), "Cannot check-sat for synthesis!")

    if negateQuery || isSynthesis || singleQuery then {
      // combine all the queries
      val orRef = termgraph.memoAddInstruction(TheoryMacro("or"))
      val asserts =
        termgraph.memoAddInstruction(Application(orRef, assertionRefs.toList))
      val assertion = if negateQuery then {
        val notRef = termgraph.memoAddInstruction(TheoryMacro("not"))
        termgraph.memoAddInstruction(Application(notRef, List(asserts)))
      } else {
        asserts
      }
      val innerCtx = new SyMTContext(termgraph)
      options.foreach(o => innerCtx.addOption(o._1, o._2))
      innerCtx.addAssertion(assertion)
      if checkQuery || traceQuery then {
        innerCtx.checkSat()
      }
      innerCtx.toQueries(pp)
    } else {
      assertionRefs.foldLeft(List.empty: List[String]) { (acc, ass) =>
        val innerCtx = new SyMTContext(termgraph)
        options.foreach(o => innerCtx.addOption(o._1, o._2))
        innerCtx.addAssertion(ass)
        if checkQuery || traceQuery then {
          innerCtx.checkSat()
        }
        acc ++ innerCtx.toQueries(pp)
      }
    }
  }
}
