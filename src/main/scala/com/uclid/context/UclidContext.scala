package com.uclid.context

import com.uclid.termgraph._

import scala.collection.mutable.ListBuffer

class UclidContext(termgraph: TermGraph) extends SyMTContext(termgraph) {
  protected val assertionRefs = new ListBuffer[Int]()
  protected val axiomRefs = new ListBuffer[Int]()
  var getValues: Option[List[Int]] = None

  override def addAssertion(ass: Int): Unit =
    assertionRefs.addOne(ass)

  def addAxiom(ax: Int): Unit =
    axiomRefs.addOne(ax)

  var checkQuery = false
  var traceQuery = false

  override def ignoreResult() = traceQuery

  override def entryPoints() = assertionRefs.toList ++ axiomRefs ++ getValues.getOrElse(List.empty)

  override def toQuery(prettyPrint: Boolean): String = {
    if (!prettyPrint) {
      TAB = ""
      NEWLINE = " "
    }
    alreadyDeclared.clear()
    val logic = termgraph.queryLogic(entryPoints())
    val logicString = s"(set-logic ${logic})"
    val opts = options.map(o => s"(set-option :${o._1} ${o._2})").mkString("\n")

    val axiomStrings = axiomRefs
      .map(r => s"${TAB * 1}${programPointToQueryTerm(r, 1)}")
      .mkString("\n")

    val assertionStrings = assertionRefs
      .map(r => s"${TAB * 2}${programPointToQueryTerm(r, 2)}")
      .mkString("\n")

    val spec = (assertionRefs.length > 0, axiomRefs.length > 0) match {
      case (true, true) =>
        s"(and\n$axiomStrings\n${TAB * 1}(or\n$assertionStrings))"
      case (true, false) => s"(or\n$assertionStrings)"
      case (false, true) => s"(and\n$axiomStrings)"
      case _             => ""
    }

    val body: String = if (spec != "") {
      if (termgraph.isSynthesisQuery) {
        programToQueryCtx() + "\n(constraint (not " + spec + "))"
      } else {
        programToQueryCtx() + "\n(assert " + spec + ")"
      }
    } else {
      if (getValues.isDefined) {
        programToQueryCtx()
      } else {
        ""
      } + "\n; nothing to verify"
    }

    val postQuery = if (checkQuery || traceQuery) {
      val model = if (getValues.isDefined) {
        val cmd = if (getValues.get.length == 0) {
          "(get-model)"
        } else {
          s"(get-value (${getValues.get.map(v => programPointToQueryTerm(v)).mkString(" ")}))"
        }
        "(echo \"Model\")\n" + cmd
      } else {
        ""
      }

      val proofStatus = if (assertionRefs.length + axiomRefs.length > 0) {
        "(echo \"Proof Status\")\n(get-assignment)"
      } else {
        ""
      }

      if (termgraph.isSynthesisQuery) {
        "\n\n(check-synth)"
      } else {
        "\n\n(check-sat)\n(echo \"\")\n" + proofStatus + "\n(echo \"\")\n" + model
      }
    } else {
      ""
    }

    s"$logicString\n$opts\n\n$body\n$postQuery"
  }
}
