package com.uclid.context

import com.uclid.termgraph.TermGraph

abstract class Context(val termgraph: TermGraph) {
  def toQuery(prettyPrint: Int): String
  def ignoreResult() : Boolean
  def entryPoints() : List[Int]
}
