package com.uclid.context

import com.uclid.termgraph.TermGraph

abstract class Context(val termgraph: TermGraph) {
  def toQuery(prettyPrint: Boolean): String
  def ignoreResult() : Boolean
  def entryPoints() : List[Int]
}
