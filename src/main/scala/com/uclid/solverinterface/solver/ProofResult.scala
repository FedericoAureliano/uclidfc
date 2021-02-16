package com.uclid.solverinterface.solver

import java.io.{File, PrintWriter}
import scala.sys.process._

class ProofResult(
  var result: Option[Boolean] = None,
  var messages: String = ""
) {

  override def toString(): String = {
    val extra = "Detailed Output:" + messages
    val answer = result match {
      case Some(true)  => "Counterexample!"
      case Some(false) => "Verified!"
      case None        => "Neither Verified Nor Rejected."
    }
    List(
      s"\n${"*" * answer.length()}\n" + answer + s"\n${"*" * answer.length()}\n",
      extra
    ).mkString("\n") + "\n\n"
  }
}
