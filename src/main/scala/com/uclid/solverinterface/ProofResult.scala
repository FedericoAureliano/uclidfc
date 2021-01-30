package com.uclid.solverinterface



import sys.process._
import java.io.{File, PrintWriter}

class ProofResult(
  var result: Option[Boolean] = None,
  var messages: String = ""
) {

  override def toString(): String = {
    val extra = "Detailed Output:" + messages
    val answer = result match {
      case Some(true)  => "Rejected!"
      case Some(false) => "Verified!"
      case None        => "Neither Verified Nor Rejected."
    }
    List(
      s"\n\n${"*" * answer.length()}\n" + answer + s"\n${"*" * answer.length()}\n",
      extra
    ).mkString("\n") + "\n"
  }
}
