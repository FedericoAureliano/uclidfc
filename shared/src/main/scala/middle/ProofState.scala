package middle

import java.util.HashMap
import scala.collection.mutable.ListBuffer

class ProofState(
  val program: Program,
  var result: Option[Boolean] = None,
  var model: Option[Ref] = None,
  val messages: ListBuffer[String] = new ListBuffer()
) {

  override def toString(): String = {
    val answer = result match {
      case Some(true)  => "sat"
      case Some(false) => "unsat"
      case None        => "unknown/error/timeout"
    }
    (List(answer, "\nMessages:") ++ messages).mkString("\n")
  }
}
