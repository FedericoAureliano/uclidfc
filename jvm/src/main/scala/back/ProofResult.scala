package back

import middle._

import java.util.HashMap
import scala.collection.mutable.ListBuffer

class ProofResult(
  var result: Option[Boolean] = None,
  var model: Option[Ref] = None,
  var messages: List[String] = List.empty
) {

  override def toString(): String = {
    val answer = result match {
      case Some(true)  => "Counterexample"
      case Some(false) => "Verification Passes"
      case None        => "unknown/error/timeout"
    }
    (List(answer, "Solver Output:") ++ messages)
      .mkString("\n") + "\n"
  }
}
