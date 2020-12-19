package front

import scala.util.parsing.input.Position
import java.io.File
import java.io.PrintWriter

object Utils {

  class RuntimeError(msg: String = null, cause: Throwable = null)
      extends java.lang.RuntimeException(msg, cause)

  class ParserError(
    val msg: String,
    val pos: Option[Position],
    val filename: Option[String]
  ) extends java.lang.RuntimeException(msg, null) {

    override def hashCode: Int =
      msg.hashCode() + pos.hashCode() + filename.hashCode()
    def errorName = "Parser"

    lazy val positionStr = (filename, pos) match {
      case (Some(f), Some(p)) => f.toString + ", line " + p.line.toString
      case (None, Some(p))    => "line " + p.line.toString
      case _                  => ""
    }

    lazy val fullStr = pos match {
      case Some(p) => p.longString
      case None    => ""
    }
  }

  class TypeError(msg: String, pos: Option[Position], filename: Option[String])
      extends ParserError(msg, pos, filename) {

    override def equals(that: Any): Boolean =
      that match {
        case that: TypeError =>
          (msg == that.msg) && (pos == that.pos) && filename == that.filename
        case _ =>
          false
      }
    override def errorName = "Type"
  }

  class SyntaxError(
    msg: String,
    pos: Option[Position],
    filename: Option[String]
  ) extends ParserError(msg, pos, filename) {

    override def equals(that: Any): Boolean =
      that match {
        case that: SyntaxError =>
          (msg == that.msg) && (pos == that.pos) && filename == that.filename
        case _ => false
      }
    override def errorName = "Syntax"
  }
}
