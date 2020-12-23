package front

import scala.util.parsing.input.Position

class SyntaxError(
  val msg: String,
  val pos: Position,
  val filename: String
) extends java.lang.RuntimeException(msg, null) {

  lazy val positionStr = filename + ", line " + pos.line.toString

  override def toString(): String = s"${positionStr}\n${msg}"
}

class WrongTopeLevelDeclError(
  override val msg: String,
  override val pos: Position,
  override val filename: String
) extends SyntaxError(msg, pos, filename)

class MissingSemicolon(
  override val msg: String,
  override val pos: Position,
  override val filename: String
) extends SyntaxError(msg, pos, filename)

class BadLhs(
  override val msg: String,
  override val pos: Position,
  override val filename: String
) extends SyntaxError(msg, pos, filename)
