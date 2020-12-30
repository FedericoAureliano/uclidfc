package front

import scala.util.parsing.input.Position

abstract class SyntaxError(
  val pos: Position,
  val filename: Option[String]
) extends java.lang.RuntimeException {
  def info(): String

  override def toString(): String = {
    val userError = filename match {
      case None =>
        s"Syntax error on line ${pos.line}: ${info()}\n\n${pos.longString}\n"
      case Some(value) =>
        s"Syntax error on line ${pos.line} of ${value}: ${info()}\n\n${pos.longString}\n"
    }

    val uclidStackElement = getStackTrace().find(p => !p.isNativeMethod()).get
    val uclidError =
      s"The error was thrown at line ${uclidStackElement.getLineNumber()} of ${uclidStackElement.getFileName()}. If this error is spurious, please report it to the developers."

    userError + "\n" + uclidError
  }
}

class ParseFailedError(
  override val pos: Position,
  override val filename: Option[String]
) extends SyntaxError(pos, filename) {
  def info(): String = "Failed to parse starting here"
}

class WrongTopeLevelDeclError(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {

  def info(): String =
    s"${node.getClass().getSimpleName()} is not a valid top-level declaration!"
}

class MissingSemicolon(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {

  def info(): String =
    s"Missing semicolon after ${node.getClass().getSimpleName()}!"
}

class BadLeftHandSideError(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {

  def info(): String =
    s"${node.getClass().getSimpleName()} is not a valid left-hand-side!"
}

class InvariantMissingNameError(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {
  def info(): String = s"Must name your invariants!"
}

class PrimeExpressionError(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {
  def info(): String = s"Cannot prime an expression!"
}

class ConstantMustBeLiteral(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {
  def info(): String = s"${node} is not a literal!"
}

class MissingCloseBracket(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {

  def info(): String =
    s"Missing closing bracket after ${node.getClass().getSimpleName()} statement!"
}

class TypeMustBeNamed(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {
  def info(): String = "Enums and records must be named before use!"
}

class MismatchingAssign(
  val node: ASTNode,
  val lhsCount: Int,
  val rhsCount: Int
) extends SyntaxError(node.pos, node.filename) {

  def info(): String =
    s"Number of expressions on left-hand-side (${lhsCount}) does not match number of expressions on right-hand-side (${rhsCount})!"
}

class TooManyCompositionsError(
  val node: ASTNode
) extends SyntaxError(node.pos, node.filename) {
  def info(): String = "A maximum of eight (8) compositions allowed at once!"
}
