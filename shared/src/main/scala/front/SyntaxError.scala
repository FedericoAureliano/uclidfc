package front

import scala.util.parsing.input.Position

abstract class SyntaxError(
  val pos: Position,
  val filename: String
) extends java.lang.RuntimeException {
  def info(): String

  override def toString(): String =
    s"Syntax error on line ${pos.line} of ${filename}: ${info()}\n\n${pos.longString}"
}

class ParseFailedError(
  override val pos: Position,
  override val filename: String
) extends SyntaxError(pos, filename) {
  def info(): String = "Failed to parse starting here"
}

class WrongTopeLevelDeclError(
  override val pos: Position,
  override val filename: String,
  val decl: Decl
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"${decl.getClass().getSimpleName()} is not a valid top-level declaration!"
}

class MissingSemicolonStatement(
  override val pos: Position,
  override val filename: String,
  val stmt: Statement
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing semicolon after ${stmt.getClass().getSimpleName()} statement!"
}

class MissingSemicolonCommand(
  override val pos: Position,
  override val filename: String,
  val cmnd: ProofCommand
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing semicolon after ${cmnd.getClass().getSimpleName()} command!"
}

class MissingSemicolonDecl(
  override val pos: Position,
  override val filename: String,
  val decl: Decl
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing semicolon after ${decl.getClass().getSimpleName()} declaration!"
}

class BadLeftHandSideError(
  override val pos: Position,
  override val filename: String,
  val lhs: Expr
) extends SyntaxError(pos, filename) {
  def info(): String = s"${lhs} is not a valid left-hand-side!"
}

class InvariantMissingNameError(
  override val pos: Position,
  override val filename: String
) extends SyntaxError(pos, filename) {
  def info(): String = s"Must name your invariants!"
}

class PrimeExpressionError(
  override val pos: Position,
  override val filename: String
) extends SyntaxError(pos, filename) {
  def info(): String = s"Cannot prime an expression!"
}

class ConstantMustBeLiteral(
  override val pos: Position,
  override val filename: String,
  val lit: Expr
) extends SyntaxError(pos, filename) {
  def info(): String = s"${lit} is not a literal!"
}

class MissingCloseBracketStatement(
  override val pos: Position,
  override val filename: String,
  val stmt: Statement
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing closing bracket after ${stmt.getClass().getSimpleName()} statement!"
}

class MissingCloseBracketDecl(
  override val pos: Position,
  override val filename: String,
  val decl: Decl
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing closing bracket after ${decl.getClass().getSimpleName()} declaration!"
}

class MissingCloseBracketCommand(
  override val pos: Position,
  override val filename: String,
  val cmd: ProofCommand
) extends SyntaxError(pos, filename) {

  def info(): String =
    s"Missing closing bracket after ${cmd.getClass().getSimpleName()} command!"
}

class TypeMustBeNamed(
  override val pos: Position,
  override val filename: String
) extends SyntaxError(pos, filename) {
  def info(): String = s"Enums and records must be named before use!"
}
