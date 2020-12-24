package middle

import front._

import scala.util.parsing.input.Position

abstract class SemanticError(
  val pos: Position
) extends java.lang.RuntimeException {
  def info(): String

  override def toString(): String =
    s"Semantic error on line ${pos.line}: ${info()}\n\n${pos.longString}"
}

class ExprNotSupportedYet(
  override val pos: Position,
  val expr: Expr
) extends SemanticError(pos) {
  def info(): String = s"Expressions ${expr} not suported yet!"
}

class CommandNotSupportedYet(
  override val pos: Position,
  val cmd: ProofCommand
) extends SemanticError(pos) {
  def info(): String = s"Command ${cmd} not suported yet!"
}

class StatementNotSupportedYet(
  override val pos: Position,
  val stmt: Statement
) extends SemanticError(pos) {
  def info(): String = s"Statement ${stmt} not suported yet!"
}

class TypeNotSupportedYet(
  override val pos: Position,
  val typ: Type
) extends SemanticError(pos) {
  def info(): String = s"Type ${typ} not suported yet!"
}

class TypeOutOfScope(
  override val pos: Position,
  val typ: Type
) extends SemanticError(pos) {
  def info(): String = s"Type ${typ} not declared in scope!"
}

class TypeOverride(
  override val pos: Position,
  val typ: TypeDecl
) extends SemanticError(pos) {

  def info(): String =
    s"Type ${typ.id.name} already declared in this type closure!"
}

class ModuleOverride(
  override val pos: Position,
  val mod: ModuleDecl
) extends SemanticError(pos) {

  def info(): String =
    s"Module ${mod.id.name} name already used in this type closure!"
}

class IdentifierOutOfScope(
  override val pos: Position,
  val id: Identifier
) extends SemanticError(pos) {
  def info(): String = s"Identifier ${id.name} not declared in scope!"
}
