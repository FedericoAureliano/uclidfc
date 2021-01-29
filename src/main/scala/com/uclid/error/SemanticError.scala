package com.uclid.error

import com.uclid.uclidlanguage.parser._

import scala.util.parsing.input.Position

abstract class SemanticError(
  val pos: Position
) extends java.lang.RuntimeException {
  def info(): String

  override def toString(): String = {
    val userError = s"Semantic error on line ${pos.line}: ${info()}\n\n${pos.longString}\n"
    val uclidStackElement = getStackTrace().find(p => !p.isNativeMethod()).get
    val uclidError =
      s"The error was thrown at line ${uclidStackElement.getLineNumber()} of ${uclidStackElement.getFileName()}. If this error is spurious, please report it to the developers."

    userError + "\n" + uclidError
  }
}

class InternalNotSupportedYet(
  val node: UclidAST
) extends SemanticError(node.pos) {

  def info(): String =
    s"Expressions ${node.getClass().getSimpleName()} not suported yet!"
}

class TypeOutOfScope(
  val node: UclidAST
) extends SemanticError(node.pos) {

  def info(): String =
    s"Type ${node.getClass().getSimpleName()} not declared in scope!"
}

class TypeOverride(
  val typ: TypeDecl
) extends SemanticError(typ.pos) {

  def info(): String =
    s"Type ${typ.id.name} already declared in this type closure!"
}

class ModuleOverride(
  val mod: ModuleDecl
) extends SemanticError(mod.pos) {

  def info(): String =
    s"Module ${mod.id.name} name already used in this type closure!"
}

class IdentifierOutOfScope(
  val id: Identifier
) extends SemanticError(id.pos) {
  def info(): String = s"Identifier ${id.name} not declared in scope!"
}
