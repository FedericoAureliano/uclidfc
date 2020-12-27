package middle

import front._

import scala.util.parsing.input.Position

abstract class SemanticError(
  val pos: Position,
  val filename: Option[String]
) extends java.lang.RuntimeException {
  def info(): String

  override def toString(): String = {
    val userError = filename match {
      case None =>
        s"Semantic error on line ${pos.line}: ${info()}\n\n${pos.longString}\n"
      case Some(value) =>
        s"Semantic error on line ${pos.line} of ${value}: ${info()}\n\n${pos.longString}\n"
    }
    val uclidStackElement = getStackTrace().find(p => !p.isNativeMethod()).get
    val uclidError =
      s"The error was thrown at line ${uclidStackElement.getLineNumber()} of ${uclidStackElement.getFileName()}. If this error is spurious, please report it to the developers."

    userError + "\n" + uclidError
  }
}

class NotSupportedYet(
  val node: ASTNode
) extends SemanticError(node.pos, node.filename) {

  def info(): String =
    s"Expressions ${node.getClass().getSimpleName()} not suported yet!"
}

class TypeOutOfScope(
  val node: ASTNode
) extends SemanticError(node.pos, node.filename) {

  def info(): String =
    s"Type ${node.getClass().getSimpleName()} not declared in scope!"
}

class TypeOverride(
  val typ: TypeDecl
) extends SemanticError(typ.pos, typ.filename) {

  def info(): String =
    s"Type ${typ.id.name} already declared in this type closure!"
}

class ModuleOverride(
  val mod: ModuleDecl
) extends SemanticError(mod.pos, mod.filename) {

  def info(): String =
    s"Module ${mod.id.name} name already used in this type closure!"
}

class IdentifierOutOfScope(
  val id: Identifier
) extends SemanticError(id.pos, id.filename) {
  def info(): String = s"Identifier ${id.name} not declared in scope!"
}
