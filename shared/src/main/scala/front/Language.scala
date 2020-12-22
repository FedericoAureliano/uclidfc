package front

import scala.collection.immutable.Map
import scala.collection.mutable.{Map => MutableMap}
import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position
import scala.reflect.ClassTag

/** Singleton that generates unique ids for AST nodes. */
object IdGenerator {
  type Id = Int
  var idCounter: Id = 0

  def newId(): Id = {
    val id = idCounter
    idCounter = idCounter + 1
    return id
  }
}

/** An AST position consists of a filename and the Position type from the Scala std library.
  */
case class ASTPosition(filename: Option[String], pos: Position) {

  override def toString: String =
    filename match {
      case Some(fn) => fn + ", line " + pos.line.toString
      case None     => "line " + pos.line.toString
    }

  def errorString(): String =
    if (pos.line > 0) {
      filename match {
        case Some(fn) => " at " + fn + ", line " + pos.line.toString()
        case None     => "at line " + pos.line.toString()
      }
    } else {
      ""
    }
}

sealed trait PositionedNode extends Positional {
  var filename: Option[String] = None
  def position = ASTPosition(filename, pos)
}

/** All elements in the AST are derived from this class.
  *  The plan is to stick an ID into this later so that we can use the ID to store auxiliary information.
  */
sealed trait ASTNode extends Positional with PositionedNode {
  val astNodeId = IdGenerator.newId()
}

sealed trait Operator extends ASTNode {
  val name: String
}

// This is the polymorphic operator type. They need to be re-written to the correct type
sealed abstract class PolymorphicOperator extends Operator {}

case class LTOp() extends PolymorphicOperator {
  override val name = "<"
}

case class LEOp() extends PolymorphicOperator {
  override val name = "<="
}

case class GTOp() extends PolymorphicOperator {
  override val name = ">"
}

case class GEOp() extends PolymorphicOperator {
  override val name = ">="
}

case class AddOp() extends PolymorphicOperator {
  override val name = "+"
}

case class SubOp() extends PolymorphicOperator {
  override val name = "-"
}

case class MulOp() extends PolymorphicOperator {
  override val name = "*"
}

case class UnaryMinusOp() extends PolymorphicOperator {
  override val name = "-"
}

// Boolean operators.
sealed abstract class BooleanOperator extends Operator {}

case class ConjunctionOp() extends BooleanOperator {
  override val name = "and"
}

case class DisjunctionOp() extends BooleanOperator {
  override val name = "or"
}

case class IffOp() extends BooleanOperator {
  override val name = "="
}

case class ImplicationOp() extends BooleanOperator {
  override val name = "=>"
}

case class NegationOp() extends BooleanOperator {
  override val name = "not"
}

// (In-)equality operators.
sealed abstract class ComparisonOperator() extends Operator {}

case class EqualityOp() extends ComparisonOperator {
  override val name = "="
}

case class InequalityOp() extends ComparisonOperator {
  override val name = "!="
}

// ITE operator
case class ITEOp() extends Operator {
  override val name = "ite"
}

case class PolymorphicSelect(id: Identifier) extends Operator {
  val ident = id
  override val name = id.toString()
}

case class ArraySelect(indices: List[Expr]) extends Operator {
  override val name = "select"
}

case class ArrayUpdate(indices: List[Expr], value: Expr) extends Operator {
  override val name = "store"
}

case class GetNextValueOp() extends Operator {
  override val name = "'"
}

sealed abstract class Expr extends ASTNode {}

case class Identifier(name: String) extends Expr {
  override def toString = name.toString
}

sealed abstract class Literal extends Expr {
  def negate(): Literal
}

case class BoolLit(value: Boolean) extends Literal {
  override def toString = value.toString
  override def negate() = BoolLit(!value)
}

case class IntLit(value: BigInt) extends Literal {
  override def toString = value.toString
  override def negate() = IntLit(-value)
}

case class ConstArray(exp: Expr, typ: Type) extends Expr {
  override def toString = "const(%s, %s)".format(exp.toString(), typ.toString())
}

//for symbols interpreted by underlying Theory solvers
case class OperatorApplication(op: Operator, operands: List[Expr])
    extends Expr {}

//for uninterpreted function symbols
case class FuncApplication(e: Expr, args: List[Expr]) extends Expr {}

case class ModuleNextCallExpr(expr: Expr) extends Expr {}

case class ModuleInitCallExpr(id: Identifier) extends Expr {}

case class Lhs(val expr: Expr) extends ASTNode {}

sealed abstract class Type extends PositionedNode {
  def ids = List.empty[Identifier]
  def matches(t2: Type) = (this == t2)
  def defaultValue: Option[Expr] = None
  val name: String
}

/**  Uninterpreted types.
  */
case class UninterpretedType(nameIn: Identifier) extends Type {
  override val name = nameIn.toString
}

/** Regular types.
  */
case class BooleanType() extends Type {
  override val name = "boolean"
  override def defaultValue = Some(BoolLit(false))
}

case class IntegerType() extends Type {
  override val name = "integer"
  override def defaultValue = Some(IntLit(0))
}

case class ArrayType(inTypes: List[Type], outType: Type) extends Type {

  override val name = "array"
}

case class SynonymType(id: Identifier) extends Type {
  override val name = id.toString

  override def equals(other: Any) = other match {
    case that: SynonymType => that.id.name == this.id.name
    case _                 => false
  }
}

/** Statements * */
sealed abstract class Statement extends ASTNode {}

case class SkipStmt() extends Statement {}

case class AssertStmt(e: Expr, id: Option[Identifier]) extends Statement {}

case class AssumeStmt(e: Expr, id: Option[Identifier]) extends Statement {}

case class HavocStmt(havocable: Identifier) extends Statement {}

case class AssignStmt(lhss: List[Lhs], rhss: List[Expr]) extends Statement {}

case class BlockStmt(stmts: List[Statement]) extends Statement {}

case class IfElseStmt(cond: Expr, ifblock: Statement, elseblock: Statement)
    extends Statement {}

case class CaseStmt(body: List[(Expr, Statement)]) extends Statement {}

case class ModuleNextCallStmt(expr: Expr) extends Statement {}

case class BlockVarsDecl(ids: List[Identifier], typ: Type) extends ASTNode {}

sealed abstract class Decl extends ASTNode {}

case class TypeDecl(id: Option[Identifier], typ: Type) extends Decl {}

case class StateVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class InputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class OutputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class SharedVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class DefineDecl(
  id: Identifier,
  params: List[(Identifier, Type)],
  retTyp: Type,
  expr: Expr
) extends Decl {}

case class FunctionsDecl(
  ids: List[Identifier],
  argTypes: List[Type],
  retTyp: Type
) extends Decl {}

case class InitDecl(body: Statement) extends Decl {}

case class NextDecl(body: Statement) extends Decl {}

case class SpecDecl(id: Identifier, expr: Expr) extends Decl {
  val propertyKeyword = "invariant"
  def name = "%s %s".format(propertyKeyword, id.toString())
}

case class ProofCommand(
  name: Identifier,
  k: Option[IntLit]
) extends ASTNode

case class Module(
  id: Identifier,
  decls: List[Decl],
  cmds: List[ProofCommand]
) extends ASTNode {

  // create a new module with with the filename set.
  def withFilename(name: String): Module = {
    val newModule = Module(id, decls, cmds)
    newModule.filename = Some(name)
    return newModule
  }

  // module types.
  lazy val typeDecls: List[TypeDecl] =
    decls
      .collect { case typs: TypeDecl => typs }

  // module inputs.
  lazy val inputs: List[(Identifier, Type)] =
    decls
      .collect { case inps: InputVarsDecl => inps }
      .flatMap(i => i.ids.map(id => (id, i.typ)))

  // module outputs.
  lazy val outputs: List[(Identifier, Type)] =
    decls
      .collect { case outs: OutputVarsDecl => outs }
      .flatMap(o => o.ids.map(id => (id, o.typ)))

  // module state variables.
  lazy val vars: List[(Identifier, Type)] =
    decls
      .collect { case vars: StateVarsDecl => vars }
      .flatMap(v => v.ids.map(id => (id, v.typ)))

  lazy val sharedVars: List[(Identifier, Type)] =
    decls
      .collect { case sVars: SharedVarsDecl => sVars }
      .flatMap(sVar => sVar.ids.map(id => (id, sVar.typ)))

  lazy val defines: List[(Identifier, List[(Identifier, Type)], Type, Expr)] =
    decls
      .collect {
        case defi: DefineDecl =>
          (defi.id, defi.params, defi.retTyp, defi.expr)
      }

  lazy val functions: List[(Identifier, List[Type], Type)] =
    decls
      .collect { case cnsts: FunctionsDecl => cnsts }
      .flatMap(cnst => cnst.ids.map(id => (id, cnst.argTypes, cnst.retTyp)))

  // module properties.
  lazy val properties: List[SpecDecl] = decls.collect {
    case spec: SpecDecl =>
      spec
  }

  // the init block.
  lazy val init: Option[InitDecl] = {
    decls
      .find(_.isInstanceOf[InitDecl])
      .flatMap((d) => Some(d.asInstanceOf[InitDecl]))
  }

  // the next block.
  lazy val next: Option[NextDecl] = {
    decls
      .find(_.isInstanceOf[NextDecl])
      .flatMap((d) => Some(d.asInstanceOf[NextDecl]))
  }
}
