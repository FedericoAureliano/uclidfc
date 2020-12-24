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
case class ASTPosition(filename: Option[String], pos: Position) {}

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

case class LTOp() extends Operator {
  override val name = "<"
}

case class LEOp() extends Operator {
  override val name = "<="
}

case class GTOp() extends Operator {
  override val name = ">"
}

case class GEOp() extends Operator {
  override val name = ">="
}

case class AddOp() extends Operator {
  override val name = "+"
}

case class SubOp() extends Operator {
  override val name = "-"
}

case class MulOp() extends Operator {
  override val name = "*"
}

case class UnaryMinusOp() extends Operator {
  override val name = "-"
}

case class ConjunctionOp() extends Operator {
  override val name = "and"
}

case class DisjunctionOp() extends Operator {
  override val name = "or"
}

case class IffOp() extends Operator {
  override val name = "="
}

case class ImplicationOp() extends Operator {
  override val name = "=>"
}

case class NegationOp() extends Operator {
  override val name = "not"
}

case class EqualityOp() extends Operator {
  override val name = "="
}

case class ITEOp() extends Operator {
  override val name = "ite"
}

sealed abstract class Quantifier extends Operator {
  def variables: List[(Identifier, Type)]
}

case class ForallOp(vs: List[(Identifier, Type)]) extends Quantifier {
  override def variables = vs

  override val name =
    s"forall${vs.map(p => p._1.name + p._2.name).mkString("")}"
}

case class ExistsOp(vs: List[(Identifier, Type)]) extends Quantifier {
  override def variables = vs

  override val name =
    s"exists${vs.map(p => p._1.name + p._2.name).mkString("")}"
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
  val name: String
}

/**  Uninterpreted types.
  */
case class UninterpretedType() extends Type {
  override val name = "usort"
}

/** Regular types.
  */
case class BooleanType() extends Type {
  override val name = "boolean"
}

case class IntegerType() extends Type {
  override val name = "integer"
}

case class EnumType(variants: List[Identifier]) extends Type {
  override val name = "enum"
}

case class RecordType(elements: List[(Identifier, Type)]) extends Type {
  override val name = "record"
}

case class ArrayType(inTypes: List[Type], outType: Type) extends Type {

  override val name =
    s"[${inTypes.map(p => p.name).mkString("")}]${outType.name}"
}

case class SynonymType(id: Identifier) extends Type {
  override val name = id.name
}

/** Statements * */
sealed abstract class Statement extends ASTNode {}

case class AssignStmt(lhss: List[Lhs], rhss: List[Expr]) extends Statement {}

case class BlockStmt(stmts: List[Statement]) extends Statement {}

case class IfElseStmt(cond: Expr, ifblock: Statement, elseblock: Statement)
    extends Statement {}

case class CaseStmt(body: List[(Expr, Statement)]) extends Statement {}

case class ModuleNextCallStmt(expr: Expr) extends Statement {}

sealed abstract class Decl extends ASTNode {}

sealed abstract class TopLevelDecl extends Decl

case class TypeDecl(id: Identifier, typ: Type) extends TopLevelDecl {}

case class StateVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class InputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class OutputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class SharedVarsDecl(ids: List[Identifier], typ: Type) extends Decl {}

case class DefineDecl(
  id: Identifier,
  params: List[(Identifier, Type)],
  retTyp: Type,
  expr: Expr
) extends TopLevelDecl {}

case class FunctionDecl(
  id: Identifier,
  argTypes: List[Type],
  retTyp: Type
) extends TopLevelDecl {}

case class InitDecl(body: BlockStmt) extends Decl {}

case class NextDecl(body: BlockStmt) extends Decl {}

case class SpecDecl(id: Identifier, expr: Expr) extends Decl {}

case class ProofCommand(
  name: Identifier,
  k: Option[IntLit]
) extends ASTNode

case class ModuleDecl(
  id: Identifier,
  decls: List[Decl],
  cmds: List[ProofCommand]
) extends TopLevelDecl {

  // module types.
  val typeDecls: List[TypeDecl] =
    decls
      .collect { case typs: TypeDecl => typs }

  // module inputs.
  val inputs: List[(Identifier, Type)] =
    decls
      .collect { case inps: InputVarsDecl => inps }
      .flatMap(i => i.ids.map(id => (id, i.typ)))

  // module outputs.
  val outputs: List[(Identifier, Type)] =
    decls
      .collect { case outs: OutputVarsDecl => outs }
      .flatMap(o => o.ids.map(id => (id, o.typ)))

  // module state variables.
  val vars: List[(Identifier, Type)] =
    decls
      .collect { case vars: StateVarsDecl => vars }
      .flatMap(v => v.ids.map(id => (id, v.typ)))

  val sharedVars: List[(Identifier, Type)] =
    decls
      .collect { case sVars: SharedVarsDecl => sVars }
      .flatMap(sVar => sVar.ids.map(id => (id, sVar.typ)))

  val defines: List[DefineDecl] =
    decls
      .collect {
        case defi: DefineDecl => defi
      }

  val functions: List[FunctionDecl] =
    decls
      .collect { case cnsts: FunctionDecl => cnsts }

  // module properties.
  val properties: List[SpecDecl] = decls.collect {
    case spec: SpecDecl =>
      spec
  }

  // the init block.
  val init: Option[InitDecl] = {
    val collected = decls.collect { case i: InitDecl => i }
    assert(collected.length <= 1, "must have at most one init block")
    collected.find(_.isInstanceOf[InitDecl])
  }

  // the next block.
  val next: Option[NextDecl] = {
    val collected = decls.collect { case i: NextDecl => i }
    assert(collected.length <= 1, "must have at most one next block")
    collected.find(_.isInstanceOf[NextDecl])
  }
}
