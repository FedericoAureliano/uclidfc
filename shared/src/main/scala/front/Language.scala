package front

import scala.util.parsing.input.Positional
import scala.util.parsing.input.Position

/** Singleton that generates unique ids for AST nodes. */
object IdGenerator {
  var idCounter: Int = 0

  def newId(): Int = {
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

sealed trait ASTNode extends Positional with PositionedNode {
  val astNodeId = IdGenerator.newId()
}

// operators (interpreted symbols), statements, and expressions
sealed trait TermNode extends ASTNode

sealed trait Command extends ASTNode

sealed trait Operator extends TermNode {
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
  def variables: List[(Identifier, InlineType)]
}

case class ForallOp(vs: List[(Identifier, InlineType)]) extends Quantifier {
  override def variables = vs
  override val name = "forall"
}

case class ExistsOp(vs: List[(Identifier, InlineType)]) extends Quantifier {
  override def variables = vs
  override val name = "exists"
}

case class ArraySelect() extends Operator {
  override val name = "select"
}

case class ArrayUpdate() extends Operator {
  override val name = "store"
}

case class GetNextValueOp() extends Operator {
  override val name = "'"
}

case class ConstArray(typ: Type) extends Operator {
  override val name = "as const"
}

case class PolymorphicSelect(id: Identifier) extends Operator {
  override val name = ""
}

sealed abstract class Expr extends TermNode {}

case class Identifier(name: String) extends Expr {}

sealed abstract class Literal extends Expr {
  def negate(): Literal
  def value(): String
}

case class BoolLit(literal: Boolean) extends Literal {
  override def negate() = BoolLit(!literal)
  override def value(): String = literal.toString()
}

case class IntLit(literal: BigInt) extends Literal {
  override def negate() = IntLit(-literal)
  override def value(): String = literal.toString()
}

case class FreshLit(typ: Type) extends Literal {
  override def negate() = FreshLit(typ)
  override def value(): String = "???"
}

case class ModuleNextCallExpr(expr: Expr) extends Expr {}

case class ModuleInitCallExpr(expr: Expr) extends Expr {}

//for symbols interpreted by underlying Theory solvers
case class OperatorApplication(op: Operator, operands: List[Expr])
    extends Expr {}

//for uninterpreted function symbols
case class FunctionApplication(e: Expr, args: List[Expr]) extends Expr {}

sealed abstract class Type extends ASTNode {
  def defaultVal(): Option[Expr];
}

// types that can be used in line without first declaring
sealed abstract class InlineType extends Type {}

case class BooleanType() extends InlineType {
  def defaultVal(): Option[Expr] = Some(BoolLit(false))
}

case class IntegerType() extends InlineType {
  def defaultVal(): Option[Expr] = Some(IntLit(0))
}

case class EnumType(variants: List[Identifier]) extends Type {
  def defaultVal(): Option[Expr] = Some(variants.head)
}

case class RecordType(elements: List[(Identifier, InlineType)]) extends Type {
  def defaultVal(): Option[Expr] = Some(elements.head._1)
}

sealed abstract class ComposedType(types: List[InlineType]) extends Type {
  if (types.length > 8) {
    throw new TooManyCompositionsError(types.last)
  }

  val fields = List(
    "first",
    "second",
    "third",
    "fourth",
    "fifth",
    "sixth",
    "seventh",
    "eigth"
  ).zip(types)
}

case class ConjunctionComposition(types: List[InlineType])
    extends ComposedType(types) {
  def defaultVal(): Option[Expr] = None
}

case class DisjunctionComposition(types: List[InlineType])
    extends ComposedType(types) {
  def defaultVal(): Option[Expr] = None
}

case class ArrayType(inType: InlineType, outType: InlineType)
    extends InlineType {

  def defaultVal(): Option[Expr] =
    outType.defaultVal() match {
      case Some(value) =>
        Some(
          OperatorApplication(
            ConstArray(ArrayType(inType, outType)),
            List(value)
          )
        )
      case None => None
    }

}

case class NamedType(id: Identifier) extends InlineType {
  def defaultVal(): Option[Expr] = None
}

/** Statements * */
sealed abstract class Statement extends TermNode {}

case class HavocStmt(toHavoc: Expr) extends Statement {}

case class AssumeStmt(pred: Expr) extends Statement {}

case class AssertStmt(pred: Expr) extends Statement {}

case class AssignStmt(lhs: Expr, rhs: Expr) extends Statement {}

case class BlockStmt(stmts: List[Statement]) extends Statement {}

case class IfElseStmt(cond: Expr, ifblock: Statement, elseblock: Statement)
    extends Statement {}

case class CaseStmt(body: List[(Expr, Statement)]) extends Statement {}

case class ModuleNextCallStmt(expr: Expr) extends Statement {}

sealed abstract class Decl extends ASTNode {}

sealed abstract class TopLevelDecl extends Decl

case class TypeDecl(id: Identifier, typ: Option[Type]) extends TopLevelDecl {}

case class StateVarsDecl(ids: List[Identifier], typ: InlineType) extends Decl {}

case class InputVarsDecl(ids: List[Identifier], typ: InlineType) extends Decl {}

case class OutputVarsDecl(ids: List[Identifier], typ: InlineType) extends Decl {}

case class SharedVarsDecl(ids: List[Identifier], typ: InlineType) extends Decl {}

case class DefineDecl(
  id: Identifier,
  params: List[(Identifier, InlineType)],
  retTyp: Type,
  expr: Expr
) extends TopLevelDecl {}

case class FunctionDecl(
  id: Identifier,
  argTypes: List[Type],
  retTyp: Type
) extends TopLevelDecl {}

case class SynthesisDecl(
  id: Identifier,
  params: List[(Identifier, InlineType)],
  retTyp: Type
) extends TopLevelDecl {}

case class InitDecl(body: BlockStmt) extends Decl {}

case class NextDecl(body: BlockStmt) extends Decl {}

case class SpecDecl(id: Identifier, expr: Expr) extends Decl {}

case class ProofCommand(
  name: Identifier,
  k: Option[IntLit]
) extends Command

case class SolverOption(name: String, option: String) extends Command

case class ModuleDecl(
  id: Identifier,
  decls: List[Decl],
  cmds: List[Command]
) extends TopLevelDecl {

  // module types.
  val typeDecls: List[TypeDecl] =
    decls
      .collect { case typs: TypeDecl => typs }

  // module inputs.
  val inputs: List[(Identifier, InlineType)] =
    decls
      .collect { case inps: InputVarsDecl => inps }
      .flatMap(i => i.ids.map(id => (id, i.typ)))

  // module outputs.
  val outputs: List[(Identifier, InlineType)] =
    decls
      .collect { case outs: OutputVarsDecl => outs }
      .flatMap(o => o.ids.map(id => (id, o.typ)))

  // module state variables.
  val vars: List[(Identifier, InlineType)] =
    decls
      .collect { case vars: StateVarsDecl => vars }
      .flatMap(v => v.ids.map(id => (id, v.typ)))

  val sharedVars: List[(Identifier, InlineType)] =
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

  val synthesis: List[SynthesisDecl] =
    decls
      .collect { case cnsts: SynthesisDecl => cnsts }

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
