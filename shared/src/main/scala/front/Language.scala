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

object ASTNode {

  def introducePos[T <: PositionedNode](
    setPosition: Boolean,
    setFilename: Boolean,
    node: T,
    pos: ASTPosition
  ): T =
    if (setPosition || node.pos.line == 0) {
      val nodeP = node
      if (setFilename || nodeP.filename.isEmpty) {
        nodeP.filename = pos.filename
      }
      nodeP.pos = pos.pos
      nodeP
    } else {
      node
    }

  def introducePos[T <: PositionedNode](
    setPosition: Boolean,
    setFilename: Boolean,
    node: Option[T],
    pos: ASTPosition
  ): Option[T] =
    node match {
      case Some(n) =>
        if (setPosition || n.pos.line == 0) {
          val nP = n
          if (setFilename || nP.filename.isEmpty) { nP.filename = pos.filename }
          nP.pos = pos.pos
          Some(nP)
        } else {
          Some(n)
        }
      case None =>
        None
    }

  def introducePos[T <: PositionedNode](
    setPosition: Boolean,
    setFilename: Boolean,
    nodes: List[T],
    pos: ASTPosition
  ): List[T] =
    nodes.map { (n) =>
      if (setPosition || n.pos.line == 0) {
        val nP = n
        if (setFilename || nP.filename.isEmpty) { nP.filename = pos.filename }
        nP.pos = pos.pos
        nP
      } else {
        n
      }
    }
}

/** All elements in the AST are derived from this class.
  *  The plan is to stick an ID into this later so that we can use the ID to store auxiliary information.
  */
sealed trait ASTNode extends Positional with PositionedNode {
  val astNodeId = IdGenerator.newId()
}

sealed trait Operator extends ASTNode {}

// This is the polymorphic operator type. They need to be re-written to the correct type
sealed abstract class PolymorphicOperator extends Operator {}

case class LTOp() extends PolymorphicOperator {
  override def toString = "<"
}

case class LEOp() extends PolymorphicOperator {
  override def toString = "<="
}

case class GTOp() extends PolymorphicOperator {
  override def toString = ">"
}

case class GEOp() extends PolymorphicOperator {
  override def toString = ">="
}

case class AddOp() extends PolymorphicOperator {
  override def toString = "+"
}

case class SubOp() extends PolymorphicOperator {
  override def toString = "-"
}

case class MulOp() extends PolymorphicOperator {
  override def toString = "*"
}

case class UnaryMinusOp() extends PolymorphicOperator {
  override def toString = "-"
}

// These operators take bitvector operands and return bitvector results.
sealed abstract class BVArgOperator() extends Operator {}

case class BVLTUOp() extends BVArgOperator() {
  override def toString = "<_u"
}

case class BVLEUOp() extends BVArgOperator() {
  override def toString = "<=_u"
}

case class BVGTUOp() extends BVArgOperator() {
  override def toString = ">_u"
}

case class BVGEUOp() extends BVArgOperator() {
  override def toString = ">=_u"
}

case class BVAndOp() extends BVArgOperator() {
  override def toString = "&"
}

case class BVOrOp() extends BVArgOperator() {
  override def toString = "|"
}

case class BVXorOp() extends BVArgOperator() {
  override def toString = "^"
}

case class BVNotOp() extends BVArgOperator() {
  override def toString = "~"
}

case class BVSignExtOp(val e: Int) extends BVArgOperator() {
  override def toString = "bv_sign_extend"
}

case class BVZeroExtOp(val e: Int) extends BVArgOperator() {
  override def toString = "bv_zero_extend"
}

case class BVLeftShiftBVOp() extends BVArgOperator() {
  override def toString = "bv_left_shift"
}

case class BVLRightShiftBVOp() extends BVArgOperator() {
  override def toString = "bv_l_right_shift"
}

case class BVARightShiftBVOp() extends BVArgOperator() {
  override def toString = "bv_a_right_shift"
}

case class BVUremOp() extends BVArgOperator() {
  override def toString = "%_u"
}

case class BVSremOp() extends BVArgOperator() {
  override def toString = "%"
}

// Boolean operators.
sealed abstract class BooleanOperator extends Operator {}

case class ConjunctionOp() extends BooleanOperator {
  override def toString = "and"
}

case class DisjunctionOp() extends BooleanOperator {
  override def toString = "or"
}

case class IffOp() extends BooleanOperator {
  override def toString = "<==>"
}

case class ImplicationOp() extends BooleanOperator {
  override def toString = "==>"
}

case class NegationOp() extends BooleanOperator {
  override def toString = "not"
}

// Quantifiers
sealed abstract class QuantifiedBooleanOperator extends BooleanOperator {
  def variables: List[(Identifier, Type)]
}

object QuantifiedBooleanOperator {

  def toString(
    quantifier: String,
    vs: List[(Identifier, Type)],
    patterns: List[List[Expr]]
  ) = {
    val args =
      Utils.join(vs.map((v) => v._1.toString + " : " + v._2.toString), ", ")
    val pats = if (patterns.size == 0) { "" }
    else {
      "pattern[" +
        Utils.join(
          patterns.map(p => Utils.join(p.map(_.toString()), ", ")),
          "; "
        ) +
        "] "
    }
    quantifier + " (" + args + ") " + pats + ":: "
  }
}

case class ForallOp(vs: List[(Identifier, Type)], patterns: List[List[Expr]])
    extends QuantifiedBooleanOperator {
  override def variables = vs

  override def toString =
    QuantifiedBooleanOperator.toString("forall", vs, patterns)
}

case class ExistsOp(vs: List[(Identifier, Type)], patterns: List[List[Expr]])
    extends QuantifiedBooleanOperator {

  override def toString =
    QuantifiedBooleanOperator.toString("exists", vs, patterns)
  override def variables = vs
}

// (In-)equality operators.
sealed abstract class ComparisonOperator() extends Operator {}

case class EqualityOp() extends ComparisonOperator {
  override def toString = "="
}

case class InequalityOp() extends ComparisonOperator {
  override def toString = "!="
}

// BV2Int and Int2BV
case class BV2SignedIntOp() extends Operator {
  override def toString() = "bv_to_signed_int"
}

case class BV2UnsignedIntOp() extends Operator {
  override def toString() = "bv_to_unsigned_int"
}

// Int2BV
case class Int2BVOp(val w: Int) extends Operator {
  override def toString() = "int_to_bv"
}

// ITE operator
case class ITEOp() extends Operator {
  override def toString = "ite"
}

abstract class BitVectorSlice extends ASTNode {
  def width: Option[Int]
  def isConstantWidth: Boolean
}

case class ConstBitVectorSlice(hi: Int, lo: Int) extends BitVectorSlice {
  Utils.assert(
    hi >= lo && hi >= 0 && lo >= 0,
    "Invalid bitvector slice: [" + hi.toString + ":" + lo.toString + "]."
  )
  override def width = Some(hi - lo + 1)
  override def isConstantWidth = true
  override def toString = "[" + hi.toString + ":" + lo.toString + "]"
}

case class VarBitVectorSlice(hi: Expr, lo: Expr, wd: Option[Int] = None)
    extends BitVectorSlice {
  override def toString = "[" + hi.toString + ":" + lo.toString + "]"
  override def width = wd
  override def isConstantWidth = wd.isDefined
}

sealed abstract class ExtractOp extends Operator

case class ConstExtractOp(slice: ConstBitVectorSlice) extends ExtractOp {
  override def toString = slice.toString
}

case class VarExtractOp(slice: VarBitVectorSlice) extends ExtractOp {
  override def toString = slice.toString()
}

case class ConcatOp() extends Operator {
  override def toString = "++"
}

case class PolymorphicSelect(id: Identifier) extends Operator {
  val ident = id
  override def toString = id.toString()
}

case class ArraySelect(indices: List[Expr]) extends Operator {}

case class ArrayUpdate(indices: List[Expr], value: Expr) extends Operator {}

case class GetNextValueOp() extends Operator {
  override def toString = "'"
}

case class DistinctOp() extends Operator {
  override def toString = "distinct"
}

sealed abstract class Expr extends ASTNode {}

case class Identifier(name: String) extends Expr {
  override def toString = name.toString
}

sealed abstract class Literal extends Expr {}

/** A non-deterministic new constant. */
case class FreshLit(typ: Type) extends Literal {}

sealed abstract class NumericLit extends Literal {
  def typeOf: NumericType
  def to(n: NumericLit): Seq[NumericLit]
  def negate: NumericLit
}

case class BoolLit(value: Boolean) extends Literal {
  override def toString = value.toString
}

case class IntLit(value: BigInt) extends NumericLit {
  override def toString = value.toString
  override def typeOf: NumericType = IntegerType()

  override def to(n: NumericLit): Seq[NumericLit] =
    n match {
      case i: IntLit => (value to i.value).map(IntLit(_))
      case _ =>
        throw new Utils.RuntimeError(
          "Cannot create range for differing types of numeric literals."
        )
    }
  override def negate = IntLit(-value)
}

case class BitVectorLit(value: BigInt, width: Int) extends NumericLit {
  override def toString = value.toString + "bv" + width.toString
  override def typeOf: NumericType = BitVectorType(width)

  override def to(n: NumericLit): Seq[NumericLit] =
    n match {
      case bv: BitVectorLit => (value to bv.value).map(BitVectorLit(_, width))
      case _ =>
        throw new Utils.RuntimeError(
          "Cannot create range for differing types of numeric literals."
        )
    }
  override def negate = BitVectorLit(-value, width)
}

case class StringLit(value: String) extends Literal {
  override def toString = "\"" + value + "\""
}

case class ConstArray(exp: Expr, typ: Type) extends Expr {
  override def toString = "const(%s, %s)".format(exp.toString(), typ.toString())
}

case class Tuple(values: List[Expr]) extends Expr {
  override def toString = "{" + Utils.join(values.map(_.toString), ", ") + "}"
}

//for symbols interpreted by underlying Theory solvers
case class OperatorApplication(op: Operator, operands: List[Expr])
    extends Expr {

  override def toString =
    op match {
      case PolymorphicSelect(r) =>
        operands(0).toString + "." + r.toString()
      case ForallOp(_, _) | ExistsOp(_, _) =>
        "(" + op.toString + operands(0).toString + ")"
      case _ =>
        op.toString + "(" + Utils.join(operands.map(_.toString), ", ") + ")"
    }
}

//for uninterpreted function symbols
case class FuncApplication(e: Expr, args: List[Expr]) extends Expr {

  override def toString =
    e.toString + "(" + Utils.join(args.map(_.toString), ", ") + ")"
}

case class ModuleNextCallExpr(expr: Expr) extends Expr {
  override def toString = "next (" + expr.toString + ")"
}

case class ModuleInitCallExpr(id: Identifier) extends Expr {
  override def toString = "init (" + id.toString + ")"
}

case class Lhs(val expr: Expr) extends ASTNode {
  override def toString = expr.toString
}

sealed abstract class Type extends PositionedNode {
  def ids = List.empty[Identifier]
  def matches(t2: Type) = (this == t2)
  def defaultValue: Option[Expr] = None
}

/** Primitive types: Int, Bool and BitVector.
  */
sealed abstract class PrimitiveType extends Type {}

/**  Numeric types base class. All numeric types are also primitive types.
  */
sealed abstract class NumericType extends PrimitiveType {}

/** Undefined type. These will eventually be rewritten in the AST.
  */
case class UndefinedType() extends Type {
  override def toString = "undefined"
}

/**  Uninterpreted types.
  */
case class UninterpretedType(name: Identifier) extends Type {
  override def toString = name.toString
}

/** Regular types.
  */
case class BooleanType() extends PrimitiveType {
  override def toString = "boolean"
  override def defaultValue = Some(BoolLit(false))
}

case class IntegerType() extends NumericType {
  override def toString = "integer"
  override def defaultValue = Some(IntLit(0))
}

case class BitVectorType(width: Int) extends NumericType {
  override def toString = "bv" + width.toString

  def isValidSlice(slice: ConstBitVectorSlice): Boolean =
    return (slice.lo >= 0 && slice.hi < width)
  override def defaultValue = Some(BitVectorLit(0, width))
}

case class StringType() extends PrimitiveType {
  override def toString = "string"
  override def defaultValue = Some(StringLit(""))
}

case class EnumType(ids_ : List[Identifier]) extends Type {
  override def ids = ids_

  override def toString =
    "enum {" +
      ids.tail.foldLeft(ids.head.toString)((acc, i) => acc + "," + i) + "}"

  override def defaultValue =
    ids match {
      case hd :: _ => Some(hd)
      case _       => None
    }
}

abstract sealed class ProductType extends Type {
  def fields: List[(Identifier, Type)]
  def numFields: Int = fields.length

  def fieldType(i: Int): Option[Type] =
    if (i >= 0 && i < fields.length) Some(fields(i)._2)
    else None

  def fieldType(fieldName: Identifier): Option[Type] =
    fieldIndex(fieldName) match {
      case -1 => None
      case i  => Some(fields(i)._2)
    }

  def nestedFieldType(fields: List[Identifier]): Option[Type] = {
    val thisType: Option[Type] = Some(this)
    fields.foldLeft(thisType) { (acc, f) =>
      acc.flatMap { (typ) =>
        typ match {
          case prodType: ProductType => prodType.fieldType(f)
          case _                     => None
        }
      }
    }
  }

  def fieldIndex(name: Identifier): Int = fields.indexWhere((p) => p._1 == name)

  def hasField(fieldName: Identifier): Boolean =
    fieldIndex(fieldName) != -1
}

case class TupleType(fieldTypes: List[Type]) extends ProductType {

  override def fields =
    fieldTypes.zipWithIndex.map(p =>
      (Identifier("_" + (p._2 + 1).toString), p._1)
    )

  override def toString =
    "{" + Utils.join(fieldTypes.map(_.toString), ", ") + "}"

  override def defaultValue = {
    val defaults = fieldTypes.map(_.defaultValue).flatten
    if (defaults.size == fieldTypes.size) {
      Some(Tuple(defaults))
    } else {
      None
    }
  }
}

case class RecordType(members: List[(Identifier, Type)]) extends ProductType {
  override def fields = members

  override def toString =
    "record {" + Utils.join(
      fields.map((f) => f._1.toString + " : " + f._2.toString),
      ", "
    ) + "}"

  override def matches(t2: Type): Boolean =
    t2 match {
      case tup: TupleType =>
        fields.size == tup.fieldTypes.size &&
          fields
            .map(_._2)
            .zip(tup.fieldTypes)
            .forall(tpair => tpair._1.matches(tpair._2))
      case _ => this == t2
    }
}

case class MapType(inTypes: List[Type], outType: Type) extends Type {

  override def toString =
    Utils.join(inTypes.map(_.toString), " * ") + " -> " + outType.toString
}

case class ArrayType(inTypes: List[Type], outType: Type) extends Type {

  override def toString =
    "[" + Utils.join(inTypes.map(_.toString), " * ") + "]" + outType.toString
}

case class SynonymType(id: Identifier) extends Type {
  override def toString = id.toString

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

/** Function signatures.
  */
case class FunctionSig(args: List[(Identifier, Type)], retType: Type)
    extends ASTNode {
  type T = (Identifier, Type)
  val typ = MapType(args.map(_._2), retType)
  val printfn = { (a: T) => a._1.toString + ": " + a._2 }
}

sealed abstract class Decl extends ASTNode {
  def declNames: List[Identifier]
}

case class TypeDecl(id: Identifier, typ: Type) extends Decl {
  override def declNames = List(id)
}

case class StateVarsDecl(ids: List[Identifier], typ: Type) extends Decl {
  override def declNames = ids
}

case class InputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {
  override def declNames = ids
}

case class OutputVarsDecl(ids: List[Identifier], typ: Type) extends Decl {
  override def declNames = ids
}

case class SharedVarsDecl(ids: List[Identifier], typ: Type) extends Decl {
  override def declNames = ids
}

/** This is base trait for all entities that are exported from a module. */
sealed abstract trait ModuleExternal {
  def extNames: List[Identifier]
  def extType: Type
}

case class ConstantLitDecl(id: Identifier, lit: NumericLit) extends Decl {
  override def declNames = List(id)
}

case class ConstantsDecl(ids: List[Identifier], typ: Type)
    extends Decl
    with ModuleExternal {
  override def declNames = ids
  override def extNames = ids
  override def extType = typ
}

case class ModuleConstantsImportDecl(id: Identifier) extends Decl {
  override def declNames = List.empty
}

case class FunctionDecl(id: Identifier, sig: FunctionSig)
    extends Decl
    with ModuleExternal {
  override def declNames = List(id)
  override def extNames = List(id)
  override def extType = sig.typ
}

case class ModuleFunctionsImportDecl(id: Identifier) extends Decl {

  override def declNames = List.empty
}

case class DefineDecl(id: Identifier, sig: FunctionSig, expr: Expr)
    extends Decl {

  override def declNames = List(id)
}

case class ModuleDefinesImportDecl(id: Identifier) extends Decl {
  override def declNames = List.empty
}

case class SynthesisFunctionDecl(
  id: Identifier,
  sig: FunctionSig,
  grammarId: Option[Identifier],
  grammarArgs: List[Identifier],
  conditions: List[Expr]
) extends Decl {
  override def declNames = List(id)
}

case class InitDecl(body: Statement) extends Decl {
  override def declNames = List.empty
}

case class NextDecl(body: Statement) extends Decl {
  override def declNames = List.empty
}

case class SpecDecl(id: Identifier, expr: Expr) extends Decl {
  val propertyKeyword = "property"

  override def declNames = List(id)
  def name = "%s %s".format(propertyKeyword, id.toString())
}

case class AxiomDecl(
  id: Option[Identifier],
  expr: Expr
) extends Decl {

  override def declNames = id match {
    case Some(i) => List(i)
    case _       => List.empty
  }
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

  lazy val constLits: List[(Identifier, NumericLit)] =
    decls.collect {
      case constLit: ConstantLitDecl =>
        (constLit.id, constLit.lit)
    }
  // module constants.
  lazy val constantDecls = decls.collect { case cnsts: ConstantsDecl => cnsts }

  lazy val constImportDecls: List[ModuleConstantsImportDecl] = decls.collect {
    case imp: ModuleConstantsImportDecl => imp
  }

  lazy val constants: List[(Identifier, Type)] =
    constantDecls.flatMap(cnst => cnst.ids.map(id => (id, cnst.typ)))

  lazy val funcImportDecls: List[ModuleFunctionsImportDecl] = decls.collect {
    case imp: ModuleFunctionsImportDecl => imp
  }

  // module functions.
  lazy val functions: List[FunctionDecl] =
    decls.filter(_.isInstanceOf[FunctionDecl]).map(_.asInstanceOf[FunctionDecl])

  // module macros
  lazy val defines: List[DefineDecl] = decls.collect { case d: DefineDecl => d }

  lazy val synthFunctions: List[SynthesisFunctionDecl] =
    decls
      .filter(_.isInstanceOf[SynthesisFunctionDecl])
      .map(_.asInstanceOf[SynthesisFunctionDecl])

  // module properties.
  lazy val properties: List[SpecDecl] = decls.collect {
    case spec: SpecDecl =>
      spec
  }

  // set of type declarations.
  lazy val typeDeclarationMap: Map[Identifier, Type] = decls
    .filter(_.isInstanceOf[TypeDecl])
    .map(_.asInstanceOf[TypeDecl])
    .map(t => (t.id -> t.typ))
    .toMap

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

  // find all axioms.
  lazy val axioms: List[AxiomDecl] = {
    decls.filter(_.isInstanceOf[AxiomDecl]).map(_.asInstanceOf[AxiomDecl])
  }
}
