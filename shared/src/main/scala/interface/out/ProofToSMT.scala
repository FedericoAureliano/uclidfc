package interface.out

import scala.collection.mutable.ListBuffer

import middle.core
import middle.core.{
  Application,
  Constructor,
  DataType,
  FunctionParameter,
  Instruction,
  Numeral,
  Program,
  Ref,
  Selector,
  SortMacro,
  SortParameter,
  TheoryMacro,
  TheorySort,
  UserFunction,
  UserMacro,
  UserSort
}
import middle.core.rewrite
import front.{
  ArrayType,
  AssertStmt,
  AssignStmt,
  AssumeStmt,
  BitVectorType,
  BlockStmt,
  BooleanType,
  CaseStmt,
  ConstArray,
  EnumType,
  Expr,
  ExternalType,
  ForStmt,
  FuncApplication,
  HavocStmt,
  Identifier,
  IfElseStmt,
  InitDecl,
  IntegerType,
  Lhs,
  Literal,
  MapType,
  ModuleCallStmt,
  ModuleInstanceType,
  ModuleType,
  NextDecl,
  OperatorApplication,
  ProcedureCallStmt,
  ProcedureType,
  RecordType,
  SkipStmt,
  StringType,
  SynonymType,
  Tuple,
  TupleType,
  Type,
  UndefinedType,
  UninterpretedType,
  WhileStmt
}

package object smt {

  // Get all neccesary declarations from term
  def getSmtCtxString(term: Program): String = {
    var declarations = new ListBuffer[String]()
    term.stmts.foreach { instruction =>
      instruction match {
        case UserFunction(name, sort, params) => {
          val sortProgram = getSmtSortName(term, sort.loc)
          if (params.length > 0) {
            val args = params
              .map(p => getFunctionParameterDeclaration(term, p.loc))
              .mkString(" ")
            declarations.addOne(s"(declare-fun $name ($args) $sortProgram)")
          } else {
            declarations.addOne(s"(declare-const $name $sortProgram)")
          }
        }
        case UserMacro(name, sort, body, params) => {
          val sortProgram = getSmtSortName(term, sort.loc)
          val bodyProgram = getSmtTermString_i(term, body.loc)
          val args = params
            .map(p => getFunctionParameterDeclaration(term, p.loc))
            .mkString(" ")
          declarations.addOne(
            s"(define-fun $name ($args) $sortProgram $bodyProgram)"
          )
        }
        case DataType(name, params) => {
          val constructors =
            params.map(p => getConstructorSMTString(term, p.loc)).mkString(" ")
          declarations.addOne(
            s"(declare-datatypes (($name 0)) (($constructors)))"
          )
        }
        case _ =>
      }
    }
    declarations.mkString("\n")
  }

  def getConstructorSMTString(term: Program, position: Int): String =
    term.stmts(position) match {
      case Constructor(n, _, params) => {
        val selectors =
          params.map(p => getSelectorSMTString(term, p.loc)).mkString(" ")
        s"(${n} $selectors)"
      }
      case _ => throw new IllegalArgumentException("Must be a constructor")
    }

  def getSelectorSMTString(term: Program, position: Int): String =
    term.stmts(position) match {
      case Selector(n, s) => {
        val sortProgram = getSmtSortName(term, s.loc)
        s"($n $sortProgram)"
      }
      case _ => throw new IllegalArgumentException("Must be a selector")
    }

  def getFunctionParameterDeclaration(term: Program, position: Int): String =
    term.stmts(position) match {
      case FunctionParameter(n, s) => {
        val sortProgram = getSmtSortName(term, s.loc)
        s"($n $sortProgram)"
      }
      case _ =>
        throw new IllegalArgumentException("Must be a function parameter")
    }

  def getSmtTermString(term: Program): String = getSmtTermString_i(term, 0)

  def getSmtTermString_i(term: Program, position: Int): String =
    term.stmts(position) match {
      case Ref(i)                  => getSmtTermString_i(term, i)
      case Numeral(j)              => j.toString()
      case TheorySort(n, _)        => n
      case SortMacro(n, _)         => n
      case SortParameter(n)        => n
      case UserSort(n, _)          => n
      case FunctionParameter(n, _) => n
      case TheoryMacro(n, ps) =>
        if (ps.length == 0) n
        else
          s"(${n} ${ps.map(p => getFunctionParameterDeclaration(term, p.loc)).mkString(" ")})"
      case UserMacro(n, _, _, _)      => n
      case UserFunction(n, _, _)      => n
      case Constructor(n, _, _)       => n
      case Selector(n, _)             => n
      case DataType(n, _)             => n
      case core.Module(_, _, i, _, _) => getSmtTermString_i(term, i.loc)
      case Application(caller, args) => {
        if (args.length > 0) {
          s"(${(List(caller) ++ args).map(r => getSmtTermString_i(term, r.loc)).mkString(" ")})"
        } else {
          getSmtTermString_i(term, caller.loc)
        }
      }
    }

  def getSmtSortName(term: Program, position: Int): String =
    term.stmts(position) match {
      case Ref(i) => getSmtSortName(term, i)
      case Numeral(j) =>
        throw new IllegalArgumentException(s"must be a sort; not a numeral")
      case TheorySort(n, ps) =>
        if (ps.length == 0) n
        else
          s"(${n} ${ps.map(p => getSmtTermString_i(term, p.loc)).mkString(" ")})"
      case SortMacro(n, _)  => n
      case SortParameter(n) => n
      case UserSort(n, _)   => n
      case FunctionParameter(_, s) =>
        throw new IllegalArgumentException(s"must be a sort; no inference")
      case TheoryMacro(n, s) =>
        throw new IllegalArgumentException(s"must be a sort; no inference")
      case UserMacro(_, s, _, _) =>
        throw new IllegalArgumentException(s"must be a sort; no inference")
      case UserFunction(_, s, _) =>
        throw new IllegalArgumentException(s"must be a sort; no inference")
      case Constructor(n, s, _)       => getSmtSortName(term, s.loc)
      case Selector(n, s)             => getSmtSortName(term, s.loc)
      case DataType(n, _)             => n
      case core.Module(_, t, _, _, _) => getSmtSortName(term, t.loc)
      case Application(caller, args) => {
        if (args.length > 0) {
          s"(${(List(caller) ++ args).map(r => getSmtSortName(term, r.loc)).mkString(" ")})"
        } else {
          getSmtSortName(term, caller.loc)
        }
      }
    }

  def toSmtString(term: Program): String = {
    val ctx = getSmtCtxString(term)
    val str = getSmtTermString_i(term, 0)
    if (ctx != "") {
      ctx ++ "\n" ++ str
    } else {
      str
    }
  }
}
