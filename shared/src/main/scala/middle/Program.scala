package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import front.Type
import front.TermNode

// Essentially an AST node or edge.
abstract class Instruction

abstract class AbstractDataType extends Instruction {
  def defaultCtr(): Ref
}

// Points to another position in the array
case class Ref(loc: Int) extends Instruction {}

/*
Numerals are used for e.g. fixed-width bit-vectors
 */
case class Numeral(value: Int) extends Instruction {}

/*
Theory sorts are interpreted sorts like "Int." For example, 32-bit bit-vectors are represented by

[tst] BitVec  #1
[num] 32

The params can point to other sorts or numerals.
 */
case class TheorySort(name: String, params: List[Ref] = List.empty)
    extends Instruction {}

/*
User sorts are uninterpreted sorts
 */
case class UserSort(name: String, arity: Numeral = Numeral(0))
    extends Instruction {}

/*
Function parameters, must be arity 0
 */
case class FunctionParameter(name: String, sort: Ref) extends Instruction {}

/*
Theory macros are interpreted functions, like "+", "1", and "#b11110000".
Quantifiers are special in that they bind parameters. For example, (forall ((x Int)) (> 0 x)) is

[app] #4      #1
[app] #2      #3    #5
[tmo] >
[tmo] 0
[tmo] forall  #5
[fpr] x       #6
[tst] Int
 */
case class TheoryMacro(name: String, params: List[Ref] = List.empty)
    extends Instruction {}

/*
User macros are function definitions like f(x) = x + x, which would be in LIR

[umo] f   #4    #1    #3
[app] #2  #3    #3
[tmo] +
[fpr] x   #4
[tst] Int
 */
case class UserMacro(
  name: String,
  sort: Ref,
  body: Ref,
  params: List[Ref] = List.empty
) extends Instruction {}

/*
User functions are uninterpreted functions. For example, f(x : Int) : Int would be

[umo] f   #2    #1
[fpr] x   #2
[tst] Int
 */
case class UserFunction(name: String, sort: Ref, params: List[Ref] = List.empty)
    extends Instruction {}

case class Constructor(
  name: String,
  sort: Ref,
  selectors: List[Ref] = List.empty
) extends Instruction {}

case class Selector(name: String, sort: Ref) extends Instruction {}

/*
For algebraic datatypes like enums, records, and so on. For example a record R = {x: Int, y: Real} is

[adt] R     #1
[ctr] r     #0    #2    #3
[slr] x     #4
[slr] y     #5
[tst] Int
[tst] Real
 */
case class DataType(name: String, constructors: List[Ref])
    extends AbstractDataType {
  override def defaultCtr(): Ref = constructors.head
}

/*
A module is a record with associated init function, next function, and spec function.
 */
case class Module(name: String, ct: Ref, init: Ref, next: Ref, spec: Ref)
    extends AbstractDataType {
  override def defaultCtr(): Ref = ct
}

case class Application(caller: Ref, args: List[Ref]) extends Instruction {
  s"[app]\t${(List(caller) ++ args).mkString("\t")}"
}

class Program(val stmts: ArrayBuffer[Instruction]) {
  var isSynthesisQuery = false
  val cache = new CacheStack()
  pushCache()

  val assertions: ListBuffer[Ref] = new ListBuffer()
  var options: List[(String, String)] = List.empty

  def pushCache(): Unit = {
    cache.sortCache.push(new HashMap[Type, Ref]())
    cache.objectCache.push(new HashMap[TermNode, Ref]())
  }

  def popCache(): Unit = {
    cache.sortCache.pop()
    cache.objectCache.pop()
    // don't pop auxParams, these need to accumulate
  }

  var uniqueId = 0

  def freshSymbolName(): String = {
    uniqueId += 1
    s"nondet!${uniqueId}"
  }

  def saveSortRef(typ: Type, sort: Ref): Unit =
    cache.sortCache.top.addOne((typ, sort))

  def loadSortRef(typ: Type): Option[Ref] = {
    cache.sortCache.foreach { cache =>
      cache.get(typ) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def loadOrSaveSortRef(typ: Type, sort: => Ref): Ref =
    loadSortRef(typ) match {
      case Some(value) => value
      case None        => val r = sort; saveSortRef(typ, r); r
    }

  def saveObjectRef(term: TermNode, obj: Ref): Unit =
    cache.objectCache.top.addOne((term, obj))

  def loadObjectRef(term: TermNode): Option[Ref] = {
    cache.objectCache.foreach { cache =>
      cache.get(term) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def loadOrSaveObjectRef(term: TermNode, obj: => Ref): Ref =
    loadObjectRef(term) match {
      case Some(value) => value
      case None        => val r = obj; saveObjectRef(term, r); r
    }

  def inferLogic(): String = "ALL"
}

class CacheStack() {
  // point type to program location (modules are types)
  val sortCache: Stack[HashMap[Type, Ref]] = new Stack[HashMap[Type, Ref]]()

  // point expr to program location
  val objectCache: Stack[HashMap[TermNode, Ref]] =
    new Stack[HashMap[TermNode, Ref]]()
}
