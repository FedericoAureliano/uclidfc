package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import front.Type
import front.TermNode
import front.ConjunctionOp
import front.DisjunctionOp

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

case class Application(caller: Ref, args: List[Ref]) extends Instruction {}

class Program(val stmts: ArrayBuffer[Instruction]) {
  var isSynthesisQuery = false
  val cache = new CacheStack()
  pushCache()

  val assertions: ListBuffer[Ref] = new ListBuffer()
  // after generating assertions, we will traverse to finds calls of ._1 and add assertions that call ._2 with the same arguments
  val inlineAssumes: HashMap[Ref, Ref] = new HashMap()
  val inlineAsserts: HashMap[Ref, Ref] = new HashMap()
  var options: List[(String, String)] = List.empty

  def addAssertion(ass: Ref): Unit = {
    val assumes = new ListBuffer[Ref]()
    val asserts = new ListBuffer[Ref]()

    def updateTerm(position: Ref, updates: Map[Ref, Ref]): Ref = {
      val newPos = stmts(position.loc) match {
        case Application(caller, args) => {
          val newArgs =
            args.map(a => updateTerm(a, updates))
          if (newArgs != args) {
            stmts.addOne(Application(caller, newArgs))
            Ref(stmts.length - 1)
          } else {
            position
          }
        }
        case a: Ref => updates.getOrElse(a, a)
        case _      => updates.getOrElse(position, position)
      }
      newPos
    }

    def searchInline(
      curr: Ref,
      inlines: HashMap[Ref, Ref],
      keepTrack: ListBuffer[Ref],
      updates: Map[Ref, Ref]
    ): Unit =
      stmts(curr.loc) match {
        case Application(caller, args) => {
          stmts(caller.loc) match {
            case TheoryMacro(_, _) | Constructor(_, _, _) | Selector(_, _) |
                UserFunction(_, _, _) => {
              // just continue search in children
              args.foreach(p => searchInline(p, inlines, keepTrack, updates))
            }
            case UserMacro(_, _, body, params) => {
              val newArgs = args.map(a => updateTerm(a, updates))
              if (inlines.contains(caller)) {
                // we found a an assume or assert!
                val newAppRef = Ref(stmts.length)
                stmts.addOne(Application(inlines(caller), newArgs))
                keepTrack.addOne(newAppRef)
              } else {
                // keep searching in children
                args.foreach(p => searchInline(p, inlines, keepTrack, updates))
                // update bindings
                val bindings = params.zip(newArgs).toMap
                // recurse search into body
                searchInline(body, inlines, keepTrack, bindings)
              }
            }
            case _ => // do nothing
          }
        }
        case _ => // do nothing
      }
    searchInline(ass, inlineAssumes, assumes, Map.empty)
    searchInline(ass, inlineAsserts, asserts, Map.empty)

    val inner = if (asserts.length > 0) {
      val orRef = loadOrSaveObjectRef(DisjunctionOp(), {
        stmts.addOne(TheoryMacro("or"));
        Ref(stmts.length - 1)
      })

      val appRef = Ref(stmts.length)
      stmts.addOne(Application(orRef, List(ass) ++ asserts.toList))

      appRef
    } else {
      ass
    }

    val outer = if (assumes.length > 0) {
      val andRef = loadOrSaveObjectRef(ConjunctionOp(), {
        stmts.addOne(TheoryMacro("and"));
        Ref(stmts.length - 1)
      })

      val appRef = Ref(stmts.length)
      stmts.addOne(Application(andRef, List(inner) ++ assumes.toList))

      appRef
    } else {
      inner
    }

    assertions.addOne(outer)
  }

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
