package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

// Essentially an AST node or edge.
abstract class Instruction

// Points to another position in the array
case class Ref(loc: Int) extends Instruction {
  override def toString(): String = s"#$loc"
}

case class Numeral(value: Int) extends Instruction {
  /*
  Numerals are used for e.g. fixed-width bit-vectors
   */
  override def toString(): String = s"[num]\t$value"
}

case class TheorySort(name: String, params: List[Ref] = List.empty)
    extends Instruction {
  /*
  Theory sorts are interpreted sorts like "Int." For example, 32-bit bit-vectors are represented by

  [tst] BitVec  #1
  [num] 32

  The params can point to other sorts or numerals.
   */
  override def toString(): String = s"[tst]\t$name\t${params.mkString("\t")}"
}

case class UserSort(name: String, arity: Numeral = Numeral(0))
    extends Instruction {
  /*
  User sorts are uninterpreted sorts
   */
  override def toString(): String = s"[ust]\t$name\t$arity"
}

case class FunctionParameter(name: String, sort: Ref) extends Instruction {
  /*
  Function parameters, must be arity 0
   */
  override def toString(): String = s"[fpr]\t$name\t$sort"
}

case class TheoryMacro(name: String, params: List[Ref] = List.empty)
    extends Instruction {
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
  override def toString(): String = s"[tmo]\t$name\t${params.mkString("\t")}"
}

case class UserMacro(
  name: String,
  sort: Ref,
  body: Ref,
  params: List[Ref] = List.empty
) extends Instruction {

  /*
  User macros are function definitions like f(x) = x + x, which would be in LIR

  [umo] f   #4    #1    #3
  [app] #2  #3    #3
  [tmo] +
  [fpr] x   #4
  [tst] Int
   */
  override def toString(): String =
    s"[umo]\t$name\t$sort\t$body\t${params.mkString("\t")}"
}

case class UserFunction(name: String, sort: Ref, params: List[Ref] = List.empty)
    extends Instruction {

  /*
  User functions are uninterpreted functions. For example, f(x : Int) : Int would be

  [umo] f   #2    #1
  [fpr] x   #2
  [tst] Int
   */
  override def toString(): String =
    s"[ufn]\t$name\t$sort\t${params.mkString("\t")}"
}

case class Constructor(
  name: String,
  sort: Ref,
  selectors: List[Ref] = List.empty
) extends Instruction {

  override def toString(): String =
    s"[ctr]\t$name\t$sort\t${selectors.mkString("\t")}"
}

case class Selector(name: String, sort: Ref) extends Instruction {
  override def toString(): String = s"[slr]\t$name\t$sort"
}

case class DataType(name: String, constructors: List[Ref]) extends Instruction {

  /*
  For algebraic datatypes like enums, records, and so on. For example a record R = {x: Int, y: Real} is

  [adt] R     #1
  [ctr] r     #0    #2    #3
  [slr] x     #4
  [slr] y     #5
  [tst] Int
  [tst] Real
   */
  override def toString(): String =
    s"[adt]\t$name\t${constructors.mkString("\t")}"
}

// ct is a constructor, so Module is often just treated like a datatype
case class Module(name: String, ct: Ref, init: Ref, next: Ref, spec: Ref)
    extends Instruction {
  /*
  A module is a record with associated init function, next function, and spec function.
   */
  override def toString(): String = s"[mod]\t$name\t$ct\t$init\t$next\t$spec"
}

case class Application(caller: Ref, args: List[Ref]) extends Instruction {

  override def toString(): String =
    s"[app]\t${(List(caller) ++ args).mkString("\t")}"
}

class Program(val stmts: ArrayBuffer[Instruction]) {
  var isSynthesisQuery = false
  val cache = new CacheStack()
  pushCache()

  val assertions: ListBuffer[Ref] = new ListBuffer()
  var options: List[(String, String)] = List.empty

  override def toString(): String = stmts.mkString("\n")

  var uniqueId = 0

  def pushCache(): Unit = {
    cache.sortCache.push(new HashMap[String, Ref]())
    cache.objectCache.push(new HashMap[String, Ref]())
  }

  def popCache(): Unit = {
    cache.sortCache.pop()
    cache.objectCache.pop()
  }

  def freshSymbolName(): String = {
    uniqueId += 1
    s"nd!${uniqueId}"
  }

  def saveSortRef(name: String, sort: Ref): Unit =
    cache.sortCache.top.addOne((name, sort))

  def loadSortRef(name: String): Option[Ref] = {
    cache.sortCache.foreach { cache =>
      cache.get(name) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def loadOrSaveSortRef(name: String, sort: => Ref): Ref =
    loadSortRef(name) match {
      case Some(value) => value
      case None        => val r = sort; saveSortRef(name, r); r
    }

  def saveObjectRef(name: String, obj: Ref): Unit =
    cache.objectCache.top.addOne((name, obj))

  def loadObjectRef(name: String): Option[Ref] = {
    cache.objectCache.foreach { cache =>
      cache.get(name) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def loadOrSaveObjectRef(name: String, obj: => Ref): Ref =
    loadObjectRef(name) match {
      case Some(value) => value
      case None        => val r = obj; saveObjectRef(name, r); r
    }

  def inferLogic(): String = "ALL"
}

class CacheStack() {
  // point type name to type location (modules are types)
  val sortCache: Stack[HashMap[String, Ref]] = new Stack[HashMap[String, Ref]]()

  // point Object name to name location
  val objectCache: Stack[HashMap[String, Ref]] =
    new Stack[HashMap[String, Ref]]()
}
