package com.uclid.termgraph

import scala.collection.mutable.{ArrayBuffer, HashMap, Queue}
import scala.util.Random

object Util {
  val random = new Random
  private var uniqueId = 0
  def freshSymbolName(): String = {
    uniqueId += 1
    s"fresh!${uniqueId}"
  }
}


abstract class AbstractTermGraph() {

  /** The actual graph data structure.
    */
  private val stmts: ArrayBuffer[Instruction] = new ArrayBuffer[Instruction]()

  /** Points an instruction to its address in the term graph.
    */
  protected val memo: HashMap[Instruction, Int] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, p._2)))

  /** Add instruction to stmts if it isn't already there, otherwise return its location.
    *
    * If the instruction should be named, then add a reference to it and point to that.
    *
    * @param inst instruction to add
    * @param toName the Option[String] to name the instruction
    * @return the location of the instruction
    */
  def memoAddInstruction(inst: Instruction): Int = {
    inst match {
      case app : Application =>
        app.args.foreach(a => assert(stmts(a).isInstanceOf[Application] || stmts(a).isInstanceOf[Ref]))
        assert(!stmts(app.function).isInstanceOf[Application])
      case r : Ref => 
        assert(stmts(r.loc).isInstanceOf[Application] || stmts(r.loc).isInstanceOf[Ref])
      case _ =>
    }
    val location = memo.get(inst) match {
      case Some(loc) => loc
      case None      => addInstruction(inst)
    }
    memo.put(inst, location)
    location
  }

  /** Add an instruction to the end of the array buffer representing the term graph WITHOUT ADDING TO MEMO.
    *
    * @param inst instruction to add
    * @return address where the location was added
    */
  def addInstruction(inst: Instruction): Int = {
    inst match {
      case app : Application =>
        app.args.foreach(a => assert(stmts(a).isInstanceOf[Application] || stmts(a).isInstanceOf[Ref]))
        assert(!stmts(app.function).isInstanceOf[Application])
      case r : Ref => 
        assert(stmts(r.loc).isInstanceOf[Application] || stmts(r.loc).isInstanceOf[Ref])
      case _ =>
    }
    val r = stmts.length
    stmts.addOne(inst)
    r
  }

  /** Update an instruction and keep the memo map in sync.
    *
    * @param r location we want to update
    * @param newInstruction new instruction that we will put at that location
    */
  def memoUpdateInstruction(
    r: Int,
    newInstruction: Ref
  ): Unit = {
    // require that at most one instruction points to this address
    require(memo.forall(p => p._2 != r || stmts(r) == p._1))

    val old = stmts(r)
    memo.remove(old)

    // update the address r to contain the new instruction
    stmts(r) = newInstruction
  }

  def memoGetInstruction(inst: Instruction): Int =
    memo(inst)


  def terms(startingPoints: Iterable[Int]): Array[Boolean] = {
    val marks = Array.fill[Boolean](stmts.length)(false)
    startingPoints.foreach(r => terms_i(r, marks))
    marks
  }

  private def terms_i(position: Int, marks: Array[Boolean]): Unit = {
    // markInstruction(position)
    val frontier : Queue[Int] = Queue(position)
    while (!frontier.isEmpty) {
      val pos = frontier.dequeue()
      if (!marks(pos)) {
        marks(pos) = true
        stmts(pos) match {
          case Ref(i)               => frontier.addOne(i)
          case Application(function, args) =>
            frontier.addOne(function); args.foreach(i => frontier.addOne(i))
          // case UserMacro(_, _, b, _) => frontier.addOne(b)
          case _ =>
        }
      }
    }
  }

  def mark(startingPoints: Iterable[Int]): Array[Boolean] = {
    val marks = Array.fill[Boolean](stmts.length)(false)
    startingPoints.foreach(r => mark_i(r, marks))
    marks
  }

  private def mark_i(position: Int, marks: Array[Boolean]): Unit = {
    // markInstruction(position)
    val frontier : Queue[Int] = Queue(position)
    while (!frontier.isEmpty) {
      val pos = frontier.dequeue()
      if (!marks(pos)) {
        marks(pos) = true
        stmts(pos) match {
          case Ref(i)               => frontier.addOne(i)
          case Numeral(_)              =>
          case TheorySort(_, p)        => p.foreach(a => frontier.addOne(a))
          case UserSort(_, _)          =>
          case FunctionParameter(_, s) => frontier.addOne(s)
          case TheoryMacro(_, p)       => p.foreach(a => frontier.addOne(a))
          case UserMacro(_, s, b, p) =>
            frontier.addOne(s); frontier.addOne(b); p.foreach(a => frontier.addOne(a))
          case UserFunction(_, s, p) => frontier.addOne(s); p.foreach(a => frontier.addOne(a))
          case Synthesis(_, s, p)    => frontier.addOne(s); p.foreach(a => frontier.addOne(a))
          case Constructor(_, s, p)  => frontier.addOne(s); p.foreach(a => frontier.addOne(a))
          case Selector(_, s)        => frontier.addOne(s)
          case DataType(_, p)        => p.foreach(a => frontier.addOne(a))
          case Module(_, d, _, _, _) =>
            frontier.addOne(d)
          case Application(function, args) =>
            frontier.addOne(function); args.foreach(i => frontier.addOne(i))
        }
      }
    }
  }

  def completeButUnapplied(loc: Int): Boolean = {
    stmts(loc) match {
      case UserFunction(_, _, params) => params.length == 0
      case UserMacro(_, _, _, params) => params.length == 0
      case Synthesis(_, _, params) => params.length == 0
      case Constructor(_, _, params) => params.length == 0
      case Selector(_, _) => false
      case FunctionParameter(_, _) => true
      case TheoryMacro(name, _) => 
        // TODO support more than bools, strings, bitvecs, and ints
        name == "true" || name == "false" || (name.startsWith("\"") && name.endsWith("\"")) || name.startsWith("#") || name.startsWith("bv") || name.toIntOption.isDefined
      case _ => false
    }
  }

  def findTarget(in: Int) : Int = {
    var pos = in
    while (stmts(pos).isInstanceOf[Ref]) {
      pos = stmts(pos).asInstanceOf[Ref].loc
    }
    pos
  }

  def getStmts() : ArrayBuffer[Instruction] = stmts
  def getStmt(in: Int) : Instruction = stmts(findTarget(in))

  def repair() : Unit = {
    memo.clear()
    val newStmts: ArrayBuffer[Instruction] = stmts.clone()
    
    (0 to stmts.length - 1).foreach { p =>
      var pos = findTarget(p)

      val newInst = stmts(pos) match {
        case Ref(i) => Ref(findTarget(i))
        case TheorySort(n, p) => TheorySort(n, p.map(a => findTarget(a)))
        case FunctionParameter(n, s) => FunctionParameter(n, findTarget(s))
        case TheoryMacro(n, p) => TheoryMacro(n, p.map(a => findTarget(a)))
        case UserMacro(n, s, b, p) => UserMacro(n, findTarget(s), findTarget(b), p.map(a => findTarget(a)))
        case UserFunction(n, s, p) => UserFunction(n, findTarget(s), p.map(a => findTarget(a)))
        case Synthesis(n, s, p) => Synthesis(n, findTarget(s), p.map(a => findTarget(a)))
        case Constructor(n, s, p) => Constructor(n, findTarget(s), p.map(a => findTarget(a)))
        case Selector(n, s) => Selector(n, findTarget(s))
        case DataType(n, p) => DataType(n, p.map(a => findTarget(a)))
        case Module(n, d, i, x, s) => Module(n, findTarget(d), findTarget(i), findTarget(x), s.map(spec => findTarget(spec)))
        case Application(function, args) => Application(findTarget(function), args.map(a => findTarget(a)))
        case other => other
      }
      
      memo.get(newInst) match {
        case Some(otherPos) =>  newStmts(p) = Ref(otherPos)
        case None => {
          newStmts(p) = newInst
          memo.addOne(newInst, p)
        }
      }
    }

    stmts.clear()
    stmts.addAll(newStmts)
  }

  def isQuantifier(pos: Int) : Boolean = {
    getStmt(pos) match {
      case TheoryMacro("forall", _) => true
      case TheoryMacro("exists", _) => true
      case _ => false
    }
  }
}

class TermGraph
    extends AbstractTermGraph
    with Fuzzable
    with Rewritable
    with WellFormed
    with Probed
