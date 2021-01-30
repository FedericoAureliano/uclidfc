package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

abstract class AbstractTermGraph() {
  /**
    * The actual graph data structure.
    */
  val stmts: ArrayBuffer[Instruction] = new ArrayBuffer[Instruction]()

  /**
    * Points an instruction to its address in the term graph.
    */
  private val memo: HashMap[Instruction, Int] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, p._2)))

  private var uniqueId = 0

  def freshSymbolName(): String = {
    uniqueId += 1
    s"fresh!${uniqueId}"
  }

  def clear() : Unit = {
    stmts.clear()
    memo.clear()
    uniqueId = 0
  }


  def getStmtsSize(): Int = stmts.length
  def getMemoKeySize(): Int = memo.keys.toList.length
  def getMemoValueSize(): Int = memo.values.toList.length

  /** Add instruction to stmts if it isn't already there, otherwise return its location.
    *
    * If the instruction should be named, then add a reference to it and point to that.
    *
    * @param inst instruction to add
    * @param toName the Option[String] to name the instruction
    * @return the location of the instruction
    */
  def memoAddInstruction(
    inst: Instruction,
    toName: Option[String] = None
  ): Int =
    inst match {
      case FunctionParameter(_, _) => {
        // We never want to memoize variables so that we avoid accidental variable capture.
        // This would happen a lot in rewrites where updating a variable in some function could mess up some other function.
        addInstruction(inst)
      }
      case _ => {
        val location = memo.get(inst) match {
          case Some(loc) => loc
          case None      => addInstruction(inst)
        }
        toName match {
          case Some(name) => {
            val newLoc = addInstruction(Ref(location, name))
            memo.put(inst, newLoc)
            newLoc
          }
          case None => {
            memo.put(inst, location)
            location
          }
        }
      }
    }

  /** Add an instruction to the end of the array buffer representing the term graph WITHOUT ADDING TO MEMO.
    *
    * @param inst instruction to add
    * @return address where the location was added
    */
  def addInstruction(inst: Instruction): Int = {
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
    newInstruction: Instruction
  ): Unit = {
    // require that at most one instruction points to this address
    require(memo.forall(p => p._2 != r || stmts(r) == p._1))

    val old = stmts(r)
    memo.remove(old)

    // update the address r to contain the new instruction
    stmts.update(
      r,
      newInstruction
    )
    // make sure the memo map points to this position
    memo.put(
      newInstruction,
      r
    )
  }

  def memoGetInstruction(inst: Instruction): Int =
    memo(inst)

  def mark(startingPoints: Iterable[Int]): Array[Boolean] = {
    val marks = Array.fill[Boolean](stmts.length)(false)
    startingPoints.foreach(r => mark_i(r, marks))
    marks
  }

  private def mark_i(position: Int, marks: Array[Boolean]): Unit = {
    def markParams(params: List[Int]) =
      params.foreach(p => markInstruction(p))

    def markInstruction(pos: Int): Unit =
      if (!marks(pos)) {
        marks(pos) = true
        stmts(pos) match {
          case Ref(i, _)               => markInstruction(i)
          case Numeral(_)              =>
          case TheorySort(_, p)        => markParams(p)
          case UserSort(_, _)          =>
          case FunctionParameter(_, s) => markInstruction(s)
          case TheoryMacro(_, p)       => markParams(p)
          case UserMacro(_, s, b, p) =>
            markInstruction(s); markInstruction(b); markParams(p)
          case UserFunction(_, s, p) => markInstruction(s); markParams(p)
          case Synthesis(_, s, p)    => markInstruction(s); markParams(p)
          case Constructor(_, _, p)  => markParams(p)
          case Selector(_, s)        => markInstruction(s)
          case DataType(_, p)        => markParams(p)
          case Module(_, d, i, x, v) =>
            markInstruction(i); markInstruction(d); markInstruction(x);
            markInstruction(v)
          case Application(caller, args) =>
            markInstruction(caller); args.foreach(i => markInstruction(i))
        }
      }

    markInstruction(position)
  }
}

class TermGraph extends AbstractTermGraph with Fuzzable with Rewritable with WellFormed