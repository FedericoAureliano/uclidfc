package com.uclid.termgraph

import scala.collection.mutable.{ArrayBuffer, HashMap, Queue}

abstract class AbstractTermGraph() {

  /** The actual graph data structure.
    */
  val stmts: ArrayBuffer[Instruction] = new ArrayBuffer[Instruction]()

  /** Points an instruction to its address in the term graph.
    */
  protected val memo: HashMap[Instruction, Int] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, p._2)))

  var isSynthesisQuery = false

  private var uniqueId = 0

  def freshSymbolName(): String = {
    uniqueId += 1
    s"fresh!${uniqueId}"
  }

  def clear(): Unit = {
    stmts.clear()
    memo.clear()
    uniqueId = 0
  }

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
      case FunctionParameter(_, _) =>
        // We never want to memoize variables so that we avoid accidental variable capture.
        // This would happen a lot in rewrites where updating a variable in some function could mess up some other function.
        addInstruction(inst)
      case _ =>
        val location = memo.get(inst) match {
          case Some(loc) => loc
          case None      => addInstruction(inst)
        }
        toName match {
          case Some(name) =>
            val newLoc = addInstruction(Ref(location, name))
            memo.put(inst, newLoc)
            newLoc
          case None =>
            memo.put(inst, location)
            location
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
    // markInstruction(position)
    val frontier : Queue[Int] = Queue(position)
    while (!frontier.isEmpty) {
      val pos = frontier.dequeue()
      if (!marks(pos)) {
        marks(pos) = true
        stmts(pos) match {
          case Ref(i, _)               => frontier.addOne(i)
          case Numeral(_)              =>
          case TheorySort(_, p)        => p.foreach(a => frontier.addOne(a))
          case UserSort(_, _)          =>
          case FunctionParameter(_, s) => frontier.addOne(s)
          case TheoryMacro(_, p)       => p.foreach(a => frontier.addOne(a))
          case UserMacro(_, s, b, p) =>
            frontier.addOne(s); frontier.addOne(b); p.foreach(a => frontier.addOne(a))
          case UserFunction(_, s, p) => frontier.addOne(s); p.foreach(a => frontier.addOne(a))
          case Synthesis(_, s, p)    => frontier.addOne(s); p.foreach(a => frontier.addOne(a))
          case Constructor(_, _, p)  => p.foreach(a => frontier.addOne(a))
          case Selector(_, s)        => frontier.addOne(s)
          case DataType(_, p)        => p.foreach(a => frontier.addOne(a))
          case Module(_, d, i, x, v) =>
            frontier.addOne(i); frontier.addOne(d); frontier.addOne(x);
            frontier.addOne(v)
          case Application(caller, args) =>
            frontier.addOne(caller); args.foreach(i => frontier.addOne(i))
        }
      }
    }
  }
}

class TermGraph
    extends AbstractTermGraph
    with Fuzzable
    with Rewritable
    with WellFormed
    with Probed
