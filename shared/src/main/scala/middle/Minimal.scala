package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Minimal(stmts: ArrayBuffer[Instruction]) extends WellFormed(stmts) {

  val assertionRefs: ListBuffer[Int] = new ListBuffer()
  val axiomRefs: ListBuffer[Int] = new ListBuffer()

  private val memo: HashMap[Instruction, Int] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, p._2)))

  /** Add instruction to stmts if it isn't already there, otherwise return its location.
    *
    * If the instruction should be named, then add a reference to it and point to that.
    *
    * @param inst instruction to add
    * @param toName the Option[String] to name the instruction
    * @return the location of the instruction
    */
  protected def memoAddInstruction(
    inst: Instruction,
    toName: Option[String] = None
  ): Int = {
    val location = memo.get(inst) match {
      case Some(loc) => loc
      case None => {
        stmts.addOne(inst)
        val newLoc = stmts.length - 1
        memo(inst) = newLoc
        newLoc
      }
    }
    toName match {
      case Some(name) => {
        stmts.addOne(Ref(location, name))
        val newLoc = stmts.length - 1
        memo(inst) = newLoc
        newLoc
      }
      case None => location
    }
  }

  protected def addInstruction(inst: Instruction): Int = {
    stmts.addOne(inst)
    stmts.length - 1
  }

  protected def memoUpdateInstruction(
    r: Int,
    newInstruction: Instruction
  ): Unit = {
    val old = stmts(r)
    memo(newInstruction) = memo.getOrElse(old, r)
    memo.remove(old)
    stmts.update(r, newInstruction)
  }

  protected def memoGetInstruction(inst: Instruction): Int =
    memo(inst)

  protected def mark(startingPoints: Iterable[Int]): Array[Boolean] = {
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

  /*
  Copy an application term by inserting new copies of all instructions and returning a map describing the copy
   */
  protected def copyTerm(pos: Int): Int = {
    assert(
      stmts(pos).isInstanceOf[Application],
      s"should be an application but is ${stmts(pos)}"
    )
    val map = copyTermHelper(pos)
    updateTerm(map(pos), map)
    map(pos)
  }

  /*
  Copies the term but instead of updating references, returns a map describing the changes
   */
  private def copyTermHelper(
    pos: Int,
    map: HashMap[Int, Int] = HashMap.empty
  ): HashMap[Int, Int] = {
    stmts(pos) match {
      case Application(caller, args) => {
        val newCaller = copyTermHelper(caller, map).getOrElse(caller, caller)
        val newArgs = args.map(a => copyTermHelper(a, map).getOrElse(a, a))
        val newPos: Int = addInstruction(Application(newCaller, newArgs))
        map.addOne((pos, newPos))
      }
      case _ =>
    }
    map
  }

  /*
  Updates the references in an application using the map
   */
  protected def updateTerm(pos: Int, map: HashMap[Int, Int]): Unit =
    stmts(pos) match {
      case Application(caller, args) => {
        memoUpdateInstruction(
          pos,
          Application(
            map.getOrElse(caller, caller),
            args.map(a => map.getOrElse(a, a))
          )
        )
        updateTerm(caller, map)
        args.foreach(a => updateTerm(a, map))
      }
      case _ =>
    }
}
