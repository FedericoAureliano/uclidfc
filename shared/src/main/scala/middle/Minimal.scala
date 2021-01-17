package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

class Minimal(stmts: ArrayBuffer[Instruction]) extends WellFormed(stmts) {

  val assertionRefs: ListBuffer[Ref] = new ListBuffer()
  val axiomRefs: ListBuffer[Ref] = new ListBuffer()

  val memo: HashMap[Instruction, Ref] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, Ref(p._2, None))))

  def memoAddInstruction(
    inst: Instruction,
    toName: Option[String] = None
  ): Ref =
    // we want to name what ever reference we get back, unless it already has a name
    memo.get(inst) match {
      case Some(value) => {
        value.named match {
          case Some(_) => value
          case None => {
            // it is not already named so name it
            val newLoc = Ref(stmts.length, toName)
            stmts.addOne(inst)
            memo.addOne((inst, newLoc))
            newLoc
          }
        }
      }
      case None => {
        // it is not already saved, so name it
        val newLoc = Ref(stmts.length, toName)
        stmts.addOne(inst)
        memo.addOne((inst, newLoc))
        newLoc
      }
    }

  def addInstruction(inst: Instruction): Ref = {
    stmts.addOne(inst)
    Ref(stmts.length - 1, None)
  }

  def memoUpdateInstruction(r: Ref, newInstruction: Instruction): Unit = {
    val old = stmts(r.loc)
    stmts.update(r.loc, newInstruction)
    memo.remove(old)
    memo(newInstruction) = r
  }

  def mark(startingPoints: Iterable[Ref]): Array[Boolean] = {
    val marks = Array.fill[Boolean](stmts.length)(false)
    startingPoints.foreach(r => mark_i(r, marks))
    marks
  }

  def mark_i(position: Ref, marks: Array[Boolean]): Unit = {
    def markParams(params: List[Ref]) =
      params.foreach { p =>
        if (!marks(p.loc)) {
          marks(p.loc) = true
          markInstruction(stmts(p.loc))
        }
      }

    def markInstruction(instruction: Instruction): Unit =
      instruction match {
        case Ref(i, _) => {
          if (!marks(i)) {
            marks(i) = true
            markInstruction(stmts(i))
          }
        }
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

    marks(position.loc) = true
    markInstruction(stmts(position.loc))
  }

  /*
  Copy an application term by inserting new copies of all instructions and returning a map describing the copy
   */
  def copyTerm(pos: Ref): Ref = {
    assert(
      stmts(pos.loc).isInstanceOf[Application],
      s"should be an application but is ${stmts(pos.loc)}"
    )
    val map = copyTermHelper(pos)
    updateTerm(map(pos), map)
    map(pos)
  }

  /*
  Copies the term but instead of updating references, returns a map describing the changes
   */
  def copyTermHelper(
    pos: Ref,
    map: HashMap[Ref, Ref] = HashMap.empty
  ): HashMap[Ref, Ref] = {
    stmts(pos.loc) match {
      case Application(caller, args) => {
        val newCaller = copyTermHelper(caller, map).getOrElse(caller, caller)
        val newArgs = args.map(a => copyTermHelper(a, map).getOrElse(a, a))
        val newPos: Ref = addInstruction(Application(newCaller, newArgs))
        map.addOne((pos, newPos))
      }
      case _ =>
    }
    map
  }

  /*
  Updates the references in an application using the map
   */
  def updateTerm(pos: Ref, map: HashMap[Ref, Ref]): Unit =
    stmts(pos.loc) match {
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
