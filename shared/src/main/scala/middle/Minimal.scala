package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class Minimal(stmts: ArrayBuffer[Instruction]) extends WellFormed(stmts) {

  val memo: HashMap[Instruction, Ref] =
    new HashMap().addAll(stmts.zipWithIndex.map(p => (p._1, Ref(p._2))))

  def memoAddInstruction(inst: Instruction): Ref =
    memo.getOrElse(inst, {
      val newLoc = Ref(stmts.length)
      stmts.addOne(inst)
      memo(inst) = newLoc
      newLoc
    })

  def addInstruction(inst: Instruction): Ref = {
    stmts.addOne(inst)
    Ref(stmts.length - 1)
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
        case Ref(i) => {
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
}
