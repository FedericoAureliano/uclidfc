package middle

import scala.collection.mutable.ArrayBuffer

/*
Passing semantic checks must gurantee a well formed query
 */

class WellFormed(stmts: ArrayBuffer[Instruction]) extends TermGraph(stmts) {

  def checkRefBounds(): Unit = {
    def checkRef(r: Ref): Boolean = r.loc < stmts.length && r.loc >= 0

    stmts.zipWithIndex.foreach {
      case (s, i) => {
        val err = s"${i}: ${s} >>> REFERENCE OUT OF BOUNDS!"
        s match {
          case Ref(j) =>
            if (!checkRef(Ref(j))) throw new RefOutOfBoundsError(err)
          case Numeral(_) =>
          case TheorySort(_, p) =>
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          case UserSort(_, _) =>
          case FunctionParameter(_, sr) =>
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          case TheoryMacro(_, p) =>
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          case UserMacro(_, sr, b, p) => {
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
            if (!checkRef(b)) throw new RefOutOfBoundsError(err)
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          }
          case UserFunction(_, sr, p) => {
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          }
          case Synthesis(_, sr, p) => {
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          }
          case Constructor(_, sr, p) => {
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          }
          case Selector(_, sr) =>
            if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          case DataType(_, p) =>
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          case Module(_, d, in, x, v) => {
            if (!checkRef(d)) throw new RefOutOfBoundsError(err)
            if (!checkRef(in)) throw new RefOutOfBoundsError(err)
            if (!checkRef(x)) throw new RefOutOfBoundsError(err)
            if (!checkRef(v)) throw new RefOutOfBoundsError(err)
          }
          case Application(caller, p) => {
            if (!checkRef(caller)) throw new RefOutOfBoundsError(err)
            p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
          }
        }
      }
    }

    None
  }

  def inferTermType(
    app: Ref
  ): Ref =
    stmts(app.loc) match {
      case Application(caller, args) => {
        stmts(caller.loc) match {
          case TheoryMacro("ite", _) => inferTermType(args.head)
          case TheoryMacro("store", _) =>
            inferTermType(args.head)
          case TheoryMacro("select", _) => {
            val arrayRef = inferTermType(args.head)
            val arraySort =
              stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => inferTermType(caller)
        }
      }
      case Constructor(_, sort, _)    => sort
      case FunctionParameter(_, sort) => sort
      case Selector(_, sort)          => sort
      case _ =>
        throw new IllegalArgumentException(
          s"type inference not yet supported: ${stmts(app.loc)}"
        )
    }
}
