package middle

import scala.collection.mutable.ArrayBuffer

/*
Passing semantic checks must gurantee a well formed query
 */

class WellFormed(stmts: ArrayBuffer[Instruction]) extends TermGraph(stmts) {

  def checkRefBounds(): Option[String] = {
    def checkRef(r: Ref): Boolean = r.loc < stmts.length && r.loc >= 0

    stmts.zipWithIndex.foreach {
      case (s, i) => {
        val err = Some(s"${i}: ${s} >>> REFERENCE OUT OF BOUNDS!")
        s match {
          case Ref(j)                   => if (!checkRef(Ref(j))) return err
          case Numeral(_)               =>
          case TheorySort(_, p)         => p.foreach(a => if (!checkRef(a)) return err)
          case UserSort(_, _)           =>
          case FunctionParameter(_, sr) => if (!checkRef(sr)) return err
          case TheoryMacro(_, p)        => p.foreach(a => if (!checkRef(a)) return err)
          case UserMacro(_, sr, b, p) => {
            if (!checkRef(sr)) return err
            if (!checkRef(b)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case UserFunction(_, sr, p) => {
            if (!checkRef(sr)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case Constructor(_, sr, p) => {
            if (!checkRef(sr)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case Selector(_, sr) => if (!checkRef(sr)) return err
          case DataType(_, p)  => p.foreach(a => if (!checkRef(a)) return err)
          case Module(_, d, in, x, v) => {
            if (!checkRef(d)) return err
            if (!checkRef(in)) return err
            if (!checkRef(x)) return err
            if (!checkRef(v)) return err
          }
          case Application(caller, p) => {
            if (!checkRef(caller)) return err
            p.foreach(a => if (!checkRef(a)) return err)
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
