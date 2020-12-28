package middle

/*
Passing semantic checks must gurantee a well formed smt-lib query
(except for functions to synthesize and procedure calls)
 */

object Checker {

  def checkRefBounds(term: Program): Option[String] = {
    def checkRef(r: Ref): Boolean = r.loc < term.stmts.length && r.loc >= 0

    term.stmts.zipWithIndex.foreach {
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
    program: Program,
    app: Ref
  ): Ref =
    program.stmts(app.loc) match {
      case Application(caller, args) => {
        program.stmts(caller.loc) match {
          case TheoryMacro("ite", _) => inferTermType(program, args.head)
          case TheoryMacro("store", _) =>
            inferTermType(program, args.head)
          case TheoryMacro("select", _) => {
            val arrayRef = inferTermType(program, args.head)
            val arraySort =
              program.stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => inferTermType(program, caller)
        }
      }
      case Constructor(_, sort, _)    => sort
      case FunctionParameter(_, sort) => sort
      case Selector(_, sort)          => sort
      case _ =>
        throw new IllegalArgumentException(
          s"type inference not yet supported: ${program.stmts(app.loc)}"
        )
    }
}
