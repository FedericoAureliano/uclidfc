package middle

/*
Passing semantic checks must gurantee a well formed smt-lib query
(except for functions to synthesize and procedure calls)
 */

object Checker {

  def checkRefBounds(term: TermGraph): Option[String] = {
    def checkRef(r: Ref): Boolean = r.loc < term.stmts.length && r.loc >= 0

    term.stmts.zipWithIndex.foreach {
      case (s, i) => {
        val err = Some(s"${i}: ${s} >>> REFERENCE OUT OF BOUNDS!")
        s match {
          case Ref(j)                   => if (!checkRef(Ref(j))) return err
          case Numeral(_)               =>
          case TheorySort(n, p)         => p.foreach(a => if (!checkRef(a)) return err)
          case SortMacro(n, b)          => if (!checkRef(b)) return err
          case SortParameter(_)         =>
          case UserSort(n, a)           =>
          case FunctionParameter(n, sr) => if (!checkRef(sr)) return err
          case TheoryMacro(n, p)        => p.foreach(a => if (!checkRef(a)) return err)
          case UserMacro(n, sr, b, p) => {
            if (!checkRef(sr)) return err
            if (!checkRef(b)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case UserFunction(n, sr, p) => {
            if (!checkRef(sr)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case Constructor(n, sr, p) => {
            if (!checkRef(sr)) return err
            p.foreach(a => if (!checkRef(a)) return err)
          }
          case Selector(n, sr) => if (!checkRef(sr)) return err
          case DataType(n, p)  => p.foreach(a => if (!checkRef(a)) return err)
          case Module(n, d, in, x, v) => {
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

  def inferSort(term: TermGraph, position: Int): Instruction =
    term.stmts(position) match {
      case Ref(i) => inferSort(term, i)
      case Numeral(_) =>
        throw new IllegalArgumentException("numeral has no sort")
      case TheorySort(_, _) => Ref(position)
      case SortMacro(_, _)  => Ref(position)
      case SortParameter(_) =>
        throw new IllegalArgumentException("must be a complete sort")
      case UserSort(_, s)          => Ref(position)
      case FunctionParameter(_, s) => inferSort(term, s.loc)
      case TheoryMacro(n, args) => {
        n match {
          case "true"     => TheorySort("Bool")
          case "false"    => TheorySort("Bool")
          case "not"      => TheorySort("Bool")
          case "=>"       => TheorySort("Bool")
          case "and"      => TheorySort("Bool")
          case "or"       => TheorySort("Bool")
          case "xor"      => TheorySort("Bool")
          case "="        => TheorySort("Bool")
          case "distinct" => TheorySort("Bool")

          case "-"   => TheorySort("Int")
          case "+"   => TheorySort("Int")
          case "*"   => TheorySort("Int")
          case "div" => TheorySort("Int")
          case "mod" => TheorySort("Int")
          case "abs" => TheorySort("Int")

          case "<=" => TheorySort("Bool")
          case "<"  => TheorySort("Bool")
          case ">=" => TheorySort("Bool")
          case ">"  => TheorySort("Bool")

          case "ite" => inferSort(term, args(0).loc)

          case "store" => inferSort(term, args(0).loc)
          case "select" => {
            // get the array type and then find what the array contains
            inferSort(term, args(0).loc) match {
              case Ref(loc) => {
                term.stmts(loc) match {
                  case TheorySort("Array", List(b, c)) => c
                  case _ =>
                    throw new IllegalArgumentException(
                      s"must be an array ${term.stmts(loc)}"
                    )
                }
              }
              case _ =>
                throw new IllegalArgumentException(
                  s"must be an array ${args(0)}"
                )
            }
          }

          case _ =>
            throw new IllegalArgumentException(
              s"unsupported theory macro: ${n}"
            )
        }
      }
      case UserMacro(_, s, _, _)     => inferSort(term, s.loc)
      case UserFunction(_, s, _)     => inferSort(term, s.loc)
      case Constructor(_, s, _)      => inferSort(term, s.loc)
      case Selector(n, s)            => inferSort(term, s.loc)
      case DataType(n, _)            => Ref(position)
      case Module(n, _, _, _, _)     => Ref(position)
      case Application(caller, args) => inferSort(term, caller.loc)
    }
}
