package middle

object Collector {

  def mark(term: TermGraph): Array[Boolean] = {
    val marks = Array.fill[Boolean](term.stmts.length)(false)
    term.assertions.foreach(r => mark_i(term, r.loc, marks))
    marks
  }

  def mark_i(term: TermGraph, position: Int, marks: Array[Boolean]): Unit = {
    def markParams(params: List[Ref]) =
      params.foreach { p =>
        if (!marks(p.loc)) {
          marks(p.loc) = true
          markInstruction(term.stmts(p.loc))
        }
      }

    def markInstruction(instruction: Instruction): Unit =
      instruction match {
        case Ref(i) => {
          if (!marks(i)) {
            marks(i) = true
            markInstruction(term.stmts(i))
          }
        }
        case Numeral(_)              =>
        case TheorySort(_, p)        => markParams(p)
        case SortMacro(_, b)         => markInstruction(b)
        case SortParameter(_)        =>
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

    marks(position) = true
    markInstruction(term.stmts(position))
  }

  def sweep(term: TermGraph, marks: Array[Boolean]): TermGraph = {
    assert(
      term.stmts.length == marks.length,
      "term length must equal marks length"
    )

    val len = marks.length

    var newLocations = Array.fill[Int](len)(-1)

    var count = 0

    marks.zipWithIndex.foreach {
      case (b, i) => {
        if (!b) {
          count += 1
        } else {
          newLocations(i) = (i - count)
        }
      }
    }

    val newTerm = new TermGraph(
      term.stmts.clone().zipWithIndex.filter { case (i, p) => marks(p) }.map {
        case (i, p) => i
      }
    )

    Rewriter.updateRefs(newTerm, newLocations)

    newTerm
  }

  def collectGarbage(term: TermGraph): TermGraph = sweep(term, mark(term))
}
