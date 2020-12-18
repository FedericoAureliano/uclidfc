package middle

import front._

import scala.collection.mutable.HashSet

object Printer {

  val TAB = "  "

  def programPointToQueryTerm(
    term: TermGraph,
    point: Ref,
    indentInput: Int = 0
  ): String = {
    var indent = indentInput

    def dispatch(position: Ref): String =
      term.stmts(position.loc) match {
        case a: Application       => applicationToQueryTerm(a)
        case c: Constructor       => constructorToQueryTerm(c)
        case d: DataType          => datatypeToQueryTerm(d)
        case f: FunctionParameter => functionparameterToQueryTerm(f)
        case s: Selector          => selectorToQueryTerm(s)
        case n: Numeral           => numeralToQueryTerm(n)
        case r: Ref               => refToQueryTerm(r)
        case s: SortMacro         => sortmacroToQueryTerm(s)
        case s: SortParameter     => sortparameterToQueryTerm(s)
        case t: TheoryMacro       => theorymacroToQueryTerm(t)
        case t: TheorySort        => theorysortToQueryTerm(t)
        case u: UserFunction      => userfunctionToQueryTerm(u)
        case u: UserMacro         => usermacroToQueryTerm(u)
        case u: UserSort          => usersortToQueryTerm(u)
        case m: middle.Module     => moduleToQueryTerm(m)
      }

    // Helper functions that deal with each case

    def applicationToQueryTerm(a: Application): String =
      if (a.args.length > 1) {
        indent += 1
        val args =
          a.args.zipWithIndex
            .map { p =>
              val t = dispatch(p._1)

              // if we have a constructor lets label the arguments
              if (term.stmts(a.caller.loc).isInstanceOf[Constructor]) {
                //get the ith selector
                val ctr = term.stmts(a.caller.loc).asInstanceOf[Constructor]
                val comment =
                  s"; assigning to ${dispatch(ctr.selectors(p._2))}\n${TAB * indent}"
                comment + t
              } else {
                t
              }

            }
            .mkString(s"\n${TAB * indent}")
        val result = s"(${dispatch(a.caller)}\n${TAB * indent}${args})"
        indent -= 1
        result
      } else {
        if (a.args.length == 1) {
          s"(${dispatch(a.caller)} ${dispatch(a.args(0))})"
        } else {
          dispatch(a.caller)
        }
      }

    def constructorToQueryTerm(c: Constructor): String =
      c.name

    def datatypeToQueryTerm(d: DataType): String =
      d.name

    def functionparameterToQueryTerm(f: FunctionParameter): String =
      f.name

    def selectorToQueryTerm(s: Selector): String =
      s.name

    def numeralToQueryTerm(n: Numeral): String =
      n.value.toString()

    def refToQueryTerm(r: Ref): String =
      dispatch(r)

    def sortmacroToQueryTerm(s: SortMacro): String =
      s.name

    def sortparameterToQueryTerm(s: SortParameter): String =
      s.name

    def theorymacroToQueryTerm(t: TheoryMacro): String =
      if (t.params.length > 0) {
        s"${t.name} (${t.params.map(p => dispatch(p)).mkString(" ")})"
      } else {
        t.name
      }

    def theorysortToQueryTerm(t: TheorySort): String =
      if (t.params.length > 0) {
        s"(${t.name} ${t.params.map(p => dispatch(p)).mkString(" ")})"
      } else {
        t.name
      }

    def userfunctionToQueryTerm(u: UserFunction): String =
      u.name

    def usermacroToQueryTerm(u: UserMacro): String =
      u.name

    def usersortToQueryTerm(u: UserSort): String =
      u.name

    def moduleToQueryTerm(m: middle.Module): String =
      m.name

    dispatch(point)
  }

  def programToQueryCtx(term: TermGraph): String = {
    var indent = 0
    val toDeclare = Collector.mark(term)

    def dispatch(position: Ref): Option[String] =
      if (toDeclare(position.loc)) {
        toDeclare.update(position.loc, false)
        term.stmts(position.loc) match {
          case r: Ref          => dispatch(r)
          case d: DataType     => Some(datatypeToQueryCtx(d))
          case s: SortMacro    => Some(sortmacroToQueryCtx(s))
          case u: UserFunction => Some(userfunctionToQueryCtx(u))
          case u: UserMacro => {
            val dispatched =
              List(dispatch(u.body), Some(usermacroToQueryCtx(u))).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString("\n"))
            } else {
              None
            }
          }
          case u: UserSort      => Some(usersortToQueryCtx(u))
          case m: middle.Module => Some(moduleToQueryCtx(m))
          case a: Application => {
            val dispatched =
              (List(a.caller) ++ a.args).map(a => dispatch(a)).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString("\n"))
            } else {
              None
            }
          }
          case _ => None
        }
      } else {
        None
      }

    // Helper functions that deal with each case

    def datatypeToQueryCtx(d: DataType): String = {
      val tmp = new StringBuilder()
      // for each constructor
      d.constructors.foreach { ct =>
        val ctr = term.stmts(ct.loc).asInstanceOf[Constructor]
        tmp ++= s"${TAB * indent}(declare-datatypes ((${d.name} 0)) (((${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= "\n"
          val sel = term.stmts(s.loc).asInstanceOf[Selector]
          tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(new TermGraph(term.stmts), sel.sort, indent)})"
        }
      }
      tmp ++= "))))\n"
      indent -= 1

      tmp.toString()
    }

    def sortmacroToQueryCtx(s: SortMacro): String =
      s"${TAB * indent}(define-sort ${s.name} ${programPointToQueryTerm(new TermGraph(term.stmts), s.body, indent)})"

    def userfunctionToQueryCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} (${u.params
          .map { p =>
            val fp = term.stmts(p.loc).asInstanceOf[FunctionParameter]
            s"(${fp.name} ${programPointToQueryTerm(new TermGraph(term.stmts), fp.sort, indent)})"
          }
          .mkString(" ")}) "
      } else {
        tmp ++= s"${TAB * indent}(declare-const ${u.name} "
      }
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(new TermGraph(term.stmts), u.sort, indent)})\n"

      tmp.toString()
    }

    def usermacroToQueryCtx(u: UserMacro): String = {
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(define-fun ${u.name} (${u.params
        .map { p =>
          val fp = term.stmts(p.loc).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(new TermGraph(term.stmts), fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(new TermGraph(term.stmts), u.sort, indent)}\n"
      indent += 1
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(new TermGraph(term.stmts), u.body, indent)})\n"
      indent -= 1

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name})"

    def moduleToQueryCtx(m: middle.Module): String = {
      val tmp = new StringBuilder()
      val ctr = term.stmts(m.ct.loc).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name} \n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= "\n"
        val sel = term.stmts(s.loc).asInstanceOf[Selector]
        tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(new TermGraph(term.stmts), sel.sort, indent)})"
      }
      tmp ++= "))))\n\n"
      indent -= 1
      val init = dispatch(m.init)
      val next = dispatch(m.next)
      val spec = dispatch(m.spec)

      tmp ++= List(init, next, spec).flatten.mkString("\n")
      indent -= 1
      tmp ++= s"${TAB * indent}; done declaring module ${m.name}\n"

      tmp.toString()
    }

    term.stmts.zipWithIndex.map(p => dispatch(Ref(p._2))).flatten.mkString("\n")
  }

  def programToQuery(term: TermGraph): String = {
    val assertions = term.assertions
      .map(r => s"(assert ${programPointToQueryTerm(term, r)})")
      .mkString("\n")
    "(set-logic ALL)\n(set-option :produce-models true)\n" + programToQueryCtx(
      term
    ) + "\n" + assertions + "\n(check-sat)\n(get-model)"
  }
}
