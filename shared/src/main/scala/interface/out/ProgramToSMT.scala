package interface.out

import middle.core.garbage

import middle.core
import middle.core._
import middle.core.rewrite
import front._
import scala.collection.mutable.HashSet

package object smt {

  val TAB = "  "

  def programToSmtTerm(term: Program, indentInput: Int): String = {
    var indent = indentInput

    def dispatch(position: Ref): String =
      term.stmts(position.loc) match {
        case a: Application       => applicationToSmtTerm(a)
        case c: Constructor       => constructorToSmtTerm(c)
        case d: DataType          => datatypeToSmtTerm(d)
        case f: FunctionParameter => functionparameterToSmtTerm(f)
        case s: Selector          => selectorToSmtTerm(s)
        case n: Numeral           => numeralToSmtTerm(n)
        case r: Ref               => refToSmtTerm(r)
        case s: SortMacro         => sortmacroToSmtTerm(s)
        case s: SortParameter     => sortparameterToSmtTerm(s)
        case t: TheoryMacro       => theorymacroToSmtTerm(t)
        case t: TheorySort        => theorysortToSmtTerm(t)
        case u: UserFunction      => userfunctionToSmtTerm(u)
        case u: UserMacro         => usermacroToSmtTerm(u)
        case u: UserSort          => usersortToSmtTerm(u)
        case m: core.Module       => moduleToSmtTerm(m)
      }

    // Helper functions that deal with each case

    def applicationToSmtTerm(a: Application): String =
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

    def constructorToSmtTerm(c: Constructor): String =
      c.name

    def datatypeToSmtTerm(d: DataType): String =
      d.name

    def functionparameterToSmtTerm(f: FunctionParameter): String =
      f.name

    def selectorToSmtTerm(s: Selector): String =
      s.name

    def numeralToSmtTerm(n: Numeral): String =
      n.value.toString()

    def refToSmtTerm(r: Ref): String =
      dispatch(r)

    def sortmacroToSmtTerm(s: SortMacro): String =
      s.name

    def sortparameterToSmtTerm(s: SortParameter): String =
      s.name

    def theorymacroToSmtTerm(t: TheoryMacro): String =
      if (t.params.length > 0) {
        s"${t.name} (${t.params.map(p => dispatch(p)).mkString(" ")})"
      } else {
        t.name
      }

    def theorysortToSmtTerm(t: TheorySort): String =
      if (t.params.length > 0) {
        s"(${t.name} ${t.params.map(p => dispatch(p)).mkString(" ")})"
      } else {
        t.name
      }

    def userfunctionToSmtTerm(u: UserFunction): String =
      u.name

    def usermacroToSmtTerm(u: UserMacro): String =
      u.name

    def usersortToSmtTerm(u: UserSort): String =
      u.name

    def moduleToSmtTerm(m: core.Module): String =
      m.name

    dispatch(Ref(term.head))
  }

  def programToSmtCtx(term: Program): String = {
    var indent = 0
    val toDeclare = garbage.mark(term)

    def dispatch(position: Ref): Option[String] =
      if (toDeclare(position.loc)) {
        toDeclare.update(position.loc, false)
        term.stmts(position.loc) match {
          case r: Ref          => dispatch(r)
          case d: DataType     => Some(datatypeToSmtCtx(d))
          case s: SortMacro    => Some(sortmacroToSmtCtx(s))
          case u: UserFunction => Some(userfunctionToSmtCtx(u))
          case u: UserMacro => {
            val dispatched =
              List(dispatch(u.body), Some(usermacroToSmtCtx(u))).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString("\n"))
            } else {
              None
            }
          }
          case u: UserSort    => Some(usersortToSmtCtx(u))
          case m: core.Module => Some(moduleToSmtCtx(m))
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

    def datatypeToSmtCtx(d: DataType): String = {
      val tmp = new StringBuilder()
      // for each constructor
      d.constructors.foreach { ct =>
        val ctr = term.stmts(ct.loc).asInstanceOf[Constructor]
        tmp ++= s"${TAB * indent}(declare-datatypes ((${d.name} 0)) (((${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= "\n"
          val sel = term.stmts(s.loc).asInstanceOf[Selector]
          tmp ++= s"${TAB * indent}(${sel.name} ${programToSmtTerm(new Program(term.stmts, sel.sort.loc), indent)})"
        }
      }
      tmp ++= "))))\n"
      indent -= 1

      tmp.toString()
    }

    def sortmacroToSmtCtx(s: SortMacro): String =
      s"${TAB * indent}(define-sort ${s.name} ${programToSmtTerm(new Program(term.stmts, s.body.loc), indent)})"

    def userfunctionToSmtCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} (${u.params
          .map { p =>
            val fp = term.stmts(p.loc).asInstanceOf[FunctionParameter]
            s"(${fp.name} ${programToSmtTerm(new Program(term.stmts, fp.sort.loc), indent)})"
          }
          .mkString(" ")}) "
      } else {
        tmp ++= s"${TAB * indent}(declare-const ${u.name}"
      }
      tmp ++= s"${TAB * indent}${programToSmtTerm(new Program(term.stmts, u.sort.loc), indent)})\n"

      tmp.toString()
    }

    def usermacroToSmtCtx(u: UserMacro): String = {
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(define-fun ${u.name} (${u.params
        .map { p =>
          val fp = term.stmts(p.loc).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programToSmtTerm(new Program(term.stmts, fp.sort.loc), indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programToSmtTerm(new Program(term.stmts, u.sort.loc), indent)}\n"
      indent += 1
      tmp ++= s"${TAB * indent}${programToSmtTerm(new Program(term.stmts, u.body.loc), indent)})\n"
      indent -= 1

      tmp.toString()
    }

    def usersortToSmtCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name})"

    def moduleToSmtCtx(m: core.Module): String = {
      val tmp = new StringBuilder()
      val ctr = term.stmts(m.ct.loc).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name} \n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= "\n"
        val sel = term.stmts(s.loc).asInstanceOf[Selector]
        tmp ++= s"${TAB * indent}(${sel.name} ${programToSmtTerm(new Program(term.stmts, sel.sort.loc), indent)})"
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

  def programToSmt(term: Program): String =
    programToSmtCtx(term) + "\n" + programToSmtTerm(term, 0)
}
