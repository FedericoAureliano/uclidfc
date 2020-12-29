package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

class Writable(stmts: ArrayBuffer[Instruction]) extends Minimal(stmts) {

  val TAB = "  "

  def inferLogic(): String = "ALL"

  var isSynthesisQuery = false
  var options: List[(String, String)] = List.empty

  val assertionRefs: ListBuffer[Ref] = new ListBuffer()

  def programPointToQueryTerm(
    point: Ref,
    indentInput: Int = 0
  ): String = {
    var indent = indentInput

    def dispatch(position: Ref): String =
      stmts(position.loc) match {
        case a: Application       => applicationToQueryTerm(a)
        case c: Constructor       => constructorToQueryTerm(c)
        case d: DataType          => datatypeToQueryTerm(d)
        case f: FunctionParameter => functionparameterToQueryTerm(f)
        case s: Selector          => selectorToQueryTerm(s)
        case n: Numeral           => numeralToQueryTerm(n)
        case r: Ref               => refToQueryTerm(r)
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
              if (stmts(a.caller.loc).isInstanceOf[Constructor]) {
                //get the ith selector
                val ctr = stmts(a.caller.loc).asInstanceOf[Constructor]
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

    def theorymacroToQueryTerm(t: TheoryMacro): String =
      if (t.params.length > 0) {
        val args = t.params.map { s =>
          val sel = stmts(s.loc).asInstanceOf[FunctionParameter]
          s"(${sel.name} ${programPointToQueryTerm(sel.sort, 0)})"
        }
        s"${t.name} (${args.mkString(" ")})"
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

  def programToQueryCtx(): String = {
    var indent = 0
    val toDeclare = mark(assertionRefs)

    def dispatch(position: Ref): Option[String] =
      if (toDeclare(position.loc)) {
        toDeclare.update(position.loc, false)
        stmts(position.loc) match {
          case r: Ref          => dispatch(r)
          case d: DataType     => Some(datatypeToQueryCtx(d))
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
      tmp ++= s"${TAB * indent}(declare-datatypes ((${d.name} 0)) (("
      d.constructors.foreach { ct =>
        val ctr = stmts(ct.loc).asInstanceOf[Constructor]
        tmp ++= s"(${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= "\n"
          val sel = stmts(s.loc).asInstanceOf[Selector]
          tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
        }
        tmp ++= ")"
      }
      tmp ++= ")))\n"
      indent -= 1

      tmp.toString()
    }

    def userfunctionToQueryCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} ${u.params
          .map(p => s"(${programPointToQueryTerm(p, indent)})")
          .mkString(" ")} "
      } else {
        tmp ++= s"${TAB * indent}(declare-const ${u.name} "
      }
      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})\n"

      tmp.toString()
    }

    def usermacroToQueryCtx(u: UserMacro): String = {
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(define-fun ${u.name} (${u.params
        .map { p =>
          val fp = stmts(p.loc).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)}\n"
      indent += 1
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(u.body, indent)})\n"
      indent -= 1

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name} ${u.arity.value})"

    def moduleToQueryCtx(m: middle.Module): String = {
      val tmp = new StringBuilder()
      val ctr = stmts(m.ct.loc).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name} \n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= "\n"
        val sel = stmts(s.loc).asInstanceOf[Selector]
        tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
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

    stmts.zipWithIndex.map(p => dispatch(Ref(p._2))).flatten.mkString("\n")
  }

  def programToQuery(): String =
    if (assertionRefs.length > 0) {
      val assertionStrings = assertionRefs
        .map(r => s"${programPointToQueryTerm(r)}\n")
        .mkString("\n")

      programToQueryCtx() + "\n" + "(assert (or\n" + assertionStrings + "))\n(check-sat)"
    } else {
      "; nothing to verify"
    }
}
