package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class Writable(stmts: ArrayBuffer[Instruction]) extends Minimal(stmts) {

  private val TAB = "  "

  var isSynthesisQuery = false
  var checkQuery = false
  var traceQuery = false
  var getValues: Option[List[Int]] = None

  protected def inferLogic(): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true

    val marks = mark(assertionRefs ++ axiomRefs)

    stmts
      .zip(marks)
      .foreach(inst =>
        if (inst._2) {
          inst._1 match {
            case _: AbstractDataType => dt = true
            case Application(caller, args) => {
              stmts(caller) match {
                case TheoryMacro("*", _) => {
                  if (args.filter { a =>
                        stmts(a) match {
                          case TheoryMacro(name, _) =>
                            name.toIntOption.isDefined
                          case _ => false
                        }
                      }.length < args.length - 1) {
                    linear = false
                  }
                }
                case _ =>
              }
            }
            case TheoryMacro("exists", _) => qf = false
            case TheoryMacro("forall", _) => qf = false
            case TheoryMacro(name, _) =>
              if (name.toIntOption.isDefined) { i = true }
            case UserFunction(_, _, params) =>
              if (params.length > 0) { uf = true }
            case TheorySort("Array", _) => a = true
            case TheorySort("Int", _)   => i = true
            case Synthesis(_, _, _)     => isSynthesisQuery = true
            case _                      =>
          }
        }
      )

    s"${if (qf && !isSynthesisQuery) { "QF_" }
    else { "" }}${if (uf) { "UF" }
    else { "" }}${if (a) { "A" }
    else { "" }}${if (dt) { "DT" }
    else { "" }}${if (linear && i) { "L" }
    else if (!linear && i) { "N" }
    else { "" }}${if (i) { "IA" }
    else { "" }}"
  }

  var options: List[(String, String)] =
    List(("produce-assignments", "true"))

  private val alreadyDeclared = new HashSet[Int]()

  protected def programPointToQueryTerm(
    point: Int,
    indentInput: Int = 0
  ): String = {
    var indent = indentInput

    def dispatch(position: Int): String =
      stmts(position) match {
        case a: Application       => applicationToQueryTerm(a)
        case c: Constructor       => constructorToQueryTerm(c)
        case d: DataType          => datatypeToQueryTerm(d)
        case f: FunctionParameter => functionparameterToQueryTerm(f)
        case s: Selector          => selectorToQueryTerm(s)
        case n: Numeral           => numeralToQueryTerm(n)
        case r: Ref => {
          if (!isSynthesisQuery && !alreadyDeclared.contains(r.loc)) {
            alreadyDeclared.add(r.loc)
            s"(! ${dispatch(r.loc)} :named ${r.named})"
          } else if (!isSynthesisQuery && alreadyDeclared.contains(r.loc)) {
            r.named
          } else {
            dispatch(r.loc)
          }
        }
        case t: TheoryMacro   => theorymacroToQueryTerm(t)
        case t: TheorySort    => theorysortToQueryTerm(t)
        case u: UserFunction  => userfunctionToQueryTerm(u)
        case s: Synthesis     => synthesisToQueryTerm(s)
        case u: UserMacro     => usermacroToQueryTerm(u)
        case u: UserSort      => usersortToQueryTerm(u)
        case m: middle.Module => moduleToQueryTerm(m)
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
              if (stmts(a.caller).isInstanceOf[Constructor]) {
                //get the ith selector
                val ctr = stmts(a.caller).asInstanceOf[Constructor]
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

    def theorymacroToQueryTerm(t: TheoryMacro): String =
      if (t.params.length > 0) {
        val args = t.params.map { s =>
          val sel = stmts(s).asInstanceOf[FunctionParameter]
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

    def synthesisToQueryTerm(s: Synthesis): String = {
      if (!isSynthesisQuery) {
        throw new NotSupportedSynthesis("Must be a synthesis query!")
      }
      s.name
    }

    def usersortToQueryTerm(u: UserSort): String =
      u.name

    def moduleToQueryTerm(m: middle.Module): String =
      m.name

    dispatch(point)
  }

  def programToQueryCtx(): String = {
    var indent = 0
    val toDeclare = mark(
      assertionRefs ++ getValues.getOrElse(List.empty) ++ axiomRefs
    )

    def dispatch(position: Int): Option[String] =
      if (toDeclare(position)) {
        toDeclare.update(position, false)
        stmts(position) match {
          case r: Ref          => dispatch(r.loc)
          case d: DataType     => Some(datatypeToQueryCtx(d))
          case u: UserFunction => Some(userfunctionToQueryCtx(u))
          case s: Synthesis    => Some(synthesisToQueryCtx(s))
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
      indent += 1
      d.constructors.foreach { ct =>
        val ctr = stmts(ct).asInstanceOf[Constructor]
        tmp ++= s"(${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= "\n"
          val sel = stmts(s).asInstanceOf[Selector]
          tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
        }
        indent -= 1
        tmp ++= ")"
      }
      tmp ++= ")))\n"
      indent -= 1

      tmp.toString()
    }

    def userfunctionToQueryCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        if (isSynthesisQuery) {
          throw new NotSupportedSynthesis(
            "Uninterpreted functions are not supported for synthesis"
          )
        }
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} (${u.params
          .map(p => s"${programPointToQueryTerm(p, indent)}")
          .mkString(" ")}) "
      } else {
        if (isSynthesisQuery) {
          tmp ++= s"${TAB * indent}(declare-var ${u.name} "
        } else {
          tmp ++= s"${TAB * indent}(declare-const ${u.name} "
        }
      }
      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})\n"

      tmp.toString()
    }

    def usermacroToQueryCtx(u: UserMacro): String = {
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(define-fun ${u.name} (${u.params
        .map { p =>
          val fp = stmts(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)}\n"
      indent += 1
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(u.body, indent)})\n"
      indent -= 1

      tmp.toString()
    }

    def synthesisToQueryCtx(u: Synthesis): String = {
      if (!isSynthesisQuery) {
        throw new NotSupportedSynthesis("Must be a synthesis query!")
      }
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(synth-fun ${u.name} (${u.params
        .map { p =>
          val fp = stmts(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})\n"

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name} ${u.arity.value})"

    def moduleToQueryCtx(m: middle.Module): String = {
      val tmp = new StringBuilder()
      val ctr = stmts(m.ct).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name} \n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= "\n"
        val sel = stmts(s).asInstanceOf[Selector]
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

    stmts.zipWithIndex
      .map(p => dispatch(p._2))
      .flatten
      .mkString("\n")
  }

  def programToQuery(): String = {
    alreadyDeclared.clear()
    val logic = s"(set-logic ${inferLogic()})"
    val opts = options.map(o => s"(set-option :${o._1} ${o._2})").mkString("\n")

    val axiomStrings = axiomRefs
      .map(r => s"${TAB * 1}${programPointToQueryTerm(r, 1)}")
      .mkString("\n")

    val assertionStrings = assertionRefs
      .map(r => s"${TAB * 2}${programPointToQueryTerm(r, 2)}")
      .mkString("\n")

    val spec = (assertionRefs.length > 0, axiomRefs.length > 0) match {
      case (true, true) =>
        s"(and\n$axiomStrings\n${TAB * 1}(or\n$assertionStrings))"
      case (true, false) => s"(or\n$assertionStrings)"
      case (false, true) => s"(and\n$axiomStrings)"
      case _             => ""
    }

    val body = if (spec != "") {
      if (isSynthesisQuery) {
        programToQueryCtx() + "\n(constraint (not " + spec + "))"
      } else {
        programToQueryCtx() + "\n(assert " + spec + ")"
      }
    } else {
      if (getValues.isDefined) {
        programToQueryCtx()
      } + "\n; nothing to verify"
    }

    val postQuery = if (checkQuery || traceQuery) {
      val model = if (getValues.isDefined) {
        val cmd = if (getValues.get.length == 0) {
          "(get-model)"
        } else {
          s"(get-value (${getValues.get.map(v => programPointToQueryTerm(v)).mkString(" ")}))"
        }
        "(echo \"Model\")\n" + cmd
      } else {
        ""
      }

      val proofStatus = if (assertionRefs.length + axiomRefs.length > 0) {
        "(echo \"Proof Status\")\n(get-assignment)"
      } else {
        ""
      }

      if (isSynthesisQuery) {
        "\n\n(check-synth)"
      } else {
        "\n\n(check-sat)\n(echo \"\")\n" + proofStatus + "\n(echo \"\")\n" + model
      }
    } else {
      ""
    }

    s"$logic\n$opts\n\n$body\n$postQuery"
  }
}
