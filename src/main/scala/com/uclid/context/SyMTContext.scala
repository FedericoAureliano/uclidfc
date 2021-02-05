package com.uclid.context

import com.uclid.termgraph._

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.immutable.Nil

sealed trait Command
case class Assert(t: Int) extends Command
case class Check() extends Command

class SyMTContext(termgraph: TermGraph) extends Context(termgraph) {
  override def ignoreResult() = false
  var script: List[Command] = List.empty

  override def entryPoints() = script.foldLeft(List.empty)((acc, c) => {
    c match {
      case Assert(t) => t :: acc
      case Check() => acc
    }
  })

  def addAssertion(r: Int): Unit =
    script = script ++ List(Assert(r))

  def checkSat(): Unit =
    script = script ++ List(Check()
)
  def toQuery(): String = {
    val logic = s"(set-logic ${termgraph.queryLogic(entryPoints())})"
    val ctx = programToQueryCtx()
    val body = script
      .map { c =>
        c match {
          case Assert(t) => s"(assert ${programPointToQueryTerm(t)})"
          case Check()   => "(check-sat)"
        }
      }
      .mkString("\n")

    logic + "\n" + ctx + "\n" + body
  }

  protected val options: ListBuffer[(String, String)] =
    ListBuffer(("produce-assignments", "true"))

  def addOption(option: String, value: String): Unit =
    options.addOne((option, value))

  protected val alreadyDeclared = new HashSet[Int]()
  protected val TAB = "  "

  protected def programPointToQueryTerm(
    point: Int,
    indentInput: Int = 0
  ): String = {
    var indent = indentInput

    def dispatch(position: Int): String =
      termgraph.stmts(position) match {
        case a: Application       => applicationToQueryTerm(a)
        case c: Constructor       => constructorToQueryTerm(c)
        case d: DataType          => datatypeToQueryTerm(d)
        case f: FunctionParameter => functionparameterToQueryTerm(f)
        case s: Selector          => selectorToQueryTerm(s)
        case n: Numeral           => numeralToQueryTerm(n)
        case r: Ref =>
          if (!termgraph.isSynthesisQuery && !alreadyDeclared.contains(r.loc)) {
            alreadyDeclared.add(r.loc)
            s"(! ${dispatch(r.loc)} :named ${r.named})"
          } else if (!termgraph.isSynthesisQuery && alreadyDeclared.contains(r.loc)) {
            r.named
          } else {
            dispatch(r.loc)
          }
        case t: TheoryMacro  => theorymacroToQueryTerm(t)
        case t: TheorySort   => theorysortToQueryTerm(t)
        case u: UserFunction => userfunctionToQueryTerm(u)
        case s: Synthesis    => synthesisToQueryTerm(s)
        case u: UserMacro    => usermacroToQueryTerm(u)
        case u: UserSort     => usersortToQueryTerm(u)
        case m: Module       => moduleToQueryTerm(m)
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
              if (termgraph.stmts(a.caller).isInstanceOf[Constructor]) {
                //get the ith selector
                val ctr = termgraph.stmts(a.caller).asInstanceOf[Constructor]
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
          val sel = termgraph.stmts(s).asInstanceOf[FunctionParameter]
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
      if (!termgraph.isSynthesisQuery) {
        throw new SemanticError("Must be a synthesis query!")
      }
      s.name
    }

    def usersortToQueryTerm(u: UserSort): String =
      u.name

    def moduleToQueryTerm(m: Module): String =
      m.name

    dispatch(point)
  }

  def programToQueryCtx(): String = {
    var indent = 0
    val toDeclare = Array.fill[Boolean](termgraph.numberOfNodes())(true)

    def dispatch(position: Int): Option[String] =
      if (toDeclare(position)) {
        toDeclare.update(position, false)
        termgraph.stmts(position) match {
          case r: Ref          => dispatch(r.loc)
          case d: DataType     => Some(datatypeToQueryCtx(d))
          case u: UserFunction => Some(userfunctionToQueryCtx(u))
          case s: Synthesis    => Some(synthesisToQueryCtx(s))
          case u: UserMacro =>
            val dispatched =
              List(dispatch(u.body), Some(usermacroToQueryCtx(u))).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString("\n"))
            } else {
              None
            }
          case u: UserSort => Some(usersortToQueryCtx(u))
          case m: Module   => Some(moduleToQueryCtx(m))
          case a: Application =>
            val dispatched =
              (List(a.caller) ++ a.args).map(a => dispatch(a)).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString("\n"))
            } else {
              None
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
        val ctr = termgraph.stmts(ct).asInstanceOf[Constructor]
        tmp ++= s"(${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= "\n"
          val sel = termgraph.stmts(s).asInstanceOf[Selector]
          tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
        }
        indent -= 1
        tmp ++= ")"
      }
      tmp ++= ")))"
      indent -= 1

      tmp.toString()
    }

    def userfunctionToQueryCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        if (termgraph.isSynthesisQuery) {
          throw new SemanticError(
            "Uninterpreted functions are not supported for synthesis"
          )
        }
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} (${u.params
          .map(p => s"${programPointToQueryTerm(p, indent)}")
          .mkString(" ")}) "
      } else {
        if (termgraph.isSynthesisQuery) {
          tmp ++= s"${TAB * indent}(declare-var ${u.name} "
        } else {
          tmp ++= s"${TAB * indent}(declare-const ${u.name} "
        }
      }
      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})"

      tmp.toString()
    }

    def usermacroToQueryCtx(u: UserMacro): String = {
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(define-fun ${u.name} (${u.params
        .map { p =>
          val fp = termgraph.stmts(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)}\n"
      indent += 1
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(u.body, indent)})"
      indent -= 1

      tmp.toString()
    }

    def synthesisToQueryCtx(u: Synthesis): String = {
      if (!termgraph.isSynthesisQuery) {
        throw new SemanticError("Must be a synthesis query!")
      }
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(synth-fun ${u.name} (${u.params
        .map { p =>
          val fp = termgraph.stmts(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})\n"

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name} ${u.arity.value})"

    def moduleToQueryCtx(m: Module): String = {
      val tmp = new StringBuilder()
      val ctr = termgraph.stmts(m.ct).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name} \n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= "\n"
        val sel = termgraph.stmts(s).asInstanceOf[Selector]
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

    termgraph.stmts.zipWithIndex
      .map(p => dispatch(p._2))
      .flatten
      .mkString("\n")
  }
}
