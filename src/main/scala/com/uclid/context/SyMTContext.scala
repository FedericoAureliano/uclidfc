package com.uclid.context

import com.uclid.termgraph._

import scala.collection.immutable.Nil
import scala.collection.mutable._

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
  def toQuery(prettyPrint: Boolean): String = {
    if (!prettyPrint) {
      TAB = ""
      NEWLINE = " "
    }
    val logic = s"(set-logic ${termgraph.queryLogic(entryPoints())})"
    val ctx = programToQueryCtx()
    val body = script
      .map { c =>
        c match {
          case Assert(t) => s"(assert ${programPointToQueryTerm(t)})"
          case Check()   => "(check-sat)"
        }
      }
      .mkString(s"$NEWLINE")

    logic + "\n" + ctx + "\n" + body
  }

  protected val options: ListBuffer[(String, String)] =
    ListBuffer(("produce-assignments", "true"))

  def addOption(option: String, value: String): Unit =
    options.addOne((option, value))

  protected val alreadyDeclared = new HashSet[Int]()
  protected var TAB = "  "
  protected var NEWLINE = "\n"

  protected def programPointToQueryTerm(
    point: Int,
    indentInput: Int = 0
  ): String = {
    var indent = indentInput

    val out = new StringBuilder()
    val stack = new Stack[Either[Int, String]]()

    stack.push(Left(point))

    while (!stack.isEmpty) {
      stack.pop() match {
        case Left(position) => {
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
                stack.push(Right(s" :named ${r.named})"))
                stack.push(Left(r.loc))
                stack.push(Right("(! "))
              } else if (!termgraph.isSynthesisQuery && alreadyDeclared.contains(r.loc)) {
                stack.push(Right(r.named))
              } else {
                stack.push(Left(r.loc))
              }
            case t: TheoryMacro  => theorymacroToQueryTerm(t)
            case t: TheorySort   => theorysortToQueryTerm(t)
            case u: UserFunction => userfunctionToQueryTerm(u)
            case s: Synthesis    => synthesisToQueryTerm(s)
            case u: UserMacro    => usermacroToQueryTerm(u)
            case u: UserSort     => usersortToQueryTerm(u)
            case m: Module       => moduleToQueryTerm(m)
          }
        }
        case Right(str) => {
          out.addAll(str)
          if (str == "\n") {
            out.addAll(s"${TAB * indent}")
          } else if (str == "(") {
            indent += 1
          } else if (str == ")") {
            indent -= 1
          }
        }
      }
    }


    // Helper functions that deal with each case

    def applicationToQueryTerm(a: Application): Unit =
      if (a.args.length > 1) {
        stack.push(Right(")"))
        a.args.reverse.zipWithIndex
          .foreach { p =>
            if (p._2 != 0) {
              stack.push(Right(s"$NEWLINE"))
            }
            // if we have a constructor lets label the arguments
            if (termgraph.stmts(a.caller).isInstanceOf[Constructor]) {
              //get the ith selector
              val ctr = termgraph.stmts(a.caller).asInstanceOf[Constructor]
              stack.push(Left(p._1))
              stack.push(Right(s"\n"))
              stack.push(Left(ctr.selectors(p._2)))
              stack.push(Right("; assigning to "))
            } else {
              stack.push(Left(p._1))
            }
          }
        stack.push(Right(s"$NEWLINE"))
        stack.push(Left(a.caller))
        stack.push(Right("("))
      } else {
        if (a.args.length == 1) {
          stack.push(Right(")"))
          stack.push(Left(a.args(0)))
          stack.push(Right(" "))
          stack.push(Left(a.caller))
          stack.push(Right("("))
        } else {
          stack.push(Left(a.caller))
        }
      }

    def constructorToQueryTerm(c: Constructor): Unit =
      stack.push(Right(c.name))

    def datatypeToQueryTerm(d: DataType): Unit =
      stack.push(Right(d.name))

    def functionparameterToQueryTerm(f: FunctionParameter): Unit =
      stack.push(Right(f.name))

    def selectorToQueryTerm(s: Selector): Unit =
      stack.push(Right(s.name))

    def numeralToQueryTerm(n: Numeral): Unit =
      stack.push(Right(n.value.toString()))

    def theorymacroToQueryTerm(t: TheoryMacro): Unit =
      if (t.params.length > 0) {
        stack.push(Right(")"))
        t.params.reverse.foreach { s =>
          val sel = termgraph.stmts(s).asInstanceOf[FunctionParameter]
          stack.push(Right(s"(${sel.name} ${programPointToQueryTerm(sel.sort, 0)})"))
        }
        stack.push(Right(s"${t.name} ("))
      } else {
        stack.push(Right(t.name))
      }

    def theorysortToQueryTerm(t: TheorySort): Unit =
      if (t.params.length > 0) {
        stack.push(Right(")"))
        t.params.reverse.foreach(p => {
          stack.push(Left(p))
          stack.push(Right(" "))
        })
        stack.push(Right(s"(${t.name} "))
      } else {
        stack.push(Right(t.name))
      }

    def userfunctionToQueryTerm(u: UserFunction): Unit =
      stack.push(Right(u.name))

    def usermacroToQueryTerm(u: UserMacro): Unit =
      stack.push(Right(u.name))

    def synthesisToQueryTerm(s: Synthesis): Unit = {
      if (!termgraph.isSynthesisQuery) {
        throw new SemanticError("Must be a synthesis query!")
      }
      stack.push(Right(s.name))
    }

    def usersortToQueryTerm(u: UserSort): Unit =
      stack.push(Right(u.name))

    def moduleToQueryTerm(m: Module): Unit =
      stack.push(Right(m.name))

    out.toString()
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
              Some(dispatched.mkString(s"$NEWLINE"))
            } else {
              None
            }
          case u: UserSort => Some(usersortToQueryCtx(u))
          case m: Module   => Some(moduleToQueryCtx(m))
          case a: Application =>
            val dispatched =
              (List(a.caller) ++ a.args).map(a => dispatch(a)).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString(s"$NEWLINE"))
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
          tmp ++= s"$NEWLINE"
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

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)}$NEWLINE"
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

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})$NEWLINE"

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name} ${u.arity.value})"

    def moduleToQueryCtx(m: Module): String = {
      val tmp = new StringBuilder()
      val ctr = termgraph.stmts(m.ct).asInstanceOf[Constructor]
      tmp ++= s"${TAB * indent}; declaring module ${m.name}\n"
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= s"$NEWLINE"
        val sel = termgraph.stmts(s).asInstanceOf[Selector]
        tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
      }
      tmp ++= s"))))$NEWLINE$NEWLINE"
      indent -= 1
      val init = dispatch(m.init)
      val next = dispatch(m.next)
      val spec = dispatch(m.spec)

      tmp ++= List(init, next, spec).flatten.mkString(s"$NEWLINE")
      indent -= 1
      tmp ++= s"${TAB * indent}\n; done declaring module ${m.name}\n"

      tmp.toString()
    }

    termgraph.stmts.zipWithIndex
      .map(p => dispatch(p._2))
      .flatten
      .mkString(s"$NEWLINE")
  }
}
