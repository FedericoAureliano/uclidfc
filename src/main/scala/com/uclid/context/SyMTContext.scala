package com.uclid.context

import com.uclid.termgraph._

import scala.collection.immutable.Nil
import scala.collection.mutable._

val TAB = "  "

sealed abstract class ToPrint
sealed case class Direct(str: String) extends ToPrint
sealed case class Jump(pos: Int) extends ToPrint
sealed case class NewLine() extends ToPrint
sealed case class Indent(amount: Int) extends ToPrint

sealed trait Command
case class Assert(t: Int) extends Command
case class Check() extends Command

class SyMTContext(termgraph: TermGraph) extends Context(termgraph) {
  override def ignoreResult() = false
  var script: List[Command] = List.empty

  protected var prettyPrint = 0;

  def newline() = if (prettyPrint > 0) {"\n"} else {" "}

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
  def toQueries(pp: Int): List[String] = {
    prettyPrint = pp
    val isSynthesis = termgraph.isSynthesisQuery(entryPoints())
    
    val logic = s"(set-logic ${termgraph.queryLogic(entryPoints())})"
    val opts = options.map(o => s"(set-option :${o._1} ${o._2})").mkString(s"${newline()}")
    
    val info = if (isSynthesis) {
      logic
    } else {
      List("(set-info :smt-lib-version 2.6)", logic, "(set-info :category \"industrial\")", "(set-info :source |Generator: Uclid5.|)", "(set-info :status unknown)").mkString("\n")
    }

    val ctx = programToQueryCtx(termgraph.mark(entryPoints()))
    val body = script
      .map { c =>
        c match {
          case Assert(t) => if (isSynthesis) {
            s"(constraint (not ${programPointToQueryTerm(t)}))"
          } else {
            s"(assert ${programPointToQueryTerm(t, 0, pp == 0)})"
          }
          case Check()   => if (isSynthesis) {
            "(check-synth)"
          } else {
            "(check-sat)"
          }
        }
      }
      .mkString(s"${newline()}")

    val internal = if (prettyPrint > 1) {
      "\n\n; " + termgraph.getStmts().zipWithIndex.map((inst, i) => s"${i}: ${inst}").mkString("\n; ")
    } else {
      ""
    }

    List(List(info, opts, ctx, body, internal).mkString("\n"))
  }

  protected val options: ListBuffer[(String, String)] = ListBuffer()

  def addOption(option: String, value: String): Unit =
    options.addOne((option, value))

  protected def programPointToQueryTerm(
    pos: Int,
    indentInput: Int = 0,
    memoizeIn: Boolean = false
  ): String = {
    var indent = indentInput
    var memoize = memoizeIn
    val out = new StringBuilder()
    val stack = new Stack[ToPrint]()

    val point = termgraph.findTarget(pos)

    def pushAssignmentComment(sel: Int) = {
      if (prettyPrint > 0) {
        stack.push(NewLine())
        stack.push(Direct(s"; assigning to ${termgraph.getStmt(sel).asInstanceOf[Selector].name}"))
      } else {
        stack.push(Direct(" "))
      }
    }

    def pushDebugComment(inst: Instruction) = {
      if (prettyPrint > 1) {
        stack.push(NewLine())
        stack.push(Direct(s"; Instruction: ${inst} @${termgraph.memoGetInstruction(inst)}"))
      }
    }

    val memo : HashMap[Int, String] = new HashMap()

    stack.push(Jump(point))

    while (!stack.isEmpty) {
      stack.pop() match {
        case Jump(p) => {
          val position = termgraph.findTarget(p)
          termgraph.getStmt(position) match {
            case a: Application       => {
              if (termgraph.isQuantifier(a.function)) {
                // so that we don't get expressions with free variables
                // need to think of a less cautious way
                memoize = false 
              }
              if (memoize && memo.contains(position)) {
                stack.push(Direct(memo(position)))
              } else {
                applicationToQueryTerm(a)
              }
            }
            case c: Constructor       => constructorToQueryTerm(c)
            case d: DataType          => datatypeToQueryTerm(d)
            case f: FunctionParameter => functionparameterToQueryTerm(f)
            case s: Selector          => selectorToQueryTerm(s)
            case n: Numeral           => numeralToQueryTerm(n)
            case r: Ref               => stack.push(Jump(r.loc))
            case t: TheoryMacro  => theorymacroToQueryTerm(t)
            case t: TheorySort   => theorysortToQueryTerm(t)
            case u: UserFunction => userfunctionToQueryTerm(u)
            case s: Synthesis    => synthesisToQueryTerm(s)
            case u: UserMacro    => usermacroToQueryTerm(u)
            case u: UserSort     => usersortToQueryTerm(u)
            case m: Module       => moduleToQueryTerm(m)
          }
        }
        case Direct(str) => out.addAll(str)
        case Indent(change) => indent += change
        case NewLine() => if (prettyPrint > 0) {
          out.addAll(s"\n${TAB * indent}")
        } else {
          out.addAll(" ")
        }
      }
    }


    // Helper functions that deal with each case
    def applicationToQueryTerm(a: Application): Unit = {
      pushDebugComment(a)
      if (memoize) {
        val position = termgraph.memoGetInstruction(a)
        val name = Util.freshSymbolName()
        memo(position) = name
        stack.push(Direct(s" :named ${name})"))
      }
      if (a.args.length > 1) {
        stack.push(Indent(-1))
        stack.push(Direct(")"))
        a.args.reverse.zipWithIndex
          .foreach { p =>
            if (p._2 != 0) {
              stack.push(NewLine())
            }
            // if we have a constructor lets label the arguments
            if (termgraph.getStmt(a.function).isInstanceOf[Constructor]) {
              //get the ith selector
              val ctr = termgraph.getStmt(a.function).asInstanceOf[Constructor]
              stack.push(Jump(p._1))
              pushAssignmentComment(ctr.selectors.reverse(p._2))
            } else {
              stack.push(Jump(p._1))
            }
          }
        stack.push(NewLine())
        stack.push(Indent(1))
        stack.push(Jump(a.function))
        stack.push(Direct("("))
      } else {
        if (a.args.length == 1) {
          stack.push(Direct(")"))
          stack.push(Jump(a.args(0)))
          stack.push(Direct(" "))
          stack.push(Jump(a.function))
          stack.push(Direct("("))
        } else {
          stack.push(Jump(a.function))
        }
      }
      if (memoize) {
        stack.push(Direct("(! "))
      }
    }

    def constructorToQueryTerm(c: Constructor): Unit = {
      pushDebugComment(c)
      stack.push(Direct(c.name))
    }

    def datatypeToQueryTerm(d: DataType): Unit = {
      pushDebugComment(d)
      stack.push(Direct(d.name))
    }

    def functionparameterToQueryTerm(f: FunctionParameter): Unit = {
      pushDebugComment(f)
      stack.push(Direct(f.name))
    }

    def selectorToQueryTerm(s: Selector): Unit = {
      pushDebugComment(s)
      stack.push(Direct(s.name))
    }

    def numeralToQueryTerm(n: Numeral): Unit =
      stack.push(Direct(n.value.toString()))

    def theorymacroToQueryTerm(t: TheoryMacro): Unit = {
      pushDebugComment(t)
      if (t.name == "forall" || t.name == "exists") {
        stack.push(Direct(")"))
        t.params.reverse.foreach { s =>
          val sel = termgraph.getStmt(s).asInstanceOf[FunctionParameter]
          stack.push(Direct(s"(${sel.name} ${programPointToQueryTerm(sel.sort, 0)})"))
        }
        stack.push(Direct(s"${t.name} ("))
      } else if (t.name.startsWith("bv") && t.name.drop(2).toIntOption.isDefined) {
        stack.push(Direct(s"(_ ${t.name} ${programPointToQueryTerm(t.params(0), 0)})"))
      } else if (t.name == "as const") {
        stack.push(Direct(s"(${t.name} ${programPointToQueryTerm(t.params(0), 0)})"))
      } else {
        stack.push(Direct(t.name))
      }
    }

    def theorysortToQueryTerm(t: TheorySort): Unit = {
      pushDebugComment(t)
      if (t.params.length > 0) {
        stack.push(Direct(")"))
        t.params.reverse.foreach(p => {
          stack.push(Jump(p))
          stack.push(Direct(" "))
        })
        stack.push(Direct(s"${t.name}"))
        if (t.name == "BitVec") {
          stack.push(Direct("(_ "))
        } else {
          stack.push(Direct("( "))
        }
      } else {
        stack.push(Direct(t.name))
      }
    }

    def userfunctionToQueryTerm(u: UserFunction): Unit = {
      pushDebugComment(u)
      stack.push(Direct(u.name))
    }

    def usermacroToQueryTerm(u: UserMacro): Unit = {
      pushDebugComment(u)
      stack.push(Direct(u.name))
    }
    def synthesisToQueryTerm(s: Synthesis): Unit = {
      pushDebugComment(s)
      if (!termgraph.isSynthesisQuery()) {
        throw new SemanticError("Must be a synthesis query!")
      }
      stack.push(Direct(s.name))
    }

    def usersortToQueryTerm(u: UserSort): Unit = {
      pushDebugComment(u)
      stack.push(Direct(u.name))
    }

    def moduleToQueryTerm(m: Module): Unit = {
      pushDebugComment(m)
      stack.push(Direct(m.name))
    }

    out.toString()
  }

  def programToQueryCtx(toDeclare: Array[Boolean]): String = {
    var indent = 0

    def dispatch(pos: Int): Option[String] =
      val position = termgraph.findTarget(pos)
      if (toDeclare(position)) {
        toDeclare.update(position, false)
        termgraph.getStmt(position) match {
          case r: Ref          => dispatch(r.loc)
          case d: DataType     => Some(datatypeToQueryCtx(d))
          case u: UserFunction => Some(userfunctionToQueryCtx(u))
          case s: Synthesis    => Some(synthesisToQueryCtx(s))
          case u: UserMacro =>
            val dispatched =
              List(dispatch(u.body), Some(usermacroToQueryCtx(u))).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString(s"${newline()}"))
            } else {
              None
            }
          case u: UserSort => Some(usersortToQueryCtx(u))
          case m: Module   => Some(moduleToQueryCtx(m))
          case a: Application =>
            val dispatched =
              (List(a.function) ++ a.args).map(a => dispatch(a)).flatten
            if (dispatched.length > 0) {
              Some(dispatched.mkString(s"${newline()}"))
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
        val ctr = termgraph.getStmt(ct).asInstanceOf[Constructor]
        tmp ++= s"(${ctr.name}"
        indent += 1
        ctr.selectors.foreach { s =>
          tmp ++= s"${newline()}"
          val sel = termgraph.getStmt(s).asInstanceOf[Selector]
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
        if (termgraph.isSynthesisQuery()) {
          throw new SemanticError(
            "Uninterpreted functions are not supported for synthesis"
          )
        }
        tmp ++= s"${TAB * indent}(declare-fun ${u.name} (${u.params
          .map(p => s"${programPointToQueryTerm(p, indent)}")
          .mkString(" ")}) "
      } else {
        if (termgraph.isSynthesisQuery()) {
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
          val fp = termgraph.getStmt(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)}${newline()}"
      indent += 1
      tmp ++= s"${TAB * indent}${programPointToQueryTerm(u.body, indent)})"
      indent -= 1

      tmp.toString()
    }

    def synthesisToQueryCtx(u: Synthesis): String = {
      if (!termgraph.isSynthesisQuery()) {
        throw new SemanticError("Must be a synthesis query!")
      }
      val tmp = new StringBuilder()
      tmp ++= s"${TAB * indent}(synth-fun ${u.name} (${u.params
        .map { p =>
          val fp = termgraph.getStmt(p).asInstanceOf[FunctionParameter]
          s"(${fp.name} ${programPointToQueryTerm(fp.sort, indent)})"
        }
        .mkString(" ")}) "

      tmp ++= s"${programPointToQueryTerm(u.sort, indent)})${newline()}"

      tmp.toString()
    }

    def usersortToQueryCtx(u: UserSort): String =
      s"${TAB * indent}(declare-sort ${u.name} ${u.arity.value})"

    def moduleToQueryCtx(m: Module): String = {
      val tmp = new StringBuilder()
      val ctr = termgraph.getStmt(m.ct).asInstanceOf[Constructor]
      if (prettyPrint > 0) {tmp ++= s"${TAB * indent}; declaring module ${m.name}\n"}
      indent += 1
      tmp ++= s"${TAB * indent}(declare-datatypes ((${m.name} 0)) (((${ctr.name}"
      indent += 1
      ctr.selectors.foreach { s =>
        tmp ++= s"${newline()}"
        val sel = termgraph.getStmt(s).asInstanceOf[Selector]
        tmp ++= s"${TAB * indent}(${sel.name} ${programPointToQueryTerm(sel.sort, indent)})"
      }
      tmp ++= s"))))${newline()}"
      indent -= 1
      val init = dispatch(m.init)
      val next = dispatch(m.next)
      val spec = dispatch(m.spec)

      tmp ++= List(init, next, spec).flatten.mkString(s"${newline()}")
      indent -= 1
      if (prettyPrint > 0) {tmp ++= s"${TAB * indent}\n; done declaring module ${m.name}\n"}

      tmp.toString()
    }

    termgraph.getStmts().zipWithIndex
      .map(p => dispatch(p._2))
      .flatten
      .mkString(s"${newline()}")
  }
}
