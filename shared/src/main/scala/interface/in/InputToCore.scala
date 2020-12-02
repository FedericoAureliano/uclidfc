package interface.in

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

import middle.core
import middle.core.{
  Application,
  Constructor,
  DataType,
  FunctionParameter,
  Instruction,
  Numeral,
  Program,
  Ref,
  Selector,
  TheoryMacro,
  TheorySort,
  UserMacro,
  UserSort
}
import middle.core.rewrite
import front.{
  ArrayType,
  AssertStmt,
  AssignStmt,
  AssumeStmt,
  BitVectorType,
  BlockStmt,
  BooleanType,
  CaseStmt,
  ConstArray,
  EnumType,
  Expr,
  ExternalType,
  ForStmt,
  FuncApplication,
  HavocStmt,
  Identifier,
  IfElseStmt,
  InitDecl,
  IntegerType,
  Lhs,
  Literal,
  MapType,
  ModuleCallStmt,
  ModuleInstanceType,
  ModuleType,
  NextDecl,
  OperatorApplication,
  ProcedureCallStmt,
  ProcedureType,
  RecordType,
  SkipStmt,
  StringType,
  SynonymType,
  Tuple,
  TupleType,
  Type,
  UndefinedType,
  UninterpretedType,
  WhileStmt
}

package object Translate {

  def modelToProgram(model: List[front.Module]): Program = {
    val term = new ArrayBuffer[Instruction]()
    var offset = 0
    model.foreach { m =>
      val t = moduleToTerm(m)
      rewrite.incrementRefs(t, offset)
      term.addAll(t.stmts)
      offset += t.stmts.length
    }
    val program = new Program(term.toArray)
    program
  }

  def typeUseToTerm(t: Type): Program = {
    val term = new ArrayBuffer[Instruction]()
    t match {
      case UninterpretedType(name) => term.addOne(UserSort(name.name))
      case BooleanType()           => term.addOne(TheorySort("Bool"))
      case IntegerType()           => term.addOne(TheorySort("Int"))
      case StringType()            => term.addOne(TheorySort("String"))
      case BitVectorType(width) => {
        term.addOne(TheorySort("_ BitVec", List(Ref(1))))
        term.addOne(Numeral(width))
      }
      case _ =>
        throw new IllegalArgumentException(s"type not yet supported: ${t}")
    }
    val program = new Program(term.toArray)
    program
  }

  def varDeclToTerm(v: (Identifier, Type)): Program = {
    val term = new ArrayBuffer[Instruction]()

    term.addOne(Selector(v._1.name, Ref(1)))
    val t = typeUseToTerm(v._2)
    rewrite.incrementRefs(t, 1)
    term.addAll(t.stmts)

    val program = new Program(term.toArray)
    program
  }

  def moduleToTerm(m: front.Module): Program = {
    val term = new ArrayBuffer[Instruction]()

    val selectorRefs = new ListBuffer[Ref]()
    val selectorsTerm = new ArrayBuffer[Instruction]()

    var selectorsPos = 4 //module, constructor, datatype, boolean and then selectors
    (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs ++ m.constants).foreach {
      v =>
        val t = varDeclToTerm(v)
        rewrite.incrementRefs(t, selectorsPos)
        selectorsTerm.addAll(t.stmts)
        selectorRefs.addOne(Ref(selectorsPos))
        selectorsPos += t.stmts.length
    }

    term.addAll(selectorsTerm)
    term.prepend(TheorySort("Bool"))
    term.prepend(Constructor(m.id.name, Ref(1), selectorRefs.toList))
    term.prepend(DataType(m.id.name + "_t", List(Ref(2))))
    term.prepend(
      core.Module(m.id.name, Ref(1), Ref(-1), Ref(-1), Ref(-1))
    ) // placeholder

    def exprToTerm(
      expr: Expr,
      in: Ref,
      selectors: List[Ref],
      offset: Int
    ): Program = {
      val outBuffer = new ArrayBuffer[Instruction]()
      var termPos = offset

      expr match {
        case Identifier(name) => {
          // find selector
          val sel = selectors.filter(r =>
            term(r.loc) match {
              case Selector(n, _) => name == n
              case _              => throw new IllegalArgumentException("must be selector")
            }
          )(0)
          outBuffer.addOne(Application(sel, List(in)))
          termPos += 1 // for application
        }
        case l: Literal => {
          outBuffer.addOne(TheoryMacro(l.toString()))
          termPos += 1 // for theory macro
        }
        case OperatorApplication(op, operands) => {
          termPos += 1 // for prepending application
          val operandRefs = new ListBuffer[Ref]()

          operands.foreach { x =>
            val e = exprToTerm(x, in, selectors, termPos)
            outBuffer.addAll(e.stmts)
            operandRefs.addOne(Ref(termPos))
            termPos += e.stmts.length
            e
          }

          outBuffer.addOne(TheoryMacro(op.toString()))
          outBuffer.prepend(Application(Ref(termPos), operandRefs.toList))
          termPos += 1 // for theory macro
        }

        case _ => throw new IllegalArgumentException("not implemented yet")
      }

      val out = new Program(outBuffer.toArray)
      out
    }

    // add init, next and spec
    var initPos = 4 + selectorsTerm.length
    val init = Ref(initPos)
    val tmpInitTerm = new ArrayBuffer[Instruction]()
    val initComponents = new ListBuffer[Ref]()
    tmpInitTerm.addOne(
      UserMacro(
        m.id.name + "_i",
        Ref(1),
        Ref(init.loc + 2),
        List(Ref(init.loc + 1))
      )
    )
    tmpInitTerm.addOne(FunctionParameter("in", Ref(2)))
    tmpInitTerm.addOne(
      Application(Ref(2), initComponents.toList)
    ) // PLACEHOLDER, update later
    initPos += 3; // for prepending constructor application (+ user macro and function param above)
    selectorRefs.map(r => term(r.loc)).zipWithIndex.foreach {
      case (s: Selector, i: Int) => {
        // find the assignment
        m.init match {
          case Some(InitDecl(BlockStmt(_, stmts))) => {
            val flat: List[(Lhs, Expr)] =
              stmts.foldLeft(List[(Lhs, Expr)]())((acc, p) =>
                p match {
                  case AssignStmt(lhss, rhss) => {
                    acc ++ lhss.zip(rhss)
                  }
                  case _ =>
                    throw new IllegalArgumentException("must be an assignment")
                }
              )
            val found = flat.filter(p => p._1.ident.name == s.name)
            if (found.length > 0) {
              val e = exprToTerm(
                found(0)._2,
                Ref(init.loc + 1),
                selectorRefs.toList,
                initPos
              )
              tmpInitTerm.addAll(e.stmts)
              initComponents.addOne(Ref(initPos))
              initPos += e.stmts.length
            } else {
              tmpInitTerm
                .addOne(Application(selectorRefs(i), List(Ref(init.loc + 1))))
              initComponents.addOne(Ref(initPos))
              initPos += 1
            }
          }
          case None => {
            tmpInitTerm
              .addOne(Application(selectorRefs(i), List(Ref(init.loc + 1))))
            initComponents.addOne(Ref(initPos))
            initPos += 1
          }
          case _ => throw new IllegalArgumentException("must be a block stmt")
        }
      }
      case (_, _) =>
        throw new IllegalArgumentException("must be (s : Selector, i: Int)")
    }
    tmpInitTerm.update(
      2,
      Application(Ref(2), initComponents.toList)
    ) // apply constructor to the expressions above (FILLING-IN PLACEHOLDER)
    term.addAll(tmpInitTerm)

    var nextPos = init.loc + tmpInitTerm.length
    val next = Ref(nextPos)
    val tmpNextTerm = new ArrayBuffer[Instruction]()
    val nextComponents = new ListBuffer[Ref]()
    tmpNextTerm.addOne(
      UserMacro(
        m.id.name + "_n",
        Ref(1),
        Ref(next.loc + 2),
        List(Ref(next.loc + 1))
      )
    )
    tmpNextTerm.addOne(FunctionParameter("in", Ref(2)))
    tmpNextTerm.addOne(
      Application(Ref(2), nextComponents.toList)
    ) // PLACEHOLDER, update later
    nextPos += 3; // for prepending constructor application (+ user macro and function param above)
    selectorRefs.map(r => term(r.loc)).zipWithIndex.foreach {
      case (s: Selector, i: Int) => {
        // find the assignment
        m.next match {
          case Some(NextDecl(BlockStmt(_, stmts))) => {
            val flat: List[(Lhs, Expr)] =
              stmts.foldLeft(List[(Lhs, Expr)]())((acc, p) =>
                p match {
                  case AssignStmt(lhss, rhss) => {
                    acc ++ lhss.zip(rhss)
                  }
                  case _ =>
                    throw new IllegalArgumentException("must be an assignment")
                }
              )
            val found = flat.filter(p => p._1.ident.name == s.name)
            if (found.length > 0) {
              val e = exprToTerm(
                found(0)._2,
                Ref(next.loc + 1),
                selectorRefs.toList,
                nextPos
              )
              tmpNextTerm.addAll(e.stmts)
              nextComponents.addOne(Ref(nextPos))
              nextPos += e.stmts.length
            } else {
              tmpNextTerm
                .addOne(Application(selectorRefs(i), List(Ref(next.loc + 1))))
              nextComponents.addOne(Ref(nextPos))
              nextPos += 1
            }
          }
          case None => {
            tmpNextTerm
              .addOne(Application(selectorRefs(i), List(Ref(next.loc + 1))))
            nextComponents.addOne(Ref(nextPos))
            nextPos += 1
          }
          case _ => throw new IllegalArgumentException("must be a block stmt")
        }
      }
      case (_, _) =>
        throw new IllegalArgumentException("must be (s : Selector, i: Int)")
    }
    tmpNextTerm.update(
      2,
      Application(Ref(2), nextComponents.toList)
    ) // apply constructor to the expressions above (FILLING-IN PLACEHOLDER)
    term.addAll(tmpNextTerm)

    var specPos = next.loc + tmpNextTerm.length
    val spec = Ref(specPos)
    val tmpSpecTerm = new ListBuffer[Instruction]()
    val specComponents = new ListBuffer[Ref]()
    tmpSpecTerm.addOne(
      UserMacro(
        m.id.name + "_s",
        Ref(3),
        Ref(spec.loc + 2),
        List(Ref(spec.loc + 1))
      )
    )
    tmpSpecTerm.addOne(FunctionParameter("in", Ref(2)))
    specPos += 2
    if (m.properties.length > 1) {
      tmpSpecTerm.addOne(
        Application(Ref(spec.loc + 3), specComponents.toList)
      ) // PLACEHOLDER
      specPos += 1
      tmpSpecTerm.addOne(TheoryMacro("and"))
      specPos += 1
      m.properties.foreach { d =>
        val e =
          exprToTerm(d.expr, Ref(spec.loc + 1), selectorRefs.toList, specPos)
        specComponents.addOne(Ref(specPos))
        tmpSpecTerm.addAll(e.stmts)
        specPos += e.stmts.length
      }
      tmpSpecTerm.update(
        2,
        Application(Ref(spec.loc + 3), specComponents.toList)
      ) // UPDATING PLACEHOLDER
    } else if (m.properties.length == 1) {
      val e = exprToTerm(
        m.properties(0).expr,
        Ref(spec.loc + 1),
        selectorRefs.toList,
        spec.loc + 2
      )
      tmpSpecTerm.addAll(e.stmts)
    } else {
      tmpSpecTerm.addOne(TheoryMacro("true"))
    }
    term.addAll(tmpSpecTerm)
    term.update(0, core.Module(m.id.name, Ref(1), init, next, spec))

    val program = new Program(term.toArray)
    program
  }
}
