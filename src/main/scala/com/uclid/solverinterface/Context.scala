package com.uclid.solverinterface

import com.uclid.uclidlanguage.parser._

import com.uclid.termgraph._

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.immutable.Nil

class Context(val termgraph: TermGraph) {

  val proofStates: ListBuffer[Int] = new ListBuffer()

  // all types are global
  val typeMap: HashMap[Identifier, Int] = new HashMap()
  // all functions, synthesis functions, and macros are global
  val globalsMap: HashMap[Identifier, Int] = new HashMap()
  // let statements stack
  val letsMapStack: Stack[HashMap[Identifier, Int]] = new Stack()

  val assertionRefs = new ListBuffer[Int]()
  val axiomRefs = new ListBuffer[Int]()

  private def addAssertion(ass: Int): Unit =
    assertionRefs.addOne(ass)

  private def addAxiom(ax: Int): Unit =
    axiomRefs.addOne(ax)

  // encode a type use (adds to program if type not yet used)
  // and return a pointer to the type
  private def typeUseToSortRef(
    t: Type
  ): Int =
    t match {
      case BooleanType() => termgraph.memoAddInstruction(TheorySort("Bool"))
      case IntegerType() => termgraph.memoAddInstruction(TheorySort("Int"))
      case ArrayType(inType, outType) => {
        val args =
          (List(inType, outType)).map(arg => typeUseToSortRef(arg))
        termgraph.memoAddInstruction(TheorySort("Array", args))
      }
      case NamedType(id) =>
        typeMap
          .getOrElse(id, throw new SemanticError(t.toString))
      case _ =>
        throw new SemanticError(t.toString)
    }

  private def getTypeFromExpr(stateParam: Int, exp: Expr): Type = {
    val termRef = exprToTerm(Some(stateParam), Map.empty, exp)._1
    val typeRef = termgraph.inferTermType(termRef)
    sortToType(typeRef)
  }

  private def sortToType(sortRef: Int): InlineType =
    termgraph.stmts(sortRef) match {
      case DataType(name, _)        => NamedType(Identifier(name))
      case Module(name, _, _, _, _) => NamedType(Identifier(name))
      case TheorySort(name, params) =>
        name match {
          case "Int"  => IntegerType()
          case "Bool" => BooleanType()
          case "Array" => {
            val paramTypes = params.map(p => sortToType(p))
            ArrayType(paramTypes(0), paramTypes(1))
          }
        }
      case UserSort(name, _) => NamedType(Identifier(name))
      case _ =>
        throw new IllegalArgumentException("sort to type must get a sort ref!")
    }

  // encode a term and return a pointer to the start of the term
  private def exprToTerm(
    stateParam: Option[Int],
    local: Map[Identifier, Int],
    expr: Expr
  ): (Int, List[Int]) = {
    var newNondets: List[Int] = List.empty
    expr match {
      case id: Identifier => {
        // try locals first
        (
          local.getOrElse(
            id,
            // if not in locals, then try state
            stateParam match {
              case Some(state) => {
                val ctrRef = termgraph.stmts(
                  termgraph.inferTermType(state)
                ).asInstanceOf[AbstractDataType]
                  .defaultCtr()
                val selectorsZip = termgraph.stmts(ctrRef)
                  .asInstanceOf[Constructor]
                  .selectors
                  .map(p => (p, termgraph.stmts(p).asInstanceOf[Selector]))
                val selRef =
                  selectorsZip.find(p => p._2.name == id.name) match {
                    case Some(value) => value._1
                    // if not in state, then check lets and then in globals
                    case None => {
                      val ret = letsMapStack.find(m => m.contains(id)) match {
                        case Some(foundMap) => foundMap(id)
                        case None =>
                          globalsMap
                            .getOrElse(id, throw new SemanticError(id.toString))
                      }
                      return (ret, newNondets)
                    }
                  }
                // return application of selector to state
                termgraph.memoAddInstruction(Application(selRef, List(state)))
              }
              // if state doesn't exist, then check lets and then globals
              case None => {
                letsMapStack.find(m => m.contains(id)) match {
                  case Some(foundMap) => foundMap(id)
                  case None =>
                    globalsMap.getOrElse(id, throw new SemanticError(id.toString))
                }
              }
            }
          ),
          newNondets
        )
      }
      case FreshLit(typ) => {
        val sortRef = typeUseToSortRef(typ)
        newNondets = newNondets.appended(
          termgraph.memoAddInstruction(
            FunctionParameter(termgraph.freshSymbolName(), sortRef)
          )
        )
        (newNondets.last, newNondets)
      }
      case l: Literal => {
        (termgraph.memoAddInstruction(TheoryMacro(l.value().toString())), newNondets)
      }
      case OperatorApplication(GetNextValueOp(), _) => {
        throw new SemanticError(expr.toString)
      }

      case OperatorApplication(PolymorphicSelect(id), exp :: Nil) => {
        val opRef = {
          val exprCtrRef = {
            val res = exprToTerm(stateParam, local, exp)
            termgraph.stmts(
              termgraph.inferTermType(res._1)
            ).asInstanceOf[AbstractDataType]
              .defaultCtr()
          }
          val ctr = termgraph.stmts(exprCtrRef).asInstanceOf[Constructor]
          ctr.selectors
            .find { s =>
              val sel = termgraph.stmts(s).asInstanceOf[Selector]
              sel.name == id.name
            }
            .getOrElse(throw new SemanticError(id.toString))
        }

        val expRef = {
          val res = exprToTerm(stateParam, local, exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = termgraph.memoAddInstruction(Application(opRef, List(expRef)))

        (appRef, newNondets)
      }

      case OperatorApplication(ConstArray(typ), exp :: Nil) => {
        // BTW: Z3 can handle a non constant argument to the as const function, but CVC4 can't (Dec. 14, 2020)
        val expRef = {
          val res = exprToTerm(stateParam, local, exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val asConstAppRef = {
          val asConstRef = termgraph.memoAddInstruction(TheoryMacro("as const"))
          val typeRef = typeUseToSortRef(typ)
          termgraph.memoAddInstruction(Application(asConstRef, List(typeRef)))
        }

        val appRef = termgraph.memoAddInstruction(
          Application(asConstAppRef, List(expRef))
        )

        (appRef, newNondets)
      }

      case OperatorApplication(ForallOp(ids), operand :: Nil) => {
        val locals = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = termgraph.memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          (p._1, selRef)
        }

        val opRef = termgraph.memoAddInstruction(
          TheoryMacro("forall", locals.map(p => p._2))
        )

        val bodyRef = {
          val res = exprToTerm(stateParam, locals.toMap ++ local, operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = termgraph.memoAddInstruction(Application(opRef, List(bodyRef)))

        (resultRef, newNondets)

      }
      case OperatorApplication(ExistsOp(ids), operand :: Nil) => {
        val locals = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = termgraph.memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          (p._1, selRef)
        }

        val opRef = termgraph.memoAddInstruction(
          TheoryMacro("exists", locals.map(p => p._2))
        )

        val bodyRef = {
          val res = exprToTerm(stateParam, locals.toMap ++ local, operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = termgraph.memoAddInstruction(Application(opRef, List(bodyRef)))

        (resultRef, newNondets)

      }
      case OperatorApplication(op, operands) => {
        val opRef = termgraph.memoAddInstruction(TheoryMacro(op.name))

        val operandRefs = new ListBuffer[Int]()
        operands.foreach { x =>
          val loc = exprToTerm(stateParam, local, x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = termgraph.memoAddInstruction(Application(opRef, operandRefs.toList))

        (appRef, newNondets)
      }
      case FunctionApplication(op, operands) => {
        val opRef = {
          val res = exprToTerm(stateParam, local, op)
          newNondets = newNondets ++ res._2
          res._1
        }

        val operandRefs = new ListBuffer[Int]()
        operands.foreach { x =>
          val loc = exprToTerm(stateParam, local, x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = termgraph.memoAddInstruction(Application(opRef, operandRefs.toList))

        (appRef, newNondets)
      }

      case ModuleNextCallExpr(expr) => {
        // get the instance
        val instanceRef = {
          val res = exprToTerm(stateParam, local, expr)
          newNondets = newNondets ++ res._2
          res._1
        }
        // get the module it belongs to
        val mod = termgraph.stmts(termgraph.inferTermType(instanceRef))
          .asInstanceOf[Module]
        // find next function location
        val nextMacro =
          termgraph.stmts(mod.next).asInstanceOf[UserMacro]

        val extraArgs = nextMacro.params.tail

        val nextCallRef = termgraph.memoAddInstruction(
          Application(mod.next, List(instanceRef) ++ extraArgs)
        )

        (nextCallRef, newNondets ++ extraArgs)
      }

      case ModuleInitCallExpr(expr) => {
        // get the instance
        val instanceRef = {
          val res = exprToTerm(stateParam, local, expr)
          newNondets = newNondets ++ res._2
          res._1
        }
        // get the module it belongs to
        val mod = termgraph.stmts(termgraph.inferTermType(instanceRef))
          .asInstanceOf[Module]
        // find init function location
        val initMacro =
          termgraph.stmts(mod.init).asInstanceOf[UserMacro]

        val extraArgs = initMacro.params.tail

        val initCallRef = termgraph.memoAddInstruction(
          Application(mod.init, List(instanceRef) ++ extraArgs)
        )

        (initCallRef, newNondets ++ extraArgs)
      }
    } // end helper exprToTerm
  }

  // statements are encoded as functions.
  private def assignToMacro(
    stateParam: Int,
    ctrRef: Int, // this is the constructor to build the target object
    stmt: AssignStmt
  ): (Int, List[Int]) = {
    var newParams: List[Int] = List.empty
    (
      {
        val lhs = stmt.lhs
        val rhs = stmt.rhs

        lhs match {
          case OperatorApplication(GetNextValueOp(), expr :: Nil) => {
            // TODO: handle primes correctly (right now we just ignore them)
            val res = assignToMacro(
              stateParam,
              ctrRef,
              AssignStmt(expr, rhs)
            )
            newParams ++= res._2
            res._1
          }
          case id: Identifier => {
            // get the constructor
            val ctr = termgraph.stmts(ctrRef)
              .asInstanceOf[Constructor]

            var found = false
            val components: List[Int] = ctr.selectors.map { s =>
              val sel = termgraph.stmts(s).asInstanceOf[Selector]
              if (id.name == sel.name) {
                found = true
                val res = exprToTerm(Some(stateParam), Map.empty, rhs)
                newParams ++= res._2
                res._1
              } else {
                exprToTerm(Some(stateParam), Map.empty, Identifier(sel.name))._1
              }
            }
            if (!found) {
              throw new SemanticError(id.toString)
            }

            val bodyRef = termgraph.memoAddInstruction(Application(ctrRef, components))

            val macroRef = termgraph.memoAddInstruction(
              UserMacro(
                // line number, column number, ast id
                s"line${lhs.pos.line}col${lhs.pos.column}!${stmt.astNodeId}",
                ctr.sort,
                bodyRef,
                List(stateParam) ++ newParams
              )
            )

            macroRef
          }
          case OperatorApplication(
              PolymorphicSelect(field),
              expr :: Nil
              ) => {

            // we have an assignment like: expr.field = rhs
            // and we want to turn it into: expr = ctr(field = rhs, expr.x for x in fields of expr datatype)

            // first get the constructor for the type of expr
            val exprCtrRef = {
              val res = exprToTerm(Some(stateParam), Map.empty, expr)
              newParams ++= res._2
              termgraph.stmts(
                termgraph.inferTermType(res._1)
              ).asInstanceOf[AbstractDataType]
                .defaultCtr()
            }

            val exprCtr =
              termgraph.stmts(exprCtrRef).asInstanceOf[Constructor]

            // now get the components
            val components: List[Expr] = exprCtr.selectors.map { s =>
              val sel = termgraph.stmts(s).asInstanceOf[Selector]
              if (field.name == sel.name) {
                rhs
              } else {
                // select from the expression
                OperatorApplication(
                  PolymorphicSelect(Identifier(sel.name)),
                  List(expr)
                )
              }
            }

            val newRhs =
              FunctionApplication(Identifier(exprCtr.name), components)

            val res = assignToMacro(
              stateParam,
              ctrRef,
              AssignStmt(expr, newRhs)
            )
            newParams ++= res._2
            res._1
          }
          case OperatorApplication(ArraySelect(), expr :: index :: Nil) => {
            val newRhs =
              OperatorApplication(ArrayUpdate(), List(expr, index, rhs))
            val res = assignToMacro(
              stateParam,
              ctrRef,
              AssignStmt(expr, newRhs)
            )
            newParams ++= res._2
            res._1
          }
          case e =>
            throw new SemanticError(e.toString)
        }
      },
      newParams
    )
  }

  private def blockToMacro(
    stateParam: Int,
    ctrRef: Int, // current module constructor
    block: BlockStmt
  ): (Int, List[Int]) = {
    letsMapStack.push(new HashMap())
    var newParams: List[Int] = List.empty
    var mostRecentParams: List[Int] = List.empty

    val bodyRef = if (block.stmts.length > 0) {

      val firstRes = stmtToMacro(stateParam, ctrRef, block.stmts.head)
      newParams ++= firstRes._2
      mostRecentParams = firstRes._2

      val firstFuncRef = firstRes._1

      if (block.stmts.length == 1) {
        return firstRes
      } else {
        val startRef = termgraph.memoAddInstruction(
          Application(firstFuncRef, List(stateParam) ++ mostRecentParams)
        )

        block.stmts.tail.foldLeft(startRef) { (acc, stmt) =>
          val funcRef = {
            val res = stmtToMacro(stateParam, ctrRef, stmt)
            newParams ++= res._2
            mostRecentParams = res._2
            res._1
          }
          // add the nondet parameters at the end
          val appRef = termgraph.memoAddInstruction(
            Application(funcRef, List(acc) ++ mostRecentParams)
          )
          appRef
        }
      }

    } else {
      stateParam
    }

    val blockRef = termgraph.memoAddInstruction(
      UserMacro(
        s"line${block.pos.line}col${block.pos.column}!${block.astNodeId}",
        termgraph.stmts(ctrRef).asInstanceOf[Constructor].sort,
        bodyRef,
        List(stateParam) ++ newParams
      )
    )

    letsMapStack.pop()
    (blockRef, newParams)
  }

  private def ifelseToMacro(
    stateParam: Int,
    ctrRef: Int, // current module constructor
    ifelse: IfElseStmt
  ): (Int, List[Int]) = {
    val left = stmtToMacro(stateParam, ctrRef, ifelse.ifblock)
    val leftAppRef = termgraph.memoAddInstruction(
      Application(left._1, List(stateParam) ++ left._2)
    )

    val right = stmtToMacro(stateParam, ctrRef, ifelse.elseblock)
    val rightAppRef = termgraph.memoAddInstruction(
      Application(right._1, List(stateParam) ++ right._2)
    )

    val cond = exprToTerm(Some(stateParam), Map.empty, ifelse.cond)

    val iteRef = termgraph.memoAddInstruction(TheoryMacro("ite"))

    val bodyRef = termgraph.memoAddInstruction(
      Application(iteRef, List(cond._1, leftAppRef, rightAppRef))
    )

    val macroRef = termgraph.memoAddInstruction(
      UserMacro(
        s"line${ifelse.pos.line}col${ifelse.pos.column}!${ifelse.astNodeId}",
        termgraph.stmts(ctrRef).asInstanceOf[Constructor].sort,
        bodyRef,
        List(stateParam) ++ cond._2 ++ left._2 ++ right._2
      )
    )

    (macroRef, cond._2 ++ left._2 ++ right._2)
  }

  private def stmtToMacro(stateParam: Int, ctrRef: Int, stmt: Statement): (
    Int,
    List[Int]
  ) = // returns a pointer to the macro and the extra args you need for it
    stmt match {
      case a: AssignStmt => assignToMacro(stateParam, ctrRef, a)
      case b: BlockStmt  => blockToMacro(stateParam, ctrRef, b)
      case i: IfElseStmt => ifelseToMacro(stateParam, ctrRef, i)
      case c: CaseStmt => {
        val nested = c.body.reverse.foldLeft(
          BlockStmt(List.empty): Statement
        )((acc, f) =>
          IfElseStmt(f._1, BlockStmt(List(f._2)), BlockStmt(List(acc)))
        )
        stmtToMacro(stateParam, ctrRef, nested)
      }
      case h: HavocStmt => {
        assignToMacro(
          stateParam,
          ctrRef,
          AssignStmt(
            h.toHavoc,
            FreshLit(getTypeFromExpr(stateParam, h.toHavoc))
          )
        )
      }
      case n: ModuleNextCallStmt => {
        assignToMacro(
          stateParam,
          ctrRef,
          AssignStmt(n.expr, ModuleNextCallExpr(n.expr))
        )
      }
      case l: LetStatement => {

        val modRef = termgraph.stmts(stateParam).asInstanceOf[FunctionParameter].sort
        val selRefs = termgraph.stmts(termgraph.stmts(modRef).asInstanceOf[Module].ct)
          .asInstanceOf[Constructor]
          .selectors
        if (selRefs.exists { s =>
              termgraph.stmts(s).asInstanceOf[Selector].name == l.id.name
            }) {
          throw new SemanticError(
            s"Let statements cannot override state variables!\n\n${l.id.pos.longString}"
          )
        }

        val r = exprToTerm(Some(stateParam), Map.empty, l.expr)
        letsMapStack.top.addOne((l.id, r._1))
        // identity function
        val macroRef = termgraph.memoAddInstruction(
          UserMacro(
            // line number, column number, ast id
            s"line${l.pos.line}col${l.pos.column}!${stmt.astNodeId}",
            modRef,
            stateParam,
            List(stateParam)
          )
        )
        (macroRef, List.empty)
      }
    }

  // encode a transition block and return a pointer to the function definition
  private def transitionToTerm(
    funcName: String,
    stateParam: Int,
    ctrRef: Int,
    block: BlockStmt
  ): (Int, List[Int]) = {
    val res = stmtToMacro(
      stateParam,
      ctrRef,
      block
    )

    val um = termgraph.stmts(res._1).asInstanceOf[UserMacro]

    termgraph.memoUpdateInstruction(
      res._1,
      UserMacro(funcName, um.sort, um.body, um.params)
    )

    res
  }

  private def getAxiomRef(stateParam: Int, axioms: List[InnerAxiom]): Int = {
    // make sure invariant axioms hold on the fresh instance
    val specConjuncts = new ListBuffer[Int]()
    val axiomRef = if (axioms.length > 1) {
      val andRef = termgraph.memoAddInstruction(TheoryMacro("and"))
      axioms.foreach { d =>
        val t = exprToTerm(
          Some(stateParam),
          Map.empty,
          d.expr
        )._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      termgraph.memoAddInstruction(Application(andRef, specConjuncts.toList))
    } else if (axioms.length == 1) {
      exprToTerm(Some(stateParam), Map.empty, axioms(0).expr)._1 //specs shouldn't create new nondets
    } else {
      val trueRef = termgraph.memoAddInstruction(TheoryMacro("true"))
      trueRef
    }
    axiomRef
  }

  private def executeControl(
    moduleId: Identifier,
    initParams: List[Int],
    nextParams: List[Int],
    cmds: List[Command],
    axioms: List[InnerAxiom]
  ): Unit = {

    def generateInitVariables(): List[Int] =
      initParams.map { p =>
        termgraph.stmts(p) match {
          case FunctionParameter(_, sort) => {
            val vRef =
              termgraph.addInstruction(UserFunction(freshSymbolName(), sort))
            vRef
          }
        }
      }

    def stepKTimes(k: Int, startRef: Int, nextRef: Int): List[Int] = {
      val steps: ListBuffer[Int] = new ListBuffer()
      steps.addOne(startRef)
      (1 to k).foreach { i =>
        val args = nextParams.tail.map { p =>
          termgraph.stmts(p) match {
            case FunctionParameter(name, sort) => {
              val vRef =
                termgraph.memoAddInstruction(
                  UserFunction(s"$name!step!$i", sort)
                )
              vRef
            }
          }
        }
        steps.addOne(
          termgraph.memoAddInstruction(
            Application(nextRef, List(steps.last) ++ args),
            Some(s"State_At_Step!$i")
          )
        )
      }
      steps.tail.toList
    }

    cmds.foreach(p =>
      p match {
        case c: SolverCommand =>
          c match {
            case Check() => checkQuery = true
            case GetValue(vars) => {
              options = options.appended(("produce-models", "true"))
              val vs = proofStates.foldLeft(List.empty: List[Int]) { (acc, s) =>
                acc ++ vars.map(p => exprToTerm(Some(s), Map.empty, p)._1)
              }
              getValues = Some(vs)
            }
            case Trace(unwind, init, start) => {
              // get the module declaration
              val modRef = typeMap(moduleId)
              val mod = termgraph.stmts(modRef)
                .asInstanceOf[Module]
              val initRef = mod.init
              val nextRef = mod.next

              // TODO: currently ignores axioms
              val preInit = transitionToTerm(
                mod.name + "!pre_init",
                initParams.head,
                mod.ct,
                start
              )
              val fuzzed = termgraph.fuzz(modRef)
              val startTerm = termgraph.memoAddInstruction(
                Application(preInit._1, fuzzed :: preInit._2.map { p =>
                  termgraph.stmts(p) match {
                    case FunctionParameter(_, sort) => {
                      termgraph.fuzz(sort)
                    }
                  }
                })
              )

              val initVariables = startTerm :: initParams.tail.map { p =>
                termgraph.stmts(p) match {
                  case FunctionParameter(_, sort) => {
                    termgraph.fuzz(sort)
                  }
                }
              }

              var transRef = if (init.literal) {
                // apply init
                val initAppRef =
                  termgraph.memoAddInstruction(
                    Application(initRef, initVariables),
                    Some("State_After_Init")
                  )
                proofStates.addOne(initAppRef)
                initAppRef
              } else {
                startTerm
              }

              // Take k steps but fuzz at each step
              val k = unwind.literal.toInt
              (1 to k).foreach { i =>
                val args = nextParams.tail.map { p =>
                  termgraph.stmts(p) match {
                    case FunctionParameter(_, sort) => {
                      termgraph.fuzz(sort)
                    }
                  }
                }
                transRef = termgraph.memoAddInstruction(
                  Application(nextRef, List(transRef) ++ args),
                  Some(s"State_At_Step!$i")
                )
                proofStates.addOne(transRef)
              }
              traceQuery = true
            }
          }
        case SolverOption(name, option) =>
          options = options.appended((name, option))
        case ProofCommand(name, unwind) => {
          name.name match {
            case "induction" => {
              // create all the variables you need
              val baseInitVariables = generateInitVariables()
              if (axioms.length > 0) {
                // all axioms should hold before base case
                val axiomRef = getAxiomRef(baseInitVariables.head, axioms)
                addAxiom(axiomRef)
              }

              // get the module declaration
              val mod = termgraph.stmts(typeMap(moduleId))
                .asInstanceOf[Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              // base case
              // apply init
              val initAppRef =
                termgraph.memoAddInstruction(
                  Application(initRef, baseInitVariables),
                  Some("State_After_Init")
                )
              proofStates.addOne(initAppRef)

              // apply spec to result of init
              val initSpecRef =
                termgraph.memoAddInstruction(Application(specRef, List(initAppRef)))

              val negRef = termgraph.memoAddInstruction(TheoryMacro("not"))

              val baseRef =
                termgraph.memoAddInstruction(
                  Application(negRef, List(initSpecRef)),
                  Some("Counterexample_In_Invariants_BaseCase")
                )
              addAssertion(baseRef)

              // induction step
              // holds on entry
              // create all the variables you need
              val inductiveInitVariables = generateInitVariables()
              val inductiveAxioms = axioms.filter(p => p.invariant)
              if (inductiveAxioms.length > 0) {
                // all inductiveAxioms should hold before inductive case
                val axiomRef =
                  getAxiomRef(inductiveInitVariables.head, inductiveAxioms)
                addAxiom(axiomRef)
              }
              proofStates.addOne(inductiveInitVariables.head)
              val entryRef =
                termgraph.memoAddInstruction(
                  Application(specRef, List(inductiveInitVariables.head))
                )
              // we can borrow the nondet state from init since we pop between asserts (this lets us just generate the auxiliary arguments when applying next)

              // Take k steps
              val k = unwind.getOrElse(IntLit(1)).literal.toInt
              val states = stepKTimes(k, inductiveInitVariables.head, nextRef)
              proofStates.addAll(states)
              if (inductiveAxioms.length > 0) {
                // inductiveAxioms hold on every inner step
                states.foreach { s =>
                  val axiomRef = getAxiomRef(s, inductiveAxioms)
                  addAxiom(axiomRef)
                }
              }

              // holds on exit
              val exitRef =
                termgraph.memoAddInstruction(Application(specRef, List(states.last)))

              val negExitRef =
                termgraph.memoAddInstruction(Application(negRef, List(exitRef)))

              val andRef = termgraph.memoAddInstruction(TheoryMacro("and"))

              val inductiveRef = termgraph.memoAddInstruction(
                Application(andRef, List(entryRef, negExitRef)),
                Some("Counterexample_In_Invariants_Induction_Step")
              )

              addAssertion(inductiveRef)

            }
            case "unroll" => {
              // create all the variables you need
              val initVariables = generateInitVariables()

              // get the module declaration
              val mod = termgraph.stmts(typeMap(moduleId))
                .asInstanceOf[Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              val initAppRef =
                termgraph.memoAddInstruction(
                  Application(initRef, initVariables),
                  Some("State_After_Init")
                )
              proofStates.addOne(initAppRef)

              // apply axiom after init so that fresh variables don't cause problems when unrolling
              if (axioms.length > 0) {
                val axiomRef = getAxiomRef(initAppRef, axioms)
                addAxiom(axiomRef)
              }

              val negRef = termgraph.memoAddInstruction(TheoryMacro("not"))

              // Take k steps
              val k = unwind.getOrElse(IntLit(1)).literal.toInt
              val states = initAppRef :: stepKTimes(k, initAppRef, nextRef)
              proofStates.addAll(states)
              val inductiveAxioms = axioms.filter(p => p.invariant)
              if (inductiveAxioms.length > 0) {
                // inductiveAxioms hold on every inner step
                states.foreach { s =>
                  val axiomRef = getAxiomRef(s, inductiveAxioms)
                  addAxiom(axiomRef)
                }
              }

              states.zipWithIndex.foreach { p =>
                val exitRef =
                  termgraph.memoAddInstruction(Application(specRef, List(p._1)))
                val negExitRef =
                  termgraph.memoAddInstruction(
                    Application(negRef, List(exitRef)),
                    Some(s"Counterexample_In_Step!${p._2}")
                  )
                addAssertion(negExitRef)
              }
            }
          }
        }
      }
    )
  }

  // get all the specs and create a function from them
  private def specsToTerm(
    funcName: String,
    stateParam: Int,
    params: List[Int],
    properties: List[SpecDecl]
  ): Int = {
    // spec needs Bool, so add Bool if it's not already there
    val boolRef = termgraph.memoAddInstruction(TheorySort("Bool"))

    val specConjuncts = new ListBuffer[Int]()

    val bodyRef = if (properties.length > 1) {
      val andRef = termgraph.memoAddInstruction(TheoryMacro("and"))
      properties.foreach { d =>
        val t = exprToTerm(Some(stateParam), Map.empty, d.expr)._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      termgraph.memoAddInstruction(Application(andRef, specConjuncts.toList))
    } else if (properties.length == 1) {
      exprToTerm(Some(stateParam), Map.empty, properties(0).expr)._1 //specs shouldn't create new nondets
    } else {
      val trueRef = termgraph.memoAddInstruction(TheoryMacro("true"))
      trueRef
    }

    val specRef = termgraph.memoAddInstruction(
      UserMacro(
        funcName,
        boolRef,
        bodyRef,
        params
      )
    )

    specRef
  } // end helper specsToTerm

  // for every variable, if it contains first class modules then make sure they are inited.
  private def createInitAssumes(
    fields: List[
      (Expr, InlineType)
    ] // variable we want to init and its type
  ): List[InnerAxiom] =
    fields.foldLeft(List.empty: List[InnerAxiom]) { (acc, f) =>
      f._2 match {
        case BooleanType() => acc
        case IntegerType() => acc
        case ArrayType(inType, outType) => {
          val idx = Identifier(freshSymbolName())
          val select = OperatorApplication(ArraySelect(), List(f._1, idx))
          createInitAssumes(List((select, outType))) match {
            case Nil => acc
            case head :: Nil => {
              val innerExpr = head.expr
              val forall =
                OperatorApplication(
                  ForallOp(List((idx, inType))),
                  List(innerExpr)
                )
              acc ++ List(
                InnerAxiom(
                  Identifier(s"init_axiom_${f._1.pos.line}_${f._1.pos.column}"),
                  forall,
                  false
                )
              )
            }
            case _ =>
              throw new SemanticError(
                "createInitAssumes on arrays should not return a list of length greater than one! Please contact developers."
              )
          }
        }
        case NamedType(id) => {
          val sortRef = typeMap(id)
          termgraph.stmts(sortRef) match {
            case _: Module =>
              acc ++ List(
                InnerAxiom(
                  Identifier(s"init_axiom_${f._1.pos.line}_${f._1.pos.column}"),
                  OperatorApplication(
                    EqualityOp(),
                    List(f._1, ModuleInitCallExpr(f._1))
                  ),
                  false
                )
              )
            case adt: AbstractDataType => {
              val ctr =
                termgraph.stmts(adt.defaultCtr()).asInstanceOf[Constructor]

              val components = ctr.selectors.map { s =>
                val sel = termgraph.stmts(s).asInstanceOf[Selector]
                val newStarter =
                  OperatorApplication(
                    PolymorphicSelect(Identifier(sel.name)),
                    List(f._1)
                  )
                val typ = sortToType(sel.sort)
                (newStarter, typ)
              }
              acc ++ createInitAssumes(components)
            }
            case _ => acc
          }
        }
      }
    }

  def typeDeclToTerm(td: TypeDecl): Unit =
    td.typ match {
      case None =>
        typeMap.get(td.id) match {
          case Some(_) => throw new SemanticError(td.toString)
          case None => {
            val uRef = termgraph.memoAddInstruction(UserSort(td.id.name))
            typeMap.addOne(
              td.id,
              uRef
            )
          }
        }
      case Some(EnumType(variants)) => {
        typeMap.get(td.id) match {
          case Some(_) => throw new SemanticError(td.toString)
          case None => {
            // add datatype placeholder
            val dtRef = termgraph.addInstruction(DataType(td.id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val cRef =
                termgraph.memoAddInstruction(Constructor(i.name, dtRef, List.empty))
              globalsMap.addOne(i, cRef)
              cRef
            }
            termgraph.memoUpdateInstruction(dtRef, DataType(td.id.name, constructors))
            typeMap.addOne(td.id, dtRef)
          }
        }
      }
      case Some(RecordType(elements)) => {
        typeMap.get(td.id) match {
          case Some(_) => throw new SemanticError(td.toString)
          case None => {
            // add datatype placeholder
            val dtRef = termgraph.addInstruction(DataType(td.id.name, List.empty))

            val selecorRefs = elements.map { p =>
              val tyepRef = typeUseToSortRef(p._2)
              termgraph.memoAddInstruction(Selector(p._1.name, tyepRef))
            }

            val ctrRef = termgraph.memoAddInstruction(
              Constructor(td.id.name, dtRef, selecorRefs)
            )
            globalsMap.addOne(td.id, ctrRef)

            termgraph.memoUpdateInstruction(dtRef, DataType(td.id.name, List(ctrRef)))

            typeMap.addOne(td.id, dtRef)
          }
        }
      }
      case Some(_) => {
        // assign td.id (lhs) to whatever id is pointing to
        typeMap.get(td.id) match {
          case Some(_) =>
            throw new SemanticError(td.toString)
          case None =>
            typeMap.addOne(
              td.id,
              typeUseToSortRef(td.typ.get)
            )
        }
      }
    }

  def functionDeclToTerm(fd: FunctionDecl): Unit = {
    val typeRefs =
      (List(fd.retTyp) ++ fd.argTypes).map(t => typeUseToSortRef(t))
    globalsMap.addOne(
      fd.id,
      termgraph.memoAddInstruction(
        UserFunction(fd.id.name, typeRefs.head, typeRefs.tail)
      )
    )
  }

  def axiomToAssertion(ax: OuterAxiom): Unit = {
    val bodyRef = exprToTerm(None, Map.empty, ax.expr)._1
    addAxiom(bodyRef)
  }

  def defineDeclToTerm(dd: DefineDecl): Unit = {
    val params = dd.params.map { a =>
      val typeRef = typeUseToSortRef(a._2)
      val selRef = termgraph.memoAddInstruction(FunctionParameter(a._1.name, typeRef))
      (a._1, selRef)
    }
    val typeRef = typeUseToSortRef(dd.retTyp)
    val bodyRef = exprToTerm(None, params.toMap, dd.expr)._1 // defines cannot create new variables
    globalsMap.addOne(
      dd.id,
      termgraph.memoAddInstruction(
        UserMacro(dd.id.name, typeRef, bodyRef, params.map(p => p._2))
      )
    )
  }

  def synthesisDeclToTerm(sy: SynthesisDecl): Unit = {
    val params = sy.params.map { a =>
      val typeRef = typeUseToSortRef(a._2)
      val selRef = termgraph.memoAddInstruction(FunctionParameter(a._1.name, typeRef))
      (a._1, selRef)
    }
    val typeRef = typeUseToSortRef(sy.retTyp)
    globalsMap.addOne(
      sy.id,
      termgraph.memoAddInstruction(
        Synthesis(sy.id.name, typeRef, params.map(p => p._2))
      )
    )
  }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    m: ModuleDecl,
    execute: Boolean
  ): Unit = {

    // add placeholder for module and remember where it is
    val moduleRef = termgraph.addInstruction(
      Module(
        m.id.name,
        -1,
        -1,
        -1,
        -1
      )
    )

    typeMap.get(m.id) match {
      case Some(_) => throw new SemanticError(m.toString)
      case None    => typeMap.addOne(m.id, moduleRef)
    }

    // input state
    val inputStateRef = termgraph.memoAddInstruction(
      FunctionParameter("State", moduleRef)
    )

    val fields =
      (m.vars ++ m.consts ++ m.sharedVars ++ m.inputs ++ m.outputs).sortWith(
        (a, b) => a._1.pos < b._1.pos
      )

    // create selectors and remember where they are
    val selectorTerms = new HashMap[String, Int]()
    val selectorRefs =
      fields.map { v =>
        val typeRef = typeUseToSortRef(v._2)
        val selRef = termgraph.memoAddInstruction(Selector(v._1.name, typeRef))

        // create application for scope
        val getterRef =
          termgraph.memoAddInstruction(Application(selRef, List(inputStateRef)))
        selectorTerms.addOne((v._1.name, getterRef))

        selRef
      }

    // add constructor and remember where it is
    val constructorRef = termgraph.memoAddInstruction(
      Constructor(m.id.name, moduleRef, selectorRefs)
    )
    globalsMap.addOne(m.id, constructorRef)

    // update module with constructor; will need to update it fully at the end
    termgraph.memoUpdateInstruction(
      moduleRef,
      Module(
        m.id.name,
        constructorRef,
        -1,
        -1,
        -1
      )
    )

    val initAssumes = createInitAssumes(fields)
    var initParams = List(inputStateRef)
    val initBlock = m.init match {
      case Some(decl) => BlockStmt(decl.body.stmts)
      case None       => BlockStmt(List.empty)
    }
    val initRef = {
      val res = transitionToTerm(
        m.id.name + "!init",
        inputStateRef,
        constructorRef,
        initBlock
      )
      initParams ++= res._2
      res._1
    }

    var nextParams = List(inputStateRef)
    val nextBlock = m.next match {
      case Some(decl) => decl.body
      case None       => BlockStmt(List.empty)
    }
    val nextRef = {
      val res = transitionToTerm(
        m.id.name + "!next",
        inputStateRef,
        constructorRef,
        nextBlock
      )
      nextParams ++= res._2
      res._1
    }

    // Add spec function
    val specRef = specsToTerm(
      m.id.name + "!spec",
      inputStateRef,
      List(inputStateRef),
      m.properties
    )

    // fill in placeholder for module
    termgraph.memoUpdateInstruction(
      moduleRef,
      Module(m.id.name, constructorRef, initRef, nextRef, specRef)
    )

    if (execute) {
      executeControl(
        m.id,
        initParams,
        nextParams,
        m.cmds,
        m.axioms ++ initAssumes
      )
    }
  } // End Module to Term


  private val TAB = "  "

  var isSynthesisQuery = false
  var checkQuery = false
  var traceQuery = false
  var getValues: Option[List[Int]] = None
  var logic: String = "ALL"

  protected def inferLogic(): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true

    val marks = termgraph.mark(assertionRefs ++ axiomRefs)

    termgraph.stmts
      .zip(marks)
      .foreach(inst =>
        if (inst._2) {
          inst._1 match {
            case _: AbstractDataType => dt = true
            case Application(caller, args) => {
              termgraph.stmts(caller) match {
                case TheoryMacro("*", _) => {
                  if (args.filter { a =>
                        termgraph.stmts(a) match {
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
      termgraph.stmts(position) match {
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
        case m: Module => moduleToQueryTerm(m)
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
      if (!isSynthesisQuery) {
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
    val toDeclare = termgraph.mark(
      assertionRefs ++ getValues.getOrElse(List.empty) ++ axiomRefs
    )

    def dispatch(position: Int): Option[String] =
      if (toDeclare(position)) {
        toDeclare.update(position, false)
        termgraph.stmts(position) match {
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
          case m: Module => Some(moduleToQueryCtx(m))
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
      tmp ++= ")))\n"
      indent -= 1

      tmp.toString()
    }

    def userfunctionToQueryCtx(u: UserFunction): String = {
      val tmp = new StringBuilder()
      if (u.params.length > 0) {
        if (isSynthesisQuery) {
          throw new SemanticError(
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
          val fp = termgraph.stmts(p).asInstanceOf[FunctionParameter]
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

  def programToQuery(): String = {
    alreadyDeclared.clear()
    logic = inferLogic()
    val logicString = s"(set-logic ${logic})"
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

    val body : String = if (spec != "") {
      if (isSynthesisQuery) {
        programToQueryCtx() + "\n(constraint (not " + spec + "))"
      } else {
        programToQueryCtx() + "\n(assert " + spec + ")"
      }
    } else {
      if (getValues.isDefined) {
        programToQueryCtx()
      } else {
        ""
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

    s"$logicString\n$opts\n\n$body\n$postQuery"
  }
}