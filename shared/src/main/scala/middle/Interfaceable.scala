package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

import front._
import scala.collection.mutable.Stack
import scala.collection.immutable.Nil

class Interfaceable(stmts: ArrayBuffer[Instruction]) extends Fuzzable(stmts) {
  val proofStates: ListBuffer[Int] = new ListBuffer()

  // all types are global
  val typeMap: HashMap[Identifier, Int] = new HashMap()
  // all functions, synthesis functions, and macros are global
  val globalsMap: HashMap[Identifier, Int] = new HashMap()
  // let statements stack
  val letsMapStack: Stack[HashMap[Identifier, Int]] = new Stack()

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
      case BooleanType() => memoAddInstruction(TheorySort("Bool"))
      case IntegerType() => memoAddInstruction(TheorySort("Int"))
      case ArrayType(inType, outType) => {
        val args =
          (List(inType, outType)).map(arg => typeUseToSortRef(arg))
        memoAddInstruction(TheorySort("Array", args))
      }
      case NamedType(id) =>
        typeMap
          .getOrElse(id, throw new TypeOutOfScope(t))
      case _ =>
        throw new NotSupportedYet(t)
    }

  private def getTypeFromExpr(stateParam: Int, exp: Expr): Type = {
    val termRef = exprToTerm(Some(stateParam), Map.empty, exp)._1
    val typeRef = inferTermType(termRef)
    sortToType(typeRef)
  }

  private def sortToType(sortRef: Int): InlineType =
    stmts(sortRef) match {
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
                val ctrRef = stmts(
                  inferTermType(state)
                ).asInstanceOf[AbstractDataType]
                  .defaultCtr()
                val selectorsZip = stmts(ctrRef)
                  .asInstanceOf[Constructor]
                  .selectors
                  .map(p => (p, stmts(p).asInstanceOf[Selector]))
                val selRef =
                  selectorsZip.find(p => p._2.name == id.name) match {
                    case Some(value) => value._1
                    // if not in state, then check lets and then in globals
                    case None => {
                      val ret = letsMapStack.find(m => m.contains(id)) match {
                        case Some(foundMap) => foundMap(id)
                        case None =>
                          globalsMap
                            .getOrElse(id, throw new IdentifierOutOfScope(id))
                      }
                      return (ret, newNondets)
                    }
                  }
                // return application of selector to state
                memoAddInstruction(Application(selRef, List(state)))
              }
              // if state doesn't exist, then check lets and then globals
              case None => {
                letsMapStack.find(m => m.contains(id)) match {
                  case Some(foundMap) => foundMap(id)
                  case None =>
                    globalsMap.getOrElse(id, throw new IdentifierOutOfScope(id))
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
          memoAddInstruction(
            FunctionParameter(freshSymbolName(), sortRef)
          )
        )
        (newNondets.last, newNondets)
      }
      case l: Literal => {
        (memoAddInstruction(TheoryMacro(l.value().toString())), newNondets)
      }
      case OperatorApplication(GetNextValueOp(), _) => {
        throw new NotSupportedYet(expr)
      }

      case OperatorApplication(PolymorphicSelect(id), exp :: Nil) => {
        val opRef = {
          val exprCtrRef = {
            val res = exprToTerm(stateParam, local, exp)
            stmts(
              inferTermType(res._1)
            ).asInstanceOf[AbstractDataType]
              .defaultCtr()
          }
          val ctr = stmts(exprCtrRef).asInstanceOf[Constructor]
          ctr.selectors
            .find { s =>
              val sel = stmts(s).asInstanceOf[Selector]
              sel.name == id.name
            }
            .getOrElse(throw new IdentifierOutOfScope(id))
        }

        val expRef = {
          val res = exprToTerm(stateParam, local, exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = memoAddInstruction(Application(opRef, List(expRef)))

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
          val asConstRef = memoAddInstruction(TheoryMacro("as const"))
          val typeRef = typeUseToSortRef(typ)
          memoAddInstruction(Application(asConstRef, List(typeRef)))
        }

        val appRef = memoAddInstruction(
          Application(asConstAppRef, List(expRef))
        )

        (appRef, newNondets)
      }

      case OperatorApplication(ForallOp(ids), operand :: Nil) => {
        val locals = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          (p._1, selRef)
        }

        val opRef = memoAddInstruction(
          TheoryMacro("forall", locals.map(p => p._2))
        )

        val bodyRef = {
          val res = exprToTerm(stateParam, locals.toMap ++ local, operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = memoAddInstruction(Application(opRef, List(bodyRef)))

        (resultRef, newNondets)

      }
      case OperatorApplication(ExistsOp(ids), operand :: Nil) => {
        val locals = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          (p._1, selRef)
        }

        val opRef = memoAddInstruction(
          TheoryMacro("exists", locals.map(p => p._2))
        )

        val bodyRef = {
          val res = exprToTerm(stateParam, locals.toMap ++ local, operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = memoAddInstruction(Application(opRef, List(bodyRef)))

        (resultRef, newNondets)

      }
      case OperatorApplication(op, operands) => {
        val opRef = memoAddInstruction(TheoryMacro(op.name))

        val operandRefs = new ListBuffer[Int]()
        operands.foreach { x =>
          val loc = exprToTerm(stateParam, local, x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = memoAddInstruction(Application(opRef, operandRefs.toList))

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

        val appRef = memoAddInstruction(Application(opRef, operandRefs.toList))

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
        val mod = stmts(inferTermType(instanceRef))
          .asInstanceOf[middle.Module]
        // find next function location
        val nextMacro =
          stmts(mod.next).asInstanceOf[UserMacro]

        val extraArgs = nextMacro.params.tail

        val nextCallRef = memoAddInstruction(
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
        val mod = stmts(inferTermType(instanceRef))
          .asInstanceOf[middle.Module]
        // find init function location
        val initMacro =
          stmts(mod.init).asInstanceOf[UserMacro]

        val extraArgs = initMacro.params.tail

        val initCallRef = memoAddInstruction(
          Application(mod.init, List(instanceRef) ++ extraArgs)
        )

        (initCallRef, newNondets ++ extraArgs)
      }
      case _ => throw new NotSupportedYet(expr)
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
            val ctr = stmts(ctrRef)
              .asInstanceOf[Constructor]

            var found = false
            val components: List[Int] = ctr.selectors.map { s =>
              val sel = stmts(s).asInstanceOf[Selector]
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
              throw new IdentifierOutOfScope(id)
            }

            val bodyRef = memoAddInstruction(Application(ctrRef, components))

            val macroRef = memoAddInstruction(
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
              stmts(
                inferTermType(res._1)
              ).asInstanceOf[AbstractDataType]
                .defaultCtr()
            }

            val exprCtr =
              stmts(exprCtrRef).asInstanceOf[Constructor]

            // now get the components
            val components: List[Expr] = exprCtr.selectors.map { s =>
              val sel = stmts(s).asInstanceOf[Selector]
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
            throw new NotSupportedYet(e)
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
        val startRef = memoAddInstruction(
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
          val appRef = memoAddInstruction(
            Application(funcRef, List(acc) ++ mostRecentParams)
          )
          appRef
        }
      }

    } else {
      stateParam
    }

    val blockRef = memoAddInstruction(
      UserMacro(
        s"line${block.pos.line}col${block.pos.column}!${block.astNodeId}",
        stmts(ctrRef).asInstanceOf[Constructor].sort,
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
    val leftAppRef = memoAddInstruction(
      Application(left._1, List(stateParam) ++ left._2)
    )

    val right = stmtToMacro(stateParam, ctrRef, ifelse.elseblock)
    val rightAppRef = memoAddInstruction(
      Application(right._1, List(stateParam) ++ right._2)
    )

    val cond = exprToTerm(Some(stateParam), Map.empty, ifelse.cond)

    val iteRef = memoAddInstruction(TheoryMacro("ite"))

    val bodyRef = memoAddInstruction(
      Application(iteRef, List(cond._1, leftAppRef, rightAppRef))
    )

    val macroRef = memoAddInstruction(
      UserMacro(
        s"line${ifelse.pos.line}col${ifelse.pos.column}!${ifelse.astNodeId}",
        stmts(ctrRef).asInstanceOf[Constructor].sort,
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

        val modRef = stmts(stateParam).asInstanceOf[FunctionParameter].sort
        val selRefs = stmts(stmts(modRef).asInstanceOf[Module].ct)
          .asInstanceOf[Constructor]
          .selectors
        if (selRefs.exists { s =>
              stmts(s).asInstanceOf[Selector].name == l.id.name
            }) {
          throw new VariableOverride(
            s"Let statements cannot override state variables!\n\n${l.id.pos.longString}"
          )
        }

        val r = exprToTerm(Some(stateParam), Map.empty, l.expr)
        letsMapStack.top.addOne((l.id, r._1))
        // identity function
        val macroRef = memoAddInstruction(
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

    val um = stmts(res._1).asInstanceOf[UserMacro]

    memoUpdateInstruction(
      res._1,
      UserMacro(funcName, um.sort, um.body, um.params)
    )

    res
  }

  private def getAxiomRef(stateParam: Int, axioms: List[InnerAxiom]): Int = {
    // make sure invariant axioms hold on the fresh instance
    val specConjuncts = new ListBuffer[Int]()
    val axiomRef = if (axioms.length > 1) {
      val andRef = memoAddInstruction(TheoryMacro("and"))
      axioms.foreach { d =>
        val t = exprToTerm(
          Some(stateParam),
          Map.empty,
          d.expr
        )._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      memoAddInstruction(Application(andRef, specConjuncts.toList))
    } else if (axioms.length == 1) {
      exprToTerm(Some(stateParam), Map.empty, axioms(0).expr)._1 //specs shouldn't create new nondets
    } else {
      val trueRef = memoAddInstruction(TheoryMacro("true"))
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
        stmts(p) match {
          case FunctionParameter(_, sort) => {
            val vRef =
              addInstruction(UserFunction(freshSymbolName(), sort))
            vRef
          }
        }
      }

    def stepKTimes(k: Int, startRef: Int, nextRef: Int): List[Int] = {
      val steps: ListBuffer[Int] = new ListBuffer()
      steps.addOne(startRef)
      (1 to k).foreach { i =>
        val args = nextParams.tail.map { p =>
          stmts(p) match {
            case FunctionParameter(name, sort) => {
              val vRef =
                memoAddInstruction(
                  UserFunction(s"$name!step!$i", sort)
                )
              vRef
            }
          }
        }
        steps.addOne(
          memoAddInstruction(
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
              val mod = stmts(modRef)
                .asInstanceOf[middle.Module]
              val initRef = mod.init
              val nextRef = mod.next

              // TODO: currently ignores axioms
              val preInit = transitionToTerm(
                mod.name + "!pre_init",
                initParams.head,
                mod.ct,
                start
              )
              val fuzzed = fuzz(modRef)
              val startTerm = memoAddInstruction(
                Application(preInit._1, fuzzed :: preInit._2.map { p =>
                  stmts(p) match {
                    case FunctionParameter(_, sort) => {
                      fuzz(sort)
                    }
                  }
                })
              )

              val initVariables = startTerm :: initParams.tail.map { p =>
                stmts(p) match {
                  case FunctionParameter(_, sort) => {
                    fuzz(sort)
                  }
                }
              }

              var transRef = if (init.literal) {
                // apply init
                val initAppRef =
                  memoAddInstruction(
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
                  stmts(p) match {
                    case FunctionParameter(_, sort) => {
                      fuzz(sort)
                    }
                  }
                }
                transRef = memoAddInstruction(
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
              val mod = stmts(typeMap(moduleId))
                .asInstanceOf[middle.Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              // base case
              // apply init
              val initAppRef =
                memoAddInstruction(
                  Application(initRef, baseInitVariables),
                  Some("State_After_Init")
                )
              proofStates.addOne(initAppRef)

              // apply spec to result of init
              val initSpecRef =
                memoAddInstruction(Application(specRef, List(initAppRef)))

              val negRef = memoAddInstruction(TheoryMacro("not"))

              val baseRef =
                memoAddInstruction(
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
                memoAddInstruction(
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
                memoAddInstruction(Application(specRef, List(states.last)))

              val negExitRef =
                memoAddInstruction(Application(negRef, List(exitRef)))

              val andRef = memoAddInstruction(TheoryMacro("and"))

              val inductiveRef = memoAddInstruction(
                Application(andRef, List(entryRef, negExitRef)),
                Some("Counterexample_In_Invariants_Induction_Step")
              )

              addAssertion(inductiveRef)

            }
            case "unroll" => {
              // create all the variables you need
              val initVariables = generateInitVariables()

              // get the module declaration
              val mod = stmts(typeMap(moduleId))
                .asInstanceOf[middle.Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              val initAppRef =
                memoAddInstruction(
                  Application(initRef, initVariables),
                  Some("State_After_Init")
                )
              proofStates.addOne(initAppRef)

              // apply axiom after init so that fresh variables don't cause problems when unrolling
              if (axioms.length > 0) {
                val axiomRef = getAxiomRef(initAppRef, axioms)
                addAxiom(axiomRef)
              }

              val negRef = memoAddInstruction(TheoryMacro("not"))

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
                  memoAddInstruction(Application(specRef, List(p._1)))
                val negExitRef =
                  memoAddInstruction(
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
    val boolRef = memoAddInstruction(TheorySort("Bool"))

    val specConjuncts = new ListBuffer[Int]()

    val bodyRef = if (properties.length > 1) {
      val andRef = memoAddInstruction(TheoryMacro("and"))
      properties.foreach { d =>
        val t = exprToTerm(Some(stateParam), Map.empty, d.expr)._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      memoAddInstruction(Application(andRef, specConjuncts.toList))
    } else if (properties.length == 1) {
      exprToTerm(Some(stateParam), Map.empty, properties(0).expr)._1 //specs shouldn't create new nondets
    } else {
      val trueRef = memoAddInstruction(TheoryMacro("true"))
      trueRef
    }

    val specRef = memoAddInstruction(
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
              throw new UnreachableError(
                "createInitAssumes on arrays should not return a list of length greater than one! Please contact developers."
              )
          }
        }
        case NamedType(id) => {
          val sortRef = typeMap(id)
          stmts(sortRef) match {
            case _: middle.Module =>
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
                stmts(adt.defaultCtr()).asInstanceOf[Constructor]

              val components = ctr.selectors.map { s =>
                val sel = stmts(s).asInstanceOf[Selector]
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
          case Some(_) => throw new TypeOverride(td)
          case None => {
            val uRef = memoAddInstruction(UserSort(td.id.name))
            typeMap.addOne(
              td.id,
              uRef
            )
          }
        }
      case Some(EnumType(variants)) => {
        typeMap.get(td.id) match {
          case Some(_) => throw new TypeOverride(td)
          case None => {
            // add datatype placeholder
            val dtRef = addInstruction(DataType(td.id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val cRef =
                memoAddInstruction(Constructor(i.name, dtRef, List.empty))
              globalsMap.addOne(i, cRef)
              cRef
            }
            memoUpdateInstruction(dtRef, DataType(td.id.name, constructors))
            typeMap.addOne(td.id, dtRef)
          }
        }
      }
      case Some(RecordType(elements)) => {
        typeMap.get(td.id) match {
          case Some(_) => throw new TypeOverride(td)
          case None => {
            // add datatype placeholder
            val dtRef = addInstruction(DataType(td.id.name, List.empty))

            val selecorRefs = elements.map { p =>
              val tyepRef = typeUseToSortRef(p._2)
              memoAddInstruction(Selector(p._1.name, tyepRef))
            }

            val ctrRef = memoAddInstruction(
              Constructor(td.id.name, dtRef, selecorRefs)
            )
            globalsMap.addOne(td.id, ctrRef)

            memoUpdateInstruction(dtRef, DataType(td.id.name, List(ctrRef)))

            typeMap.addOne(td.id, dtRef)
          }
        }
      }
      case Some(_) => {
        // assign td.id (lhs) to whatever id is pointing to
        typeMap.get(td.id) match {
          case Some(_) =>
            throw new TypeOverride(td)
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
      memoAddInstruction(
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
      val selRef = memoAddInstruction(FunctionParameter(a._1.name, typeRef))
      (a._1, selRef)
    }
    val typeRef = typeUseToSortRef(dd.retTyp)
    val bodyRef = exprToTerm(None, params.toMap, dd.expr)._1 // defines cannot create new variables
    globalsMap.addOne(
      dd.id,
      memoAddInstruction(
        UserMacro(dd.id.name, typeRef, bodyRef, params.map(p => p._2))
      )
    )
  }

  def synthesisDeclToTerm(sy: SynthesisDecl): Unit = {
    val params = sy.params.map { a =>
      val typeRef = typeUseToSortRef(a._2)
      val selRef = memoAddInstruction(FunctionParameter(a._1.name, typeRef))
      (a._1, selRef)
    }
    val typeRef = typeUseToSortRef(sy.retTyp)
    globalsMap.addOne(
      sy.id,
      memoAddInstruction(
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
    val moduleRef = addInstruction(
      middle.Module(
        m.id.name,
        -1,
        -1,
        -1,
        -1
      )
    )

    typeMap.get(m.id) match {
      case Some(_) => throw new ModuleOverride(m)
      case None    => typeMap.addOne(m.id, moduleRef)
    }

    // input state
    val inputStateRef = memoAddInstruction(
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
        val selRef = memoAddInstruction(Selector(v._1.name, typeRef))

        // create application for scope
        val getterRef =
          memoAddInstruction(Application(selRef, List(inputStateRef)))
        selectorTerms.addOne((v._1.name, getterRef))

        selRef
      }

    // add constructor and remember where it is
    val constructorRef = memoAddInstruction(
      Constructor(m.id.name, moduleRef, selectorRefs)
    )
    globalsMap.addOne(m.id, constructorRef)

    // update module with constructor; will need to update it fully at the end
    memoUpdateInstruction(
      moduleRef,
      middle.Module(
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
    memoUpdateInstruction(
      moduleRef,
      middle.Module(m.id.name, constructorRef, initRef, nextRef, specRef)
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
}
