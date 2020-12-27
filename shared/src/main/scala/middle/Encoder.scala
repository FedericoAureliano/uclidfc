package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import front._

object Encoder {

  var program = new Program(ArrayBuffer[Instruction]())

  // encode a type use (adds to program if type not yet used)
  // and return a pointer to the type
  def typeUseToTerm(
    t: Type
  ): Ref =
    t match {
      case BooleanType() =>
        program.loadOrSaveSortRef(
          t.name, {
            program.stmts.addOne(TheorySort("Bool"))
            Ref(program.stmts.length - 1)
          }
        )
      case IntegerType() =>
        program.loadOrSaveSortRef(
          t.name, {
            program.stmts.addOne(TheorySort("Int"))
            Ref(program.stmts.length - 1)
          }
        )
      case ArrayType(inTypes, outType) => {
        program.loadOrSaveSortRef(
          t.name, {
            val args =
              (inTypes ++ List(outType)).map(arg => typeUseToTerm(arg))
            program.stmts.addOne(TheorySort("Array", args))
            Ref(program.stmts.length - 1)
          }
        )
      }
      case SynonymType(id) =>
        program
          .loadSortRef(id.name)
          .getOrElse(
            throw new TypeOutOfScope(t.pos, t)
          )
      case _ =>
        throw new TypeNotSupportedYet(t.pos, t)
    }

  def inferTermType(
    app: Ref
  ): Ref =
    program.stmts(app.loc) match {
      case Application(caller, args) => {
        program.stmts(caller.loc) match {
          case TheoryMacro("ite", params) => inferTermType(args.head)
          case TheoryMacro("store", params) =>
            inferTermType(args.head)
          case TheoryMacro("select", params) => {
            val arrayRef = inferTermType(args.head)
            val arraySort =
              program.stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => inferTermType(caller)
        }
      }
      case Constructor(name, sort, selectors) => sort
      case FunctionParameter(name, sort)      => sort
      case Selector(name, sort)               => sort
      case _ =>
        throw new IllegalArgumentException(
          s"type inference not yet supported: ${program.stmts(app.loc)}"
        )
    }

  def getTypeFromId(id: Identifier): Type = {
    val termRef = exprToTerm(id)._1
    val typeRef = inferTermType(termRef)
    sortToType(typeRef)
  }

  def sortToType(sortRef: Ref): Type =
    program.stmts(sortRef.loc) match {
      case DataType(name, constructors)       => SynonymType(Identifier(name))
      case Module(name, ct, init, next, spec) => SynonymType(Identifier(name))
      case TheorySort(name, params) =>
        name match {
          case "Int"  => IntegerType()
          case "Bool" => BooleanType()
          case "Array" => {
            val paramTypes = params.map(p => sortToType(p))
            ArrayType(List(paramTypes(0)), paramTypes(1))
          }
        }
      case UserSort(name, arity) => SynonymType(Identifier(name))
      case _ =>
        throw new IllegalArgumentException("sort to type must get a sort ref!")
    }

  // encode a term and return a pointer to the start of the term
  def exprToTerm(
    expr: Expr
  ): (Ref, List[Ref]) = {
    var newNondets: List[Ref] = List.empty
    expr match {
      case id: Identifier => {
        // find selector
        program.loadObjectRef(id.name) match {
          case Some(value) => (value, newNondets)
          case None        => throw new IdentifierOutOfScope(id.pos, id)
        }
      }
      case FreshLit(typ) => {
        val sortRef = typeUseToTerm(typ)
        newNondets = newNondets.appended(Ref(program.stmts.length))
        program.stmts.addOne(
          FunctionParameter(program.freshSymbolName(), sortRef)
        )
        (newNondets.last, newNondets)
      }
      case l: Literal => {
        (program.loadOrSaveObjectRef(l.toString(), {
          program.stmts.addOne(TheoryMacro(l.toString()));
          Ref(program.stmts.length - 1)
        }), newNondets)
      }
      case OperatorApplication(ArrayUpdate(indices, value), operands) => {
        assert(
          operands.length == 1,
          "array update must be applied to a single operand"
        )
        assert(indices.length == 1, "array index must be a single value")
        val opRef = program.loadOrSaveObjectRef("store", {
          program.stmts.addOne(TheoryMacro("store"));
          Ref(program.stmts.length - 1)
        })

        val arrayRef = {
          val res = exprToTerm(operands.head)
          newNondets = newNondets ++ res._2
          res._1
        }
        val valueRef = {
          val res = exprToTerm(value)
          newNondets = newNondets ++ res._2
          res._1
        }
        val indexRef = {
          val res = exprToTerm(indices.head)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(
          Application(opRef, List(arrayRef, indexRef, valueRef))
        )

        (appRef, newNondets)
      }
      case OperatorApplication(ArraySelect(indices), operands) => {
        assert(
          operands.length == 1,
          "array update must be applied to a single operand"
        )
        assert(indices.length == 1, "array index must be a single value")
        val opRef = program.loadOrSaveObjectRef("select", {
          program.stmts.addOne(TheoryMacro("select"));
          Ref(program.stmts.length - 1)
        })

        val arrayRef = {
          val res = exprToTerm(operands.head)
          newNondets = newNondets ++ res._2
          res._1
        }
        val indexRef = {
          val res = exprToTerm(indices.head)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(arrayRef, indexRef)))

        (appRef, newNondets)
      }
      case OperatorApplication(GetNextValueOp(), operands) => {
        throw new ExprNotSupportedYet(expr.pos, expr)
      }
      case OperatorApplication(ForallOp(ids), operands) => {
        assert(
          operands.length == 1,
          "forall must be applied to a single operand"
        )

        program.pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToTerm(p._2)
          val selRef = Ref(program.stmts.length)
          program.stmts.addOne(FunctionParameter(p._1.name, tyepRef))
          program.saveObjectRef(p._1.name, selRef)
          selRef
        }

        val opRef = {
          program.stmts.addOne(TheoryMacro("forall", selecorRefs));
          Ref(program.stmts.length - 1)
        }
        program.saveObjectRef(ForallOp(ids).name, opRef)

        val bodyRef = {
          val res = exprToTerm(operands.head)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(bodyRef)))

        program.popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(ExistsOp(ids), operands) => {
        assert(
          operands.length == 1,
          "exists must be applied to a single operand"
        )

        program.pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToTerm(p._2)
          val selRef = Ref(program.stmts.length)
          program.stmts.addOne(FunctionParameter(p._1.name, tyepRef))
          program.saveObjectRef(p._1.name, selRef)
          selRef
        }

        val opRef = {
          program.stmts.addOne(TheoryMacro("exists", selecorRefs));
          Ref(program.stmts.length - 1)
        }
        program.saveObjectRef(ExistsOp(ids).name, opRef)

        val bodyRef = {
          val res = exprToTerm(operands.head)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(bodyRef)))

        program.popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(op, operands) => {
        val opRef = program.loadOrSaveObjectRef(op.name, {
          program.stmts.addOne(TheoryMacro(op.name));
          Ref(program.stmts.length - 1)
        })

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, operandRefs.toList))

        (appRef, newNondets)
      }
      case FuncApplication(op, operands) => {
        val opRef = {
          val res = exprToTerm(op)
          newNondets = newNondets ++ res._2
          res._1
        }

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, operandRefs.toList))

        (appRef, newNondets)
      }

      case ModuleNextCallExpr(expr) => {
        // get the instance
        val instanceRef = {
          val res = exprToTerm(expr)
          newNondets = newNondets ++ res._2
          res._1
        }
        // get the module it belongs to
        val modRef = inferTermType(instanceRef)
        // find next function location
        val nextRef =
          program.stmts(modRef.loc).asInstanceOf[middle.Module].next
        // fill in the placeholder from before
        val nextCallRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(nextRef, List(instanceRef)))

        (nextCallRef, newNondets)
      }

      case ModuleInitCallExpr(expr) => {
        // get the instance
        val instanceRef = {
          val res = exprToTerm(expr)
          newNondets = newNondets ++ res._2
          res._1
        }
        // get the module it belongs to
        val modRef = inferTermType(instanceRef)
        // find init function location
        val initRef =
          program.stmts(modRef.loc).asInstanceOf[middle.Module].init
        // fill in the placeholder from before
        val initCallRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(initRef, List(instanceRef)))

        (initCallRef, newNondets)
      }

      case ConstArray(exp, typ) => {
        // BTW: Z3 can handle a non constant argument to the as const function, but CVC4 can't (Dec. 14, 2020)
        val expRef = {
          val res = exprToTerm(exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val asConstRef = program.loadOrSaveObjectRef("as const", {
          program.stmts.addOne(TheoryMacro("as const"));
          Ref(program.stmts.length - 1)
        })

        val typeRef = typeUseToTerm(typ)

        val asConstAppRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(asConstRef, List(typeRef)))

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(asConstAppRef, List(expRef)))

        (appRef, newNondets)
      }

      case _ => throw new ExprNotSupportedYet(expr.pos, expr)
    } // end helper exprToTerm
  }

  def flattenStmts(
    stmts: List[Statement]
  ): List[(Lhs, Expr)] =
    // find the assignment
    stmts
      .foldLeft(List[(Lhs, Expr)]())((acc, p) =>
        p match {
          case AssignStmt(lhss, rhss) =>
            acc ++ lhss.zip(rhss)
          case ModuleNextCallStmt(expr) =>
            acc ++ List(Tuple2(Lhs(expr), ModuleNextCallExpr(expr)))
          case IfElseStmt(cond, ifblock, elseblock) => {
            val left = flattenStmts(List(ifblock))
            val right = flattenStmts(List(elseblock))

            val identifiers: List[Lhs] = (left ++ right).map(p => p._1)

            val total = identifiers.foldLeft(List.empty: List[(Lhs, Expr)]) {
              (acc1, lhs) =>
                val leftTmp = left.filter(pair2 => pair2._1 == lhs)
                val rightTmp = right.filter(pair2 => pair2._1 == lhs)

                val leftFiltered = if (leftTmp.length == 0) {
                  List(Tuple2(lhs, lhs.expr))
                } else {
                  leftTmp
                }
                val rightFiltered = if (rightTmp.length == 0) {
                  List(Tuple2(lhs, lhs.expr))
                } else {
                  rightTmp
                }

                val zipped: List[(Lhs, Expr)] =
                  leftFiltered.foldLeft(List.empty: List[(Lhs, Expr)]) {
                    (acc2, l) =>
                      val inner: List[(Lhs, Expr)] =
                        rightFiltered
                          .foldLeft(List.empty: List[(Lhs, Expr)]) {
                            (acc3, r) =>
                              val e = if (l._2 == r._2) {
                                Tuple2(l._1, l._2)
                              } else {
                                Tuple2(
                                  l._1,
                                  OperatorApplication(
                                    ITEOp(),
                                    List(cond, l._2, r._2)
                                  )
                                )
                              }
                              acc3 ++ List(e)
                          }
                      acc2 ++ inner
                  }

                acc1 ++ zipped
            }

            acc ++ total.distinct

          }
          case CaseStmt(body) => {
            val nested = body.reverse.foldLeft(
              BlockStmt(List.empty): Statement
            )((acc, f) =>
              IfElseStmt(f._1, BlockStmt(List(f._2)), BlockStmt(List(acc)))
            )
            acc ++ flattenStmts(List(nested))
          }
          case BlockStmt(bstmts) =>
            acc ++ flattenStmts(bstmts)
          case HavocStmt(toMatch) =>
            acc ++ List(Tuple2(Lhs(toMatch), FreshLit(getTypeFromId(toMatch))))
          case s =>
            throw new StatementNotSupportedYet(s.pos, s)
        }
      )

  // statements are encoded as functions.
  def stmtToTerm(
    stateParam: Ref,
    ctrRef: Ref, // this is the constructor to build the target object
    stmt: AssignStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    (
      program.loadOrSaveObjectRef(
        stmt.astNodeId.toString(), {
          assert(stmt.lhss.length == 1, "lhss must be flattened by now")
          assert(stmt.rhss.length == 1, "rhss must be flattened by now")
          val lhs = stmt.lhss(0)
          val rhs = stmt.rhss(0)

          lhs.expr match {
            case OperatorApplication(GetNextValueOp(), expr :: Nil) => {
              // TODO: handle primes correctly (right now we just ignore them)
              val res = stmtToTerm(
                stateParam,
                ctrRef,
                AssignStmt(List(Lhs(expr)), List(rhs))
              )
              newParams ++= res._2
              res._1
            }
            case Identifier(id) => {
              // get the constructor
              val ctr = program
                .stmts(ctrRef.loc)
                .asInstanceOf[Constructor]

              val components: List[Ref] = ctr.selectors.map {
                s =>
                  val sel = program.stmts(s.loc).asInstanceOf[Selector]
                  lhs.expr match {
                    // if we are looking at the current selector, then process it, otherwise just return the identity
                    case Identifier(name) if name == sel.name =>
                      val res = exprToTerm(rhs)
                      newParams ++= res._2
                      res._1
                    case _ =>
                      program.loadObjectRef(sel.name) match {
                        case Some(value) => value
                        case None =>
                          throw new IdentifierOutOfScope(
                            Identifier(sel.name).pos,
                            Identifier(sel.name)
                          ) // todo: how to preserve position?
                      }
                  }
              }

              val bodyRef = Ref(program.stmts.length)
              program.stmts.addOne(Application(ctrRef, components))

              val macroRef = Ref(program.stmts.length)
              program.stmts.addOne(
                UserMacro(
                  // line number, column number, ast id
                  s"line${lhs.expr.pos.line}col${lhs.expr.pos.column}!${stmt.astNodeId}",
                  ctr.sort,
                  bodyRef,
                  List(stateParam) ++ newParams
                )
              )

              program.saveObjectRef(stmt.astNodeId.toString(), macroRef)

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
                val res = exprToTerm(expr)
                newParams ++= res._2
                program
                  .stmts(
                    inferTermType(res._1).loc
                  )
                  .asInstanceOf[AbstractDataType]
                  .defaultCtr()
              }

              val exprCtr =
                program.stmts(exprCtrRef.loc).asInstanceOf[Constructor]

              // now get the components
              val components: List[Expr] = exprCtr.selectors.map { s =>
                val sel = program.stmts(s.loc).asInstanceOf[Selector]
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
                FuncApplication(Identifier(exprCtr.name), components)

              val res = stmtToTerm(
                stateParam,
                ctrRef,
                AssignStmt(List(Lhs(expr)), List(newRhs))
              )
              newParams ++= res._2
              res._1
            }
            case OperatorApplication(ArraySelect(indices), expr :: Nil) => {
              val newRhs =
                OperatorApplication(ArrayUpdate(indices, rhs), List(expr))
              val res = stmtToTerm(
                stateParam,
                ctrRef,
                AssignStmt(List(Lhs(expr)), List(newRhs))
              )
              newParams ++= res._2
              res._1
            }
            case e =>
              throw new ExprNotSupportedYet(e.pos, e)
          }
        }
      ),
      newParams
    )
  }

  def blockToTerm(
    stateParam: Ref,
    ctrRef: Ref, // current module constructor
    stmts: List[Statement]
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    var mostRecentParams: List[Ref] = List.empty
    val flattened =
      flattenStmts(stmts).map(p => AssignStmt(List(p._1), List(p._2)))

    if (flattened.length > 0) {
      val firstFuncRef = {
        val res =
          stmtToTerm(stateParam, ctrRef, flattened.head)
        newParams ++= res._2
        mostRecentParams = res._2
        res._1
      }

      val startRef = Ref(program.stmts.length)
      program.stmts.addOne(
        Application(firstFuncRef, List(stateParam) ++ mostRecentParams)
      )

      (flattened.tail.foldLeft(startRef) { (acc, stmt) =>
        val funcRef = {
          val res = stmtToTerm(stateParam, ctrRef, stmt)
          newParams ++= res._2
          mostRecentParams = res._2
          res._1
        }
        val appRef = Ref(program.stmts.length)
        // add the nondet parameters at the end
        program.stmts.addOne(
          Application(funcRef, List(acc) ++ mostRecentParams)
        )
        appRef
      }, newParams)
    } else {
      (stateParam, newParams)
    }
  }

  // encode a transition block and return a pointer to the function definition
  def transitionToTerm(
    funcName: String,
    stateParam: Ref,
    ctrRef: Ref,
    block: BlockStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    val bodyRef = {
      val res = blockToTerm(
        stateParam,
        ctrRef,
        block.stmts
      )
      newParams ++= res._2
      res._1
    }

    val transitionBlockRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        funcName,
        program.stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        bodyRef,
        List(stateParam) ++ newParams
      )
    )

    (transitionBlockRef, newParams)
  }

  def executeControl(
    moduleName: String,
    initParams: List[Ref],
    nextParams: List[Ref],
    cmds: List[ProofCommand]
  ): Unit =
    cmds.foreach(p =>
      p.name.name match {
        case "induction" => {
          // create all the variables you need
          val initVariables = initParams.map { p =>
            program.stmts(p.loc) match {
              case FunctionParameter(name, sort) => {
                val vRef = Ref(program.stmts.length)
                program.stmts.addOne(UserFunction(s"$name!step!0", sort))
                vRef
              }
            }
          }

          // get the module declaration
          val mod = program
            .stmts(program.loadSortRef(moduleName).get.loc)
            .asInstanceOf[middle.Module]
          val initRef = mod.init
          val nextRef = mod.next
          val specRef = mod.spec

          // base case
          // apply init
          val initAppRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(initRef, initVariables))

          // apply spec to result of init
          val initSpecRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(initAppRef)))

          val negRef = program.loadOrSaveObjectRef("not", {
            program.stmts.addOne(TheoryMacro("not"))
            Ref(program.stmts.length - 1)
          })

          val baseRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(initSpecRef)))

          program.assertions.addOne(baseRef)

          // induction step
          // holds on entry
          val entryRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(initVariables.head)))
          // we can borrow the nondet state from init since we pop between asserts (this lets us just generate the auxiliary arguments when applying next)

          // Take k steps
          val k = p.k.getOrElse(IntLit(1)).value.toInt

          var transRef = initVariables.head

          (1 to k).foreach {
            i =>
              val args = nextParams.tail.map { p =>
                program.stmts(p.loc) match {
                  case FunctionParameter(name, sort) => {
                    val vRef = Ref(program.stmts.length)
                    program.stmts.addOne(UserFunction(s"$name!step!$i", sort))
                    vRef
                  }
                }
              }
              program.stmts.addOne(Application(nextRef, List(transRef) ++ args))
              transRef = Ref(program.stmts.length - 1)
          }

          // holds on exit
          val exitRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(transRef)))

          val negExitRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(exitRef)))

          val andRef = program.loadOrSaveObjectRef("and", {
            program.stmts.addOne(TheoryMacro("and"))
            Ref(program.stmts.length - 1)
          })

          val inductiveRef = Ref(program.stmts.length)
          program.stmts.addOne(
            Application(andRef, List(entryRef, negExitRef))
          )

          program.assertions.addOne(inductiveRef)

        }
        case "unroll" => {
          // create all the variables you need
          val initVariables = initParams.map { p =>
            program.stmts(p.loc) match {
              case FunctionParameter(name, sort) => {
                val vRef = Ref(program.stmts.length)
                program.stmts.addOne(UserFunction(s"$name!step!0", sort))
                vRef
              }
            }
          }

          // get the module declaration
          val mod = program
            .stmts(program.loadSortRef(moduleName).get.loc)
            .asInstanceOf[middle.Module]
          val initRef = mod.init
          val nextRef = mod.next
          val specRef = mod.spec

          val initAppRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(initRef, initVariables))

          // apply spec to result of init
          val initSpecRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(initAppRef)))

          val negRef = program.loadOrSaveObjectRef("not", {
            program.stmts.addOne(TheoryMacro("not"))
            Ref(program.stmts.length - 1)
          })

          val baseRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(initSpecRef)))

          program.assertions.addOne(baseRef)

          // Take k steps
          val k = p.k.getOrElse(IntLit(1)).value.toInt

          var transRef = initAppRef

          (1 to k).foreach {
            i =>
              val args = nextParams.tail.map { p =>
                program.stmts(p.loc) match {
                  case FunctionParameter(name, sort) => {
                    val vRef = Ref(program.stmts.length)
                    program.stmts.addOne(UserFunction(s"$name!step!$i", sort))
                    vRef
                  }
                }
              }
              program.stmts.addOne(Application(nextRef, List(transRef) ++ args))
              transRef = Ref(program.stmts.length - 1)
              // holds on exit
              val exitRef = Ref(program.stmts.length)
              program.stmts.addOne(Application(specRef, List(transRef)))
              val negExitRef = Ref(program.stmts.length)
              program.stmts.addOne(Application(negRef, List(exitRef)))

              program.assertions.addOne(negExitRef)
          }
        }
      }
    )

  // get all the specs and create a function from them
  def specsToTerm(
    funcName: String,
    params: List[Ref],
    properties: List[SpecDecl]
  ): Ref = {
    // spec needs Bool, so add Bool if it's not already there
    val boolRef = program.loadOrSaveSortRef("Bool", {
      program.stmts.addOne(TheorySort("Bool"));
      Ref(program.stmts.length - 1)
    })

    val specConjuncts = new ListBuffer[Ref]()

    val bodyRef = if (properties.length > 1) {
      val andRef = program.loadOrSaveObjectRef("and", {
        program.stmts.addOne(TheoryMacro("and"));
        Ref(program.stmts.length - 1)
      })
      properties.foreach { d =>
        val t = exprToTerm(d.expr)._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      program.stmts.addOne(Application(andRef, specConjuncts.toList))
      Ref(program.stmts.length - 1)
    } else if (properties.length == 1) {
      exprToTerm(properties(0).expr)._1 //specs shouldn't create new nondets
    } else {
      val trueRef = program.loadOrSaveObjectRef("true", {
        program.stmts.addOne(TheoryMacro("true"));
        Ref(program.stmts.length - 1)
      })
      trueRef
    }

    val specRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        funcName,
        boolRef,
        bodyRef,
        params
      )
    )

    specRef
  } // end helper specsToTerm

  def createInitCalls(
    fields: List[(Identifier, Type)] // variable we want to init and its type
  ): List[AssignStmt] =
    fields.foldLeft(List.empty: List[AssignStmt])((acc, f) =>
      createInitCallRhs(f._2) match {
        case Some(value) =>
          acc ++ List(AssignStmt(List(Lhs(f._1)), List(value)))
        case None => acc
      }
    )

  def createInitCallRhs(typ: Type): Option[Expr] =
    typ match {
      case ArrayType(inTypes, outType) => {
        createInitCallRhs(outType) match {
          case Some(value) => Some(ConstArray(value, typ))
          case None        => None
        }
      }

      case SynonymType(id) => {
        val sortRef = program.loadSortRef(id.name).get
        program.stmts(sortRef.loc) match {
          case _: middle.Module => Some(ModuleInitCallExpr(FreshLit(typ)))
          case adt: AbstractDataType => {
            val ctr =
              program.stmts(adt.defaultCtr().loc).asInstanceOf[Constructor]
            val selTypes = ctr.selectors.map(s =>
              sortToType(program.stmts(s.loc).asInstanceOf[Selector].sort)
            )
            val components = selTypes.map(p => createInitCallRhs(p))
            if (components.exists(p => p.isDefined)) {
              Some(
                FuncApplication(
                  Identifier(ctr.name),
                  components.zipWithIndex.map(p =>
                    p._1 match {
                      case Some(value) => value
                      case None        => FreshLit(selTypes(p._2))
                    }
                  )
                )
              )
            } else {
              None
            }
          }
          case _ => None
        }

      }

      case EnumType(_) | RecordType(_) =>
        throw new Unreachable(
          typ.pos,
          "enums and tuples are covered by synonym type since they can't be used inline"
        )

      case BooleanType() | IntegerType() | UninterpretedType() =>
        None // don't need to be inited
    }

  def typeDeclToTerm(typ: TypeDecl): Unit =
    typ.typ match {
      case UninterpretedType() =>
        program.loadSortRef(typ.id.name) match {
          case Some(value) => throw new TypeOverride(typ.pos, typ)
          case None => {
            program.stmts.addOne(UserSort(typ.id.name))
            program.saveSortRef(typ.id.name, Ref(program.stmts.length - 1))
          }
        }
      case EnumType(variants) => {
        program.loadSortRef(typ.id.name) match {
          case Some(value) => throw new TypeOverride(typ.pos, typ)
          case None => {
            // add datatype placeholder
            val dtRef = Ref(program.stmts.length)
            program.stmts.addOne(DataType(typ.id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val vRef = Ref(program.stmts.length)
              program.stmts.addOne(Constructor(i.name, dtRef, List.empty))
              program.saveObjectRef(i.name, vRef)
              vRef
            }
            program.stmts.update(dtRef.loc, DataType(typ.id.name, constructors))
            program.saveSortRef(typ.id.name, dtRef)
          }
        }
      }
      case RecordType(elements) => {
        program.loadSortRef(typ.id.name) match {
          case Some(value) => throw new TypeOverride(typ.pos, typ)
          case None => {
            // add datatype placeholder
            val dtRef = Ref(program.stmts.length)
            program.stmts.addOne(DataType(typ.id.name, List.empty))

            val selecorRefs = elements.map { p =>
              val tyepRef = typeUseToTerm(p._2)
              program.stmts.addOne(Selector(p._1.name, tyepRef))
              Ref(program.stmts.length - 1)
            }

            val ctrRef = Ref(program.stmts.length)
            program.stmts.addOne(Constructor(typ.id.name, dtRef, selecorRefs))
            program.saveObjectRef(typ.id.name, ctrRef)

            program.stmts.update(dtRef.loc, DataType(typ.id.name, List(ctrRef)))

            program.saveSortRef(typ.id.name, dtRef)
          }
        }
      }
      case _ => {
        // assign typ.id (lhs) to whatever id is pointing to
        program.loadSortRef(typ.id.name) match {
          case Some(value) => throw new TypeOutOfScope(typ.typ.pos, typ.typ)
          case None =>
            program.saveSortRef(typ.id.name, typeUseToTerm(typ.typ))
        }
      }
    }

  def functionDeclToTerm(fd: FunctionDecl): Unit = {
    val typeRefs =
      (List(fd.retTyp) ++ fd.argTypes).map(t => typeUseToTerm(t))
    val funcRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserFunction(fd.id.name, typeRefs.head, typeRefs.tail)
    )
    program.saveObjectRef(fd.id.name, funcRef)
  }

  def defineDeclToTerm(dd: DefineDecl): Unit = {
    program.pushCache()
    val params = dd.params.map { a =>
      val typeRef = typeUseToTerm(a._2)
      val selRef = Ref(program.stmts.length)
      program.stmts.addOne(FunctionParameter(a._1.name, typeRef))

      program.saveObjectRef(a._1.name, selRef)

      selRef
    }
    val typeRef = typeUseToTerm(dd.retTyp)
    val bodyRef = exprToTerm(dd.expr)._1 // defines cannot create new variables
    program.popCache()
    val funcRef = Ref(program.stmts.length)
    program.stmts.addOne(UserMacro(dd.id.name, typeRef, bodyRef, params))
    program.saveObjectRef(dd.id.name, funcRef)
  }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    m: ModuleDecl
  ): (List[Ref], List[Ref]) = { // returns init params and next params

    // deal with type declarations
    m.typeDecls.foreach(t => typeDeclToTerm(t))

    // add placeholder for module and remember where it is
    val moduleRef = Ref(program.stmts.length)
    program.stmts.addOne(
      middle.Module(m.id.name, Ref(-1), Ref(-1), Ref(-1), Ref(-1))
    )

    program.loadSortRef(m.id.name) match {
      case Some(value) => throw new ModuleOverride(m.pos, m)
      case None        => program.saveSortRef(m.id.name, moduleRef)
    }

    // input state
    val inputStateRef = Ref(program.stmts.length)
    program.stmts.addOne(FunctionParameter("in", moduleRef))

    val fields =
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs)

    // create selectors and remember where they are
    val selectorTerms = new HashMap[String, Ref]()
    val selectorRefs =
      fields.map { v =>
        val typeRef = typeUseToTerm(v._2)
        val selRef = Ref(program.stmts.length)
        program.stmts.addOne(Selector(v._1.name, typeRef))

        // create application for scope
        val getterRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(selRef, List(inputStateRef)))
        selectorTerms.addOne((v._1.name, getterRef))

        selRef
      }

    // functions cant be updated, but they are in the scope of the module
    m.functions.foreach(p => functionDeclToTerm(p))

    // defines and constant literals
    m.defines.foreach(p => defineDeclToTerm(p))

    // add constructor and remember where it is
    val constructorRef = Ref(program.stmts.length)
    program.stmts.addOne(
      Constructor(m.id.name, moduleRef, selectorRefs)
    )
    program.saveObjectRef(m.id.name, constructorRef)

    program.pushCache()
    selectorTerms.map(p => program.saveObjectRef(p._1, p._2))

    // update module with constructor; will need to update it fully at the end
    program.stmts.update(
      moduleRef.loc,
      middle.Module(m.id.name, constructorRef, Ref(-1), Ref(-1), Ref(-1))
    )

    val initInitCalls = createInitCalls(fields)
    var initParams = List(inputStateRef)
    val initBlock = m.init match {
      case Some(decl) => BlockStmt(decl.body.stmts ++ initInitCalls)
      case None       => BlockStmt(initInitCalls)
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
      List(inputStateRef),
      m.properties
    )

    // fill in placeholder for module
    program.stmts.update(
      moduleRef.loc,
      middle.Module(m.id.name, constructorRef, initRef, nextRef, specRef)
    )

    program.popCache()

    (initParams, nextParams)
  } // End Module to Term

  def run(
    model: List[TopLevelDecl],
    main: Option[String]
  ): Program = {
    program = new Program(ArrayBuffer[Instruction]())
    // 1. Add every module to the program.
    // 2. When we find the main module, execute it
    val result = model.foreach { m =>
      m match {
        case td: TypeDecl     => typeDeclToTerm(td)
        case dd: DefineDecl   => defineDeclToTerm(dd)
        case fd: FunctionDecl => functionDeclToTerm(fd)
        case mod: ModuleDecl => {
          val params = moduleToTerm(mod)
          if (Some(mod.id.name) == main) {
            executeControl(mod.id.name, params._1, params._2, mod.cmds)
          }
        }
      }
    }

    program
  }
}
