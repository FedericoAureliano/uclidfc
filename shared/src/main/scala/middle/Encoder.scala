package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import front._

// represents the current function we are building
class FunctionFrame(
  // signature of the function we're building
  var stateParam: Ref, // points to a function parameter of module type
  var auxParams: List[Ref], // auxiliary parameters (stuff like nondets)
  var outputSort: Ref // sort the function is returning (a module to represent state)
) {

  def addNondetParam(nondetParam: Ref): Unit =
    auxParams = auxParams.appended(nondetParam)

  def getParams(): List[Ref] =
    List(stateParam) ++ auxParams
}

object Encoder {

  // encode a type use (adds to program if type not yet used)
  // and return a pointer to the type
  def typeUseToTerm(
    program: Program, // modified
    t: Type
  ): Ref =
    t match {
      case UninterpretedType(name) =>
        program.loadOrSaveSortRef(
          name.name, {
            program.stmts.addOne(UserSort(name.name))
            program.saveSortRef(name.name, Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case BooleanType() =>
        program.loadOrSaveSortRef(
          t.toString(), {
            program.stmts.addOne(TheorySort("Bool"))
            program.saveSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case IntegerType() =>
        program.loadOrSaveSortRef(
          t.toString(), {
            program.stmts.addOne(TheorySort("Int"))
            program.saveSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case ArrayType(inTypes, outType) => {
        program.loadOrSaveSortRef(
          t.toString(), {
            val args =
              (inTypes ++ List(outType)).map(arg => typeUseToTerm(program, arg))
            program.stmts.addOne(TheorySort("Array", args))
            Ref(program.stmts.length - 1)
          }
        )
      }
      case SynonymType(id) =>
        program.loadSortRef(id.name) match {
          case Some(value) => value
          case None        =>
            // if we failed to find it as a regular type, try to find it as a module
            program.loadSortRef(id.name + "!type") match {
              case Some(value) => value
              case None =>
                throw new IllegalArgumentException(s"type not declared: ${t}")
            }
        }
      case _ =>
        throw new IllegalArgumentException(s"type not yet supported: ${t}")
    }

  def inferTermType(
    program: Program, // modified
    app: Ref
  ): Ref =
    program.stmts(app.loc) match {
      case Application(caller, args) => {
        program.stmts(caller.loc) match {
          case TheoryMacro("ite", params) => inferTermType(program, args.head)
          case TheoryMacro("store", params) =>
            inferTermType(program, args.head)
          case TheoryMacro("select", params) => {
            val arrayRef = inferTermType(program, args.head)
            val arraySort =
              program.stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => inferTermType(program, caller)
        }
      }
      case Constructor(name, sort, selectors)  => sort
      case FunctionParameter(name, sort)       => sort
      case Selector(name, sort)                => sort
      case UserMacro(name, sort, body, params) => sort
      case TheoryMacro(name, params) =>
        name match {
          case "true" | "false" | "and" | "or" | "=" =>
            program.loadOrSaveSortRef("Bool", {
              program.stmts.addOne(TheorySort("Bool"));
              Ref(program.stmts.length - 1)
            })
          case "+" | "*" | "-" =>
            program.loadOrSaveSortRef("Int", {
              program.stmts.addOne(TheorySort("Int"));
              Ref(program.stmts.length - 1)
            })
          case _ => {
            if (name.toIntOption.isDefined) {
              program.loadOrSaveSortRef("Int", {
                program.stmts.addOne(TheorySort("Int"));
                Ref(program.stmts.length - 1)
              })
            } else {
              throw new IllegalArgumentException(
                s"theory macro not supported yet: ${name}"
              )
            }
          }
        }
    }

  // encode a term and return a pointer to the start of the term
  def exprToTerm(
    program: Program, // modified
    expr: Expr
  ): Ref =
    expr match {
      case Identifier(name) => {
        // find selector
        program.loadObjectRef(name).get
      }
      case l: Literal => {
        program.loadOrSaveObjectRef(l.toString(), {
          program.stmts.addOne(TheoryMacro(l.toString()));
          Ref(program.stmts.length - 1)
        })
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

        val arrayRef = exprToTerm(program, operands.head)
        val valueRef = exprToTerm(program, value)
        val indexRef = exprToTerm(program, indices.head)

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(
          Application(opRef, List(arrayRef, indexRef, valueRef))
        )

        appRef
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

        val arrayRef = exprToTerm(program, operands.head)
        val indexRef = exprToTerm(program, indices.head)

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(arrayRef, indexRef)))

        appRef
      }
      case OperatorApplication(op, operands) => {
        val opRef = program.loadOrSaveObjectRef(op.name, {
          program.stmts.addOne(TheoryMacro(op.name));
          Ref(program.stmts.length - 1)
        })

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(program, x)
          operandRefs.addOne(loc)
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, operandRefs.toList))

        appRef
      }
      case FuncApplication(op, operands) => {
        val opRef = exprToTerm(program, op)

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(program, x)
          operandRefs.addOne(loc)
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, operandRefs.toList))

        appRef
      }

      case ModuleNextCallExpr(expr) => {
        // get the instance
        val instanceRef = exprToTerm(program, expr)
        // get the module it belongs to
        val modRef = inferTermType(program, instanceRef)
        // find next function location
        val nextRef =
          program.stmts(modRef.loc).asInstanceOf[middle.Module].next
        // fill in the placeholder from before
        val nextCallRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(nextRef, List(instanceRef)))

        nextCallRef
      }

      case ModuleInitCallExpr(id) => {
        // get the instance
        val instanceRef = program.loadObjectRef(id.name).get
        // get the module it belongs to
        val modRef = inferTermType(program, instanceRef)
        // find init function location
        val initRef =
          program.stmts(modRef.loc).asInstanceOf[middle.Module].init
        // fill in the placeholder from before
        val initCallRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(initRef, List(instanceRef)))

        initCallRef
      }

      case ConstArray(exp, typ) => {
        // BTW: Z3 can handle a non constant argument to the as const function, but CVC4 can't (Dec. 14, 2020)
        val expRef = exprToTerm(program, exp)

        val asConstRef = program.loadOrSaveObjectRef("as const", {
          program.stmts.addOne(TheoryMacro("as const"));
          Ref(program.stmts.length - 1)
        })

        val typeRef = typeUseToTerm(program, typ)

        val asConstAppRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(asConstRef, List(typeRef)))

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(asConstAppRef, List(expRef)))

        appRef
      }

      case _ =>
        throw new IllegalArgumentException(
          s"expression not implemented yet: ${expr}"
        )
    } // end helper exprToTerm

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

            acc ++ total

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
          case _ =>
            throw new IllegalArgumentException(
              s"statement not supported yet: ${p}"
            )
        }
      )

  def stmtToTerm(
    program: Program, // modified
    scope: FunctionFrame,
    stmt: Statement
  ): Ref =
    program.loadOrSaveObjectRef(
      stmt.astNodeId.toString(), {
        stmt match {
          case AssignStmt(lhss, rhss) => {
            assert(lhss.length == 1, "lhss must be flattened by now")
            assert(rhss.length == 1, "rhss must be flattened by now")
            val lhs = lhss(0)
            val rhs = rhss(0)

            lhs.expr match {
              case OperatorApplication(GetNextValueOp(), expr :: Nil) => {
                stmtToTerm(
                  program,
                  scope,
                  AssignStmt(List(Lhs(expr)), List(rhs))
                )
              }
              case Identifier(id) => {
                // get the constructor
                val modRef = scope.outputSort
                val ctrRef = program
                  .stmts(modRef.loc)
                  .asInstanceOf[middle.Module]
                  .ct
                val selRefs =
                  program
                    .stmts(ctrRef.loc)
                    .asInstanceOf[Constructor]
                    .selectors

                val components: List[Ref] = selRefs.map { s =>
                  val sel = program.stmts(s.loc).asInstanceOf[Selector]
                  lhs.expr match {
                    case Identifier(name) if name == sel.name =>
                      exprToTerm(program, rhs)
                    case _ => program.loadObjectRef(sel.name).get
                  }
                }

                val bodyRef = Ref(program.stmts.length)
                program.stmts.addOne(Application(ctrRef, components))

                val macroRef = Ref(program.stmts.length)
                program.stmts.addOne(
                  UserMacro(
                    s"stmt!${stmt.astNodeId}",
                    scope.outputSort,
                    bodyRef,
                    scope.getParams()
                  )
                )

                program.saveObjectRef(stmt.astNodeId.toString(), macroRef)

                macroRef
              }
              case OperatorApplication(
                  PolymorphicSelect(field),
                  expr :: Nil
                  ) => {
                val innerStateRef = exprToTerm(program, expr)
                val innerModRef = inferTermType(program, innerStateRef)
                val innerMod =
                  program
                    .stmts(innerModRef.loc)
                    .asInstanceOf[middle.Module]

                val innerCtr =
                  program.stmts(innerMod.ct.loc).asInstanceOf[Constructor]
                val innerGetters = innerCtr.selectors.map { selRef =>
                  val getterRef = Ref(program.stmts.length)
                  program.stmts.addOne(
                    Application(
                      selRef,
                      List(innerStateRef)
                    )
                  )

                  val name =
                    program.stmts(selRef.loc).asInstanceOf[Selector].name

                  (name, getterRef)
                }

                program.pushCache()
                innerGetters.foreach(p => program.saveObjectRef(p._1, p._2))

                val newScope = new FunctionFrame(
                  scope.stateParam,
                  scope.auxParams,
                  innerModRef
                )

                val newLhs: Lhs = Lhs(field)

                // create the inner function call
                val innerFuncRef =
                  stmtToTerm(
                    program,
                    newScope,
                    AssignStmt(List(newLhs), List(rhs))
                  )

                program.popCache()

                val modRef = scope.outputSort
                val ctrRef = program
                  .stmts(modRef.loc)
                  .asInstanceOf[middle.Module]
                  .ct
                val selRefs =
                  program
                    .stmts(ctrRef.loc)
                    .asInstanceOf[Constructor]
                    .selectors

                val components: List[Ref] = selRefs.map {
                  s =>
                    val sel = program.stmts(s.loc).asInstanceOf[Selector]
                    lhs.expr match {
                      case Identifier(name) if name == sel.name => {
                        val exprRef = Ref(program.stmts.length)
                        program.stmts.addOne(
                          Application(
                            innerFuncRef,
                            scope.getParams()
                          )
                        )
                        exprRef
                      }
                      case _ => program.loadObjectRef(sel.name).get
                    }
                }

                val bodyRef = Ref(program.stmts.length)
                program.stmts.addOne(Application(ctrRef, components))

                val macroRef = Ref(program.stmts.length)
                program.stmts.addOne(
                  UserMacro(
                    s"stmt!${stmt.astNodeId}",
                    scope.outputSort,
                    bodyRef,
                    scope.getParams()
                  )
                )

                program.saveObjectRef(stmt.astNodeId.toString(), macroRef)

                macroRef
              }
              case OperatorApplication(ArraySelect(indices), expr :: Nil) => {
                val newRhs =
                  OperatorApplication(ArrayUpdate(indices, rhs), List(expr))
                stmtToTerm(
                  program,
                  scope,
                  AssignStmt(List(Lhs(expr)), List(newRhs))
                )
              }
              case _ =>
                throw new IllegalArgumentException(
                  s"lhs not supported yet: ${stmt}"
                )
            }
          }
          case _ =>
            throw new IllegalArgumentException(
              s"should not be reachable: ${stmt}"
            )
        }
      }
    )

  def blockToTerm(
    program: Program, // modified
    scope: FunctionFrame,
    stmts: List[Statement]
  ): Ref = {
    val flattened =
      flattenStmts(stmts).map(p => AssignStmt(List(p._1), List(p._2)))

    if (flattened.length > 0) {
      val firstFuncRef = stmtToTerm(program, scope, flattened.head)

      val startRef = Ref(program.stmts.length)
      program.stmts.addOne(
        Application(firstFuncRef, scope.getParams())
      )

      flattened.tail.foldLeft(startRef) { (acc, stmt) =>
        val funcRef = stmtToTerm(program, scope, stmt)
        val appRef = Ref(program.stmts.length)
        // add the nondet parameters at the end
        program.stmts.addOne(
          Application(funcRef, List(acc) ++ scope.getParams().tail)
        )
        appRef
      }
    } else {
      val inputSort = program
        .stmts(scope.stateParam.loc)
        .asInstanceOf[FunctionParameter]
        .sort
      if (inputSort == scope.outputSort) {
        scope.stateParam
      } else {
        // TODO: we can't just return the input because the output state is different
        scope.stateParam
      }
    }
  }

  // encode a transition block and return a pointer to the function definition
  def transitionToTerm(
    program: Program, // modified
    scope: FunctionFrame,
    funcName: String,
    block: BlockStmt
  ): Ref = {

    val bodyRef = blockToTerm(
      program,
      scope,
      block.stmts
    )

    val transitionBlockRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        funcName,
        scope.outputSort,
        bodyRef,
        scope.getParams()
      )
    )

    transitionBlockRef
  }

  def executeControl(
    program: Program, // modified
    scope: FunctionFrame,
    moduleName: String,
    cmds: List[ProofCommand]
  ): Unit =
    cmds.foreach(p =>
      p.name.name match {
        case "induction" => {
          // create all the variables you need
          val variables = scope.getParams().map { p =>
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
          program.stmts.addOne(Application(initRef, variables))

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
          program.stmts.addOne(Application(specRef, List(variables(0))))

          // Take k steps
          val k = p.k.getOrElse(IntLit(1)).value.toInt

          var transRef = variables(0)

          (1 to k).foreach {
            i =>
              val args = scope.auxParams.map { p =>
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
          val variables = scope.getParams().map { p =>
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
          program.stmts.addOne(Application(initRef, variables))

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
              val args = scope.auxParams.map { p =>
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
    program: Program, // modified
    scope: FunctionFrame,
    funcName: String,
    properties: List[SpecDecl]
  ) = {
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
        val t = exprToTerm(program, d.expr)
        specConjuncts.addOne(t)
        t
      }
      program.stmts.addOne(Application(andRef, specConjuncts.toList))
      Ref(program.stmts.length - 1)
    } else if (properties.length == 1) {
      exprToTerm(program, properties(0).expr)
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
        List(scope.stateParam)
      )
    )

    specRef
  } // end helper specsToTerm

  def createInitCalls(
    program: Program, // modified
    scope: FunctionFrame,
    pair: (Identifier, Type)
  ): Option[(Expr)] =
    pair._2 match {
      case BooleanType()           => None
      case IntegerType()           => None
      case UninterpretedType(name) => None
      case ArrayType(inTypes, outType) =>
        createInitCalls(program, scope, (pair._1, outType)) match {
          case Some(e) =>
            Some(
              ConstArray(e, pair._2)
            )
          case None => None
        }
      case SynonymType(id) => {
        val sortRef = program.loadSortRef(id.name).get
        program.stmts(sortRef.loc) match {
          case _: middle.Module =>
            // get the sort to see if we need to create a new instance
            val fieldRef = program.loadObjectRef(pair._1.name).get
            val fieldSortRef = inferTermType(program, fieldRef)
            program.stmts(fieldSortRef.loc) match {
              case _: middle.Module =>
                Some(
                  ModuleInitCallExpr(pair._1)
                )
              case _ => {
                // anything else, create an instance and pipe it in
                // add a function parameter
                val tmpName = program.freshSymbolName()
                val fp = Ref(program.stmts.length)
                program.stmts.addOne(FunctionParameter(tmpName, sortRef))
                scope.addNondetParam(fp)
                program.saveObjectRef(tmpName, fp)
                Some(
                  ModuleInitCallExpr(Identifier(tmpName))
                )
              }
            }
          case _ => None
        }
      }
    }

  def typeDeclToTerm(program: Program, typ: TypeDecl): Unit = {
    val rhsRef = typeUseToTerm(program, typ.typ)
    typ.id match {
      case Some(value) => program.saveSortRef(value.name, rhsRef)
      case None        =>
    }
  }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    program: Program, // modified
    m: front.Module
  ): FunctionFrame = {

    // deal with type declarations
    m.typeDecls.foreach(t => typeDeclToTerm(program, t))

    // add placeholder for module and remember where it is
    val moduleRef = Ref(program.stmts.length)
    program.stmts.addOne(
      middle.Module(m.id.name, Ref(-1), Ref(-1), Ref(-1), Ref(-1))
    )
    program.saveSortRef(m.id.name, moduleRef)

    // input state
    val inputStateRef = Ref(program.stmts.length)
    program.stmts.addOne(FunctionParameter("in", moduleRef))

    val fields =
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs)

    // create selectors and remember where they are
    val selectorTerms = new HashMap[String, Ref]()
    val selectorRefs =
      fields.map { v =>
        val typeRef = typeUseToTerm(program, v._2)
        val selRef = Ref(program.stmts.length)
        program.stmts.addOne(Selector(v._1.name, typeRef))

        // create application for scope
        val getterRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(selRef, List(inputStateRef)))
        selectorTerms.addOne((v._1.name, getterRef))

        selRef
      }

    // functions cant be updated, but they are in the scope of the module
    m.functions.foreach { p =>
      val typeRefs = (List(p._3) ++ p._2).map(t => typeUseToTerm(program, t))
      val funcRef = Ref(program.stmts.length)
      program.stmts.addOne(
        UserFunction(p._1.name, typeRefs.head, typeRefs.tail)
      )
      program.saveObjectRef(p._1.name, funcRef)
    }

    // defines and constant literals
    m.defines.foreach { p =>
      program.pushCache()
      val params = p._2.map { a =>
        val typeRef = typeUseToTerm(program, a._2)
        val selRef = Ref(program.stmts.length)
        program.stmts.addOne(FunctionParameter(a._1.name, typeRef))

        program.saveObjectRef(a._1.name, selRef)

        selRef
      }
      val typeRef = typeUseToTerm(program, p._3)
      val bodyRef = exprToTerm(program, p._4)
      program.popCache()
      val funcRef = Ref(program.stmts.length)
      program.stmts.addOne(UserMacro(p._1.name, typeRef, bodyRef, params))
      program.saveObjectRef(p._1.name, funcRef)
    }

    val scope = new FunctionFrame(inputStateRef, List.empty, moduleRef)
    program.pushCache()
    selectorTerms.map(p => program.saveObjectRef(p._1, p._2))

    // add constructor and remember where it is
    val constructorRef = Ref(program.stmts.length)
    program.stmts.addOne(
      Constructor(m.id.name + "!cntr", moduleRef, selectorRefs)
    )

    // update module with constructor; will need to update it fully at the end
    program.stmts.update(
      moduleRef.loc,
      middle.Module(m.id.name, constructorRef, Ref(-1), Ref(-1), Ref(-1))
    )

    // add init and next
    val initInitCalls = fields.foldLeft(List.empty[Statement]) { (acc, p) =>
      createInitCalls(program, scope, p) match {
        case Some(e) => acc ++ List(AssignStmt(List(Lhs(p._1)), List(e)))
        case None    => acc
      }
    }
    val initBlock = m.init match {
      case Some(InitDecl(BlockStmt(stmts))) =>
        BlockStmt(stmts ++ initInitCalls)
      case Some(InitDecl(stmt)) => BlockStmt(List(stmt) ++ initInitCalls)
      case None                 => BlockStmt(initInitCalls)
    }
    val initRef = transitionToTerm(
      program,
      scope,
      m.id.name + "!init",
      initBlock
    )

    val nextBlock = m.next match {
      case Some(NextDecl(BlockStmt(stmts))) => BlockStmt(stmts)
      case Some(NextDecl(stmt))             => BlockStmt(List(stmt))
      case None                             => BlockStmt(List.empty)
    }
    val nextRef = transitionToTerm(
      program,
      scope,
      m.id.name + "!next",
      nextBlock
    )

    // Add spec function
    val specRef = specsToTerm(program, scope, m.id.name + "!spec", m.properties)

    // fill in placeholder for module
    program.stmts.update(
      moduleRef.loc,
      middle.Module(m.id.name, constructorRef, initRef, nextRef, specRef)
    )

    program.popCache()
    scope
  } // End Module to Term

  def run(
    model: List[front.Module],
    main: Option[String]
  ): Program = {

    val program = new Program(ArrayBuffer[Instruction]())

    // 1. Add every module to the program.
    // 2. When we find the main module, execute it
    val result = model.foreach { m =>
      val scope = moduleToTerm(program, m)
      if (Some(m.id.name) == main) {
        executeControl(program, scope, m.id.name, m.cmds)
      }
    }

    program
  }
}
