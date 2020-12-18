package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import front._

// represents the current function we are building
class ScopeFrame(
  // signature of the function we're building
  var stateParam: Ref,
  var nondetParams: List[Ref],
  var outputSort: Ref,
  // map from identifier names to term that gets them for you
  val fieldRefs: HashMap[String, Ref]
) {

  def setStateParam(stateRef: Ref): Unit =
    stateParam = stateRef

  def getStateParam(): Ref =
    stateParam

  def setOutputSort(outputSortRef: Ref): Unit =
    outputSort = outputSortRef

  def getOutputSort(): Ref =
    outputSort

  def addNondetParam(nondetParam: Ref): Unit =
    nondetParams = nondetParams.appended(nondetParam)

  def addFieldRef(name: String, term: Ref): Unit =
    fieldRefs.addOne((name, term))

  def removeFieldRef(name: String): Unit =
    fieldRefs.remove(name)

  def getFieldRef(name: String): Ref =
    fieldRefs(name)

  def getParams(): List[Ref] =
    List(stateParam) ++ nondetParams

  def resetFrame(): Unit = {
    stateParam = Ref(-1)
    nondetParams = List.empty
    outputSort = Ref(-1)
    fieldRefs.clear()
  }
}

object Encoder {

  // encode a type use (adds to program if type not yet used)
  // and return a pointer to the type
  def typeUseToTerm(
    program: TermGraph, // modified
    t: Type
  ): Ref =
    t match {
      case UninterpretedType(name) =>
        program.getOrCacheSortRef(
          t.toString(), {
            program.stmts.addOne(UserSort(name.name))
            program.cacheSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case BooleanType() =>
        program.getOrCacheSortRef(
          t.toString(), {
            program.stmts.addOne(TheorySort("Bool"))
            program.cacheSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case IntegerType() =>
        program.getOrCacheSortRef(
          t.toString(), {
            program.stmts.addOne(TheorySort("Int"))
            program.cacheSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case StringType() =>
        program.getOrCacheSortRef(
          t.toString(), {
            program.stmts.addOne(TheorySort("String"))
            program.cacheSortRef(t.toString(), Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case BitVectorType(width) => {
        program.getOrCacheSortRef(
          t.toString(), {
            program.stmts.addOne(
              TheorySort("_ BitVec", List(Ref(program.stmts.length + 1)))
            )
            program.stmts.addOne(Numeral(width))
            program.cacheSortRef(t.toString(), Ref(program.stmts.length - 2))
            Ref(program.stmts.length - 2)
          }
        )
      }
      case ArrayType(inTypes, outType) => {
        program.getOrCacheSortRef(
          t.toString(), {
            val args =
              (inTypes ++ List(outType)).map(arg => typeUseToTerm(program, arg))
            program.stmts.addOne(TheorySort("Array", args))
            Ref(program.stmts.length - 1)
          }
        )
      }
      case SynonymType(id) =>
        program.getType(id.name) match {
          case Some(value) => value
          case None =>
            program.getType(id.name + "!type") match {
              case Some(value) => value
              case None =>
                throw new IllegalArgumentException(s"type not declared: ${t}")
            }
        }
      case _ =>
        throw new IllegalArgumentException(s"type not yet supported: ${t}")
    }

  // TODO: combine with middle.core.semantics.inferSort ?
  def getTermTypeRef(
    program: TermGraph, // modified
    app: Ref
  ): Ref =
    program.stmts(app.loc) match {
      case Application(caller, args) => {
        // TODO check if caller is ite, select, store, ...
        program.stmts(caller.loc) match {
          case TheoryMacro("ite", params) => getTermTypeRef(program, args.head)
          case TheoryMacro("store", params) =>
            getTermTypeRef(program, args.head)
          case TheoryMacro("select", params) => {
            val arrayRef = getTermTypeRef(program, args.head)
            val arraySort =
              program.stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => getTermTypeRef(program, caller)
        }
      }
      case Constructor(name, sort, selectors)  => sort
      case FunctionParameter(name, sort)       => sort
      case Selector(name, sort)                => sort
      case UserMacro(name, sort, body, params) => sort
      case TheoryMacro(name, params) =>
        name match {
          case "true" | "false" | "and" | "or" | "=" =>
            program.getOrCacheSortRef("Bool", {
              program.stmts.addOne(TheorySort("Bool"));
              Ref(program.stmts.length - 1)
            })
          case "+" | "*" | "-" =>
            program.getOrCacheSortRef("Int", {
              program.stmts.addOne(TheorySort("Int"));
              Ref(program.stmts.length - 1)
            })
          case _ => {
            if (name.toIntOption.isDefined) {
              program.getOrCacheSortRef("Int", {
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
    program: TermGraph, // modified
    scope: ScopeFrame,
    expr: Expr
  ): Ref =
    expr match {
      case Identifier(name) => {
        // find selector
        scope.getFieldRef(name)
      }
      case l: Literal => {
        program.getOrCacheCallerRef(l.toString(), {
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
        val opRef = program.getOrCacheCallerRef("store", {
          program.stmts.addOne(TheoryMacro("store"));
          Ref(program.stmts.length - 1)
        })

        val arrayRef = exprToTerm(program, scope, operands.head)
        val valueRef = exprToTerm(program, scope, value)
        val indexRef = exprToTerm(program, scope, indices.head)

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
        val opRef = program.getOrCacheCallerRef("select", {
          program.stmts.addOne(TheoryMacro("select"));
          Ref(program.stmts.length - 1)
        })

        val arrayRef = exprToTerm(program, scope, operands.head)
        val indexRef = exprToTerm(program, scope, indices.head)

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(arrayRef, indexRef)))

        appRef
      }
      case OperatorApplication(op, operands) => {
        val opRef = program.getOrCacheCallerRef(op.toString(), {
          program.stmts.addOne(TheoryMacro(op.toString()));
          Ref(program.stmts.length - 1)
        })

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(program, scope, x)
          operandRefs.addOne(loc)
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, operandRefs.toList))

        appRef
      }

      case ModuleNextCallExpr(expr) => {
        // get the instance
        val instanceRef = exprToTerm(program, scope, expr)
        // get the module it belongs to
        val modRef = getTermTypeRef(program, instanceRef)
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
        val instanceRef = scope.getFieldRef(id.name)
        // get the module it belongs to
        val modRef = getTermTypeRef(program, instanceRef)
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
        val expRef = exprToTerm(program, scope, exp)

        val asConstRef = program.getOrCacheCallerRef("as const", {
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
    program: TermGraph, // modified
    scope: ScopeFrame,
    stmt: Statement
  ): Ref =
    program.getOrCacheCallerRef(
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
                val modRef = scope.getOutputSort()
                val ctrRef = program
                  .stmts(modRef.loc)
                  .asInstanceOf[middle.Module]
                  .ct
                val selRefs =
                  program
                    .stmts(ctrRef.loc)
                    .asInstanceOf[Constructor]
                    .selectors

                val components = selRefs.map { s =>
                  val sel = program.stmts(s.loc).asInstanceOf[Selector]
                  lhs.expr match {
                    case Identifier(name) if name == sel.name =>
                      exprToTerm(program, scope, rhs)
                    case _ => scope.getFieldRef(sel.name)
                  }
                }

                val bodyRef = Ref(program.stmts.length)
                program.stmts.addOne(Application(ctrRef, components))

                val macroRef = Ref(program.stmts.length)
                program.stmts.addOne(
                  UserMacro(
                    s"stmt!${stmt.astNodeId}",
                    scope.getOutputSort(),
                    bodyRef,
                    scope.getParams()
                  )
                )

                program.cacheCallerRef(stmt.astNodeId.toString(), macroRef)

                macroRef
              }
              case OperatorApplication(
                  PolymorphicSelect(field),
                  expr :: Nil
                  ) => {
                val innerStateRef = exprToTerm(program, scope, expr)
                val innerModRef = getTermTypeRef(program, innerStateRef)
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

                innerGetters.map(p => scope.addFieldRef(p._1, p._2))
                val oldOutputRef = scope.getOutputSort()
                scope.setOutputSort(innerModRef)

                val newLhs: Lhs = Lhs(field)

                // create the inner function call
                val innerFuncRef =
                  stmtToTerm(
                    program,
                    scope,
                    AssignStmt(List(newLhs), List(rhs))
                  )

                scope.setOutputSort(oldOutputRef)
                innerGetters.map(p => scope.removeFieldRef(p._1))

                val modRef = scope.getOutputSort()
                val ctrRef = program
                  .stmts(modRef.loc)
                  .asInstanceOf[middle.Module]
                  .ct
                val selRefs =
                  program
                    .stmts(ctrRef.loc)
                    .asInstanceOf[Constructor]
                    .selectors

                val components = selRefs.map {
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
                      case _ => scope.getFieldRef(sel.name)
                    }
                }

                val bodyRef = Ref(program.stmts.length)
                program.stmts.addOne(Application(ctrRef, components))

                val macroRef = Ref(program.stmts.length)
                program.stmts.addOne(
                  UserMacro(
                    s"stmt!${stmt.astNodeId}",
                    scope.getOutputSort(),
                    bodyRef,
                    scope.getParams()
                  )
                )

                program.cacheCallerRef(stmt.astNodeId.toString(), macroRef)

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

  def encodeStatements(
    program: TermGraph, // modified
    scope: ScopeFrame,
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
        .stmts(scope.getStateParam().loc)
        .asInstanceOf[FunctionParameter]
        .sort
      if (inputSort == scope.getOutputSort()) {
        scope.getStateParam()
      } else {
        // TODO: we can't just return the input because the output state is different
        scope.getStateParam()
      }
    }
  }

  // encode a transition block and return a pointer to the function definition
  def transitionBlockToTerm(
    program: TermGraph, // modified
    scope: ScopeFrame,
    funcName: String,
    block: BlockStmt
  ): Ref = {

    val bodyRef = encodeStatements(
      program,
      scope,
      block.stmts
    )

    val transitionBlockRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        funcName,
        scope.getOutputSort(),
        bodyRef,
        scope.getParams()
      )
    )

    transitionBlockRef
  }

  def encodeControlBlock(
    program: TermGraph, // modified
    scope: ScopeFrame,
    moduleName: String,
    cmds: List[GenericProofCommand]
  ): Unit =
    cmds.foreach(p =>
      p.name.name match {
        case "induction" => {
          // create all the variables you need
          val variables = scope.getParams().map { p =>
            program.stmts(p.loc) match {
              case FunctionParameter(name, sort) => {
                val vRef = Ref(program.stmts.length)
                program.stmts.addOne(UserFunction(name, sort))
                vRef
              }
            }
          }

          // get the module declaration
          val mod = program
            .stmts(program.getType(moduleName).get.loc)
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

          val negRef = program.getOrCacheCallerRef("not", {
            program.stmts.addOne(TheoryMacro("not"))
            Ref(program.stmts.length - 1)
          })

          val baseRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(initSpecRef)))

          // induction step
          // holds on entry
          val entryRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(variables(0))))

          // TODO: k transitions
          val transRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(nextRef, variables))

          // holds on exit
          val exitRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(transRef)))

          val negExitRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(exitRef)))

          val andRef = program.getOrCacheCallerRef("and", {
            program.stmts.addOne(TheoryMacro("and"))
            Ref(program.stmts.length - 1)
          })

          val inductiveRef = Ref(program.stmts.length)
          program.stmts.addOne(
            Application(andRef, List(entryRef, negExitRef))
          )

          val orRef = program.getOrCacheCallerRef("or", {
            program.stmts.addOne(TheoryMacro("or"))
            Ref(program.stmts.length - 1)
          })
          val disjRef = Ref(program.stmts.length)
          program.stmts.addOne(
            Application(orRef, List(baseRef, inductiveRef))
          )

          program.assertions.addOne(disjRef)

        }
      }
    )

  // get all the specs and create a function from them
  def specsToTerm(
    program: TermGraph, // modified
    scope: ScopeFrame,
    funcName: String,
    properties: List[SpecDecl]
  ) = {
    // spec needs Bool, so add Bool if it's not already there
    val boolRef = program.getOrCacheSortRef("Bool", {
      program.stmts.addOne(TheorySort("Bool"));
      Ref(program.stmts.length - 1)
    })

    val specConjuncts = new ListBuffer[Ref]()

    val bodyRef = if (properties.length > 1) {
      val andRef = program.getOrCacheCallerRef("and", {
        program.stmts.addOne(TheoryMacro("and"));
        Ref(program.stmts.length - 1)
      })
      properties.foreach { d =>
        val t = exprToTerm(program, scope, d.expr)
        specConjuncts.addOne(t)
        t
      }
      program.stmts.addOne(Application(andRef, specConjuncts.toList))
      Ref(program.stmts.length - 1)
    } else if (properties.length == 1) {
      exprToTerm(program, scope, properties(0).expr)
    } else {
      val trueRef = program.getOrCacheCallerRef("true", {
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
        List(scope.getStateParam())
      )
    )

    specRef
  } // end helper specsToTerm

  def createInitCalls(
    program: TermGraph, // modified
    scope: ScopeFrame,
    pair: (Identifier, Type)
  ): Option[(Expr)] =
    pair._2 match {
      case _: PrimitiveType        => None
      case UndefinedType()         => None
      case UninterpretedType(name) => None
      case EnumType(ids_)          => None
      case _: ProductType =>
        throw new IllegalArgumentException(
          s"product types not supported yet: ${pair._2}"
        )
      case MapType(inTypes, outType) => None
      case ArrayType(inTypes, outType) =>
        createInitCalls(program, scope, (pair._1, outType)) match {
          case Some(e) =>
            Some(
              ConstArray(e, pair._2)
            )
          case None => None
        }
      case SynonymType(id) => {
        val sortRef = program.getType(id.name).get
        program.stmts(sortRef.loc) match {
          case _: middle.Module =>
            // get the sort to see if we need to create a new instance
            val fieldRef = scope.getFieldRef(pair._1.name)
            val fieldSortRef = getTermTypeRef(program, fieldRef)
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
                scope.addFieldRef(tmpName, fp)
                Some(
                  ModuleInitCallExpr(Identifier(tmpName))
                )
              }
            }
          case _ => None
        }
      }
    }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    program: TermGraph, // modified
    scope: ScopeFrame,
    m: front.Module
  ): Ref = {

    scope.resetFrame()

    val fields =
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs ++ m.constants)

    // add placeholder for module and remember where it is
    val moduleRef = Ref(program.stmts.length)
    program.stmts.addOne(
      middle.Module(m.id.name, Ref(-1), Ref(-1), Ref(-1), Ref(-1))
    )
    program.cacheSortRef(m.id.name, moduleRef)

    // input state
    val inputStateRef = Ref(program.stmts.length)
    program.stmts.addOne(FunctionParameter("in", moduleRef))

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

    scope.setStateParam(inputStateRef)
    scope.setOutputSort(moduleRef)
    selectorTerms.map(p => scope.addFieldRef(p._1, p._2))

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
    val initRef = transitionBlockToTerm(
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
    val nextRef = transitionBlockToTerm(
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

    moduleRef
  } // End Module to Term

  def run(
    model: List[front.Module],
    main: Option[String]
  ): TermGraph = {

    val program = new TermGraph(ArrayBuffer[Instruction]())
    val scope = new ScopeFrame(Ref(-1), List.empty, Ref(-1), HashMap.empty)

    // 1. Add every module to the program.
    // 2. When we find the main module, execute it
    val result = model.foreach { m =>
      val mainRef = moduleToTerm(program, scope, m)
      if (Some(m.id.name) == main) {
        encodeControlBlock(program, scope, m.id.name, m.cmds)
      }
    }

    program
  }
}
