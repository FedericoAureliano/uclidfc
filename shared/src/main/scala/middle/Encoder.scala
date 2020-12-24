package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import front._

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
          t.name, {
            program.stmts.addOne(TheorySort("Bool"))
            program.saveSortRef(t.name, Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case IntegerType() =>
        program.loadOrSaveSortRef(
          t.name, {
            program.stmts.addOne(TheorySort("Int"))
            program.saveSortRef(t.name, Ref(program.stmts.length - 1))
            Ref(program.stmts.length - 1)
          }
        )
      case EnumType(id, variants) => {
        program.loadOrSaveSortRef(
          id.name, {
            // add datatype placeholder
            val dtRef = Ref(program.stmts.length)
            program.stmts.addOne(DataType(id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val vRef = Ref(program.stmts.length)
              program.stmts.addOne(Constructor(i.name, dtRef, List.empty))
              program.saveObjectRef(i.name, vRef)
              vRef
            }
            program.stmts.update(dtRef.loc, DataType(id.name, constructors))
            dtRef
          }
        )
      }
      case RecordType(id, elements) => {
        program.loadOrSaveSortRef(
          id.name, {
            // add datatype placeholder
            val dtRef = Ref(program.stmts.length)
            program.stmts.addOne(DataType(id.name, List.empty))

            val selecorRefs = elements.map { p =>
              val tyepRef = typeUseToTerm(program, p._2)
              program.stmts.addOne(Selector(p._1.name, tyepRef))
              Ref(program.stmts.length - 1)
            }

            val ctrRef = Ref(program.stmts.length)
            program.stmts.addOne(Constructor(id.name, dtRef, selecorRefs))
            program.saveObjectRef(id.name, ctrRef)

            program.stmts.update(dtRef.loc, DataType(id.name, List(ctrRef)))

            dtRef
          }
        )
      }
      case ArrayType(inTypes, outType) => {
        program.loadOrSaveSortRef(
          t.name, {
            val args =
              (inTypes ++ List(outType)).map(arg => typeUseToTerm(program, arg))
            program.stmts.addOne(TheorySort("Array", args))
            Ref(program.stmts.length - 1)
          }
        )
      }
      case SynonymType(id) =>
        program
          .loadSortRef(id.name)
          .getOrElse(
            throw new IllegalArgumentException(s"type not declared: ${t}")
          )
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
      case Constructor(name, sort, selectors) => sort
      case FunctionParameter(name, sort)      => sort
      case Selector(name, sort)               => sort
      case _ =>
        throw new IllegalArgumentException(
          s"type inference not yet supported: ${program.stmts(app.loc)}"
        )
    }

  // encode a term and return a pointer to the start of the term
  def exprToTerm(
    program: Program, // modified
    expr: Expr
  ): Ref = {
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

  // statements are encoded as functions.
  def stmtToTerm(
    program: Program, // modified
    params: List[Ref],
    ctrRef: Ref, // this is the constructor to build the target object
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
                // TODO: handle primes correctly (right now we just ignore them)
                stmtToTerm(
                  program,
                  params,
                  ctrRef,
                  AssignStmt(List(Lhs(expr)), List(rhs))
                )
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
                    ctr.sort,
                    bodyRef,
                    params
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
                val exprCtrRef =
                  program
                    .stmts(
                      inferTermType(program, exprToTerm(program, expr)).loc
                    )
                    .asInstanceOf[AbstractDataType]
                    .defaultCtr()

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

                stmtToTerm(
                  program,
                  params,
                  ctrRef,
                  AssignStmt(List(Lhs(expr)), List(newRhs))
                )
              }
              case OperatorApplication(ArraySelect(indices), expr :: Nil) => {
                val newRhs =
                  OperatorApplication(ArrayUpdate(indices, rhs), List(expr))
                stmtToTerm(
                  program,
                  params,
                  ctrRef,
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
    params: List[Ref],
    ctrRef: Ref, // current module constructor
    stmts: List[Statement]
  ): Ref = {
    val flattened =
      flattenStmts(stmts).map(p => AssignStmt(List(p._1), List(p._2)))

    if (flattened.length > 0) {
      val firstFuncRef = stmtToTerm(program, params, ctrRef, flattened.head)

      val startRef = Ref(program.stmts.length)
      program.stmts.addOne(
        Application(firstFuncRef, params)
      )

      flattened.tail.foldLeft(startRef) { (acc, stmt) =>
        val funcRef = stmtToTerm(program, params, ctrRef, stmt)
        val appRef = Ref(program.stmts.length)
        // add the nondet parameters at the end
        program.stmts.addOne(
          Application(funcRef, List(acc) ++ params.tail)
        )
        appRef
      }
    } else {
      params.head
    }
  }

  // encode a transition block and return a pointer to the function definition
  def transitionToTerm(
    program: Program, // modified
    funcName: String,
    params: List[Ref],
    ctrRef: Ref,
    block: BlockStmt
  ): Ref = {

    val bodyRef = blockToTerm(
      program,
      params,
      ctrRef,
      block.stmts
    )

    val transitionBlockRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        funcName,
        program.stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        bodyRef,
        params
      )
    )

    transitionBlockRef
  }

  def executeControl(
    program: Program, // modified
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
    program: Program, // modified
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
        params
      )
    )

    specRef
  } // end helper specsToTerm

  def createInitCalls(
    program: Program, // modified
    params: List[Ref],
    pair: (Identifier, Type)
  ): Option[(Expr, List[Ref])] = {

    var outParams: List[Ref] = List.empty
    pair._2 match {
      case BooleanType()        => None
      case IntegerType()        => None
      case EnumType(_, _)       => None
      case UninterpretedType(_) => None
      case RecordType(id, elements) => {
        // TODO instantiate elements if they are of module type
        None
      }
      case ArrayType(inTypes, outType) =>
        createInitCalls(program, params, (pair._1, outType)).flatMap(e =>
          Some(ConstArray(e._1, pair._2), e._2)
        )
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
                  ModuleInitCallExpr(pair._1),
                  outParams
                )
              case _ => {
                // anything else, create an instance and pipe it in
                // add a function parameter
                val tmpName = program.freshSymbolName()
                val fp = Ref(program.stmts.length)
                program.stmts.addOne(FunctionParameter(tmpName, sortRef))
                outParams = outParams.appended(fp)
                program.saveObjectRef(tmpName, fp)
                Some(
                  ModuleInitCallExpr(Identifier(tmpName)),
                  outParams
                )
              }
            }
          case _ => None
        }
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

  def functionDeclToTerm(program: Program, fd: FunctionDecl): Unit = {
    val typeRefs =
      (List(fd.retTyp) ++ fd.argTypes).map(t => typeUseToTerm(program, t))
    val funcRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserFunction(fd.id.name, typeRefs.head, typeRefs.tail)
    )
    program.saveObjectRef(fd.id.name, funcRef)
  }

  def defineDeclToTerm(program: Program, dd: DefineDecl): Unit = {
    program.pushCache()
    val params = dd.params.map { a =>
      val typeRef = typeUseToTerm(program, a._2)
      val selRef = Ref(program.stmts.length)
      program.stmts.addOne(FunctionParameter(a._1.name, typeRef))

      program.saveObjectRef(a._1.name, selRef)

      selRef
    }
    val typeRef = typeUseToTerm(program, dd.retTyp)
    val bodyRef = exprToTerm(program, dd.expr)
    program.popCache()
    val funcRef = Ref(program.stmts.length)
    program.stmts.addOne(UserMacro(dd.id.name, typeRef, bodyRef, params))
    program.saveObjectRef(dd.id.name, funcRef)
  }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    program: Program, // modified
    m: ModuleDecl
  ): (List[Ref], List[Ref]) = { // returns init params and next params

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
    m.functions.foreach(p => functionDeclToTerm(program, p))

    // defines and constant literals
    m.defines.foreach(p => defineDeclToTerm(program, p))

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

    var initParams = List(inputStateRef)
    // add init and next
    val initInitCalls = fields.foldLeft(List.empty[Statement]) { (acc, p) =>
      createInitCalls(program, initParams, p) match {
        case Some(e) => {
          initParams = initParams ++ e._2
          acc ++ List(AssignStmt(List(Lhs(p._1)), List(e._1)))
        }
        case None => acc
      }
    }
    val initBlock = m.init match {
      case Some(decl) => BlockStmt(decl.body.stmts ++ initInitCalls)
      case None       => BlockStmt(initInitCalls)
    }
    val initRef = transitionToTerm(
      program,
      m.id.name + "!init",
      initParams,
      constructorRef,
      initBlock
    )

    var nextParams = List(inputStateRef)
    val nextBlock = m.next match {
      case Some(decl) => decl.body
      case None       => BlockStmt(List.empty)
    }
    val nextRef = transitionToTerm(
      program,
      m.id.name + "!next",
      nextParams,
      constructorRef,
      nextBlock
    )

    // Add spec function
    val specRef = specsToTerm(
      program,
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
    model: List[Decl],
    main: Option[String]
  ): Program = {

    val program = new Program(ArrayBuffer[Instruction]())

    // 1. Add every module to the program.
    // 2. When we find the main module, execute it
    val result = model.foreach { m =>
      m match {
        case td: TypeDecl     => typeDeclToTerm(program, td)
        case dd: DefineDecl   => defineDeclToTerm(program, dd)
        case fd: FunctionDecl => functionDeclToTerm(program, fd)
        case mod: ModuleDecl => {
          val params = moduleToTerm(program, mod)
          if (Some(mod.id.name) == main) {
            executeControl(program, mod.id.name, params._1, params._2, mod.cmds)
          }
        }
        case _ =>
          throw new IllegalArgumentException(s"must be a top level decl: ${m}")
      }
    }

    program
  }
}
