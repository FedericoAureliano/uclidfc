package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

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
          t, {
            program.stmts.addOne(TheorySort("Bool"))
            Ref(program.stmts.length - 1)
          }
        )
      case IntegerType() =>
        program.loadOrSaveSortRef(
          t, {
            program.stmts.addOne(TheorySort("Int"))
            Ref(program.stmts.length - 1)
          }
        )
      case ArrayType(inTypes, outType) => {
        program.loadOrSaveSortRef(
          t, {
            val args =
              (inTypes ++ List(outType)).map(arg => typeUseToTerm(arg))
            program.stmts.addOne(TheorySort("Array", args))
            Ref(program.stmts.length - 1)
          }
        )
      }
      case NamedType(_) =>
        program
          .loadSortRef(t)
          .getOrElse(
            throw new TypeOutOfScope(t)
          )
      case _ =>
        throw new NotSupportedYet(t)
    }

  def inferTermType(
    app: Ref
  ): Ref =
    program.stmts(app.loc) match {
      case Application(caller, args) => {
        program.stmts(caller.loc) match {
          case TheoryMacro("ite", _) => inferTermType(args.head)
          case TheoryMacro("store", _) =>
            inferTermType(args.head)
          case TheoryMacro("select", _) => {
            val arrayRef = inferTermType(args.head)
            val arraySort =
              program.stmts(arrayRef.loc).asInstanceOf[TheorySort]
            arraySort.params.last
          }
          case _ => inferTermType(caller)
        }
      }
      case Constructor(_, sort, _)    => sort
      case FunctionParameter(_, sort) => sort
      case Selector(_, sort)          => sort
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

  def sortToType(sortRef: Ref): InlineType =
    program.stmts(sortRef.loc) match {
      case DataType(name, _)        => NamedType(Identifier(name))
      case Module(name, _, _, _, _) => NamedType(Identifier(name))
      case TheorySort(name, params) =>
        name match {
          case "Int"  => IntegerType()
          case "Bool" => BooleanType()
          case "Array" => {
            val paramTypes = params.map(p => sortToType(p))
            ArrayType(List(paramTypes(0)), paramTypes(1))
          }
        }
      case UserSort(name, _) => NamedType(Identifier(name))
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
        program.loadObjectRef(id) match {
          case Some(value) => (value, newNondets)
          case None        => throw new IdentifierOutOfScope(id)
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
        (program.loadOrSaveObjectRef(l, {
          program.stmts.addOne(TheoryMacro(l.value().toString()));
          Ref(program.stmts.length - 1)
        }), newNondets)
      }
      case OperatorApplication(GetNextValueOp(), _) => {
        throw new NotSupportedYet(expr)
      }

      case OperatorApplication(PolymorphicSelect(id), exp :: Nil) => {
        val opRef = program.loadOrSaveObjectRef(PolymorphicSelect(id), {
          program.stmts.addOne(TheoryMacro(id.name));
          Ref(program.stmts.length - 1)
        })

        val expRef = {
          val res = exprToTerm(exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(expRef)))

        (appRef, newNondets)
      }

      case OperatorApplication(ConstArray(typ), exp :: Nil) => {
        // BTW: Z3 can handle a non constant argument to the as const function, but CVC4 can't (Dec. 14, 2020)
        val expRef = {
          val res = exprToTerm(exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val asConstAppRef = program.loadOrSaveObjectRef(
          ConstArray(typ), {
            val asConstRef = Ref(program.stmts.length)
            program.stmts.addOne(TheoryMacro("as const"))
            val typeRef = typeUseToTerm(typ)
            program.stmts.addOne(Application(asConstRef, List(typeRef)))
            Ref(program.stmts.length - 1)
          }
        )

        val appRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(asConstAppRef, List(expRef)))

        (appRef, newNondets)
      }

      case OperatorApplication(ForallOp(ids), operand :: Nil) => {
        program.pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToTerm(p._2)
          val selRef = Ref(program.stmts.length)
          program.stmts.addOne(FunctionParameter(p._1.name, tyepRef))
          program.saveObjectRef(p._1, selRef)
          selRef
        }

        val opRef = {
          program.stmts.addOne(TheoryMacro("forall", selecorRefs));
          Ref(program.stmts.length - 1)
        }
        program.saveObjectRef(ForallOp(ids), opRef)

        val bodyRef = {
          val res = exprToTerm(operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(bodyRef)))

        program.popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(ExistsOp(ids), operand :: Nil) => {
        program.pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToTerm(p._2)
          val selRef = Ref(program.stmts.length)
          program.stmts.addOne(FunctionParameter(p._1.name, tyepRef))
          program.saveObjectRef(p._1, selRef)
          selRef
        }

        val opRef = {
          program.stmts.addOne(TheoryMacro("exists", selecorRefs));
          Ref(program.stmts.length - 1)
        }
        program.saveObjectRef(ExistsOp(ids), opRef)

        val bodyRef = {
          val res = exprToTerm(operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = Ref(program.stmts.length)
        program.stmts.addOne(Application(opRef, List(bodyRef)))

        program.popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(op, operands) => {
        val opRef = program.loadOrSaveObjectRef(op, {
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
      case FunctionApplication(op, operands) => {
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
      case _ => throw new NotSupportedYet(expr)
    } // end helper exprToTerm
  }

  // statements are encoded as functions.
  def assignToMacro(
    stateParam: Ref,
    ctrRef: Ref, // this is the constructor to build the target object
    stmt: AssignStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    (
      program.loadOrSaveObjectRef(
        stmt, {
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
            case Identifier(_) => {
              // get the constructor
              val ctr = program
                .stmts(ctrRef.loc)
                .asInstanceOf[Constructor]

              val components: List[Ref] = ctr.selectors.map {
                s =>
                  val sel = program.stmts(s.loc).asInstanceOf[Selector]
                  lhs match {
                    // if we are looking at the current selector, then process it, otherwise just return the identity
                    case Identifier(name) if name == sel.name =>
                      val res = exprToTerm(rhs)
                      newParams ++= res._2
                      res._1
                    case _ =>
                      program.loadObjectRef(Identifier(sel.name)) match {
                        case Some(value) => value
                        case None =>
                          throw new IdentifierOutOfScope(
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
                  s"line${lhs.pos.line}col${lhs.pos.column}!${stmt.astNodeId}",
                  ctr.sort,
                  bodyRef,
                  List(stateParam) ++ newParams
                )
              )

              program.saveObjectRef(stmt, macroRef)

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
        }
      ),
      newParams
    )
  }

  def blockToMacro(
    stateParam: Ref,
    ctrRef: Ref, // current module constructor
    block: BlockStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    var mostRecentParams: List[Ref] = List.empty

    val bodyRef = if (block.stmts.length > 0) {
      val firstFuncRef = {
        val res =
          stmtToMacro(stateParam, ctrRef, block.stmts.head)
        newParams ++= res._2
        mostRecentParams = res._2
        res._1
      }

      val startRef = Ref(program.stmts.length)
      program.stmts.addOne(
        Application(firstFuncRef, List(stateParam) ++ mostRecentParams)
      )

      block.stmts.tail.foldLeft(startRef) { (acc, stmt) =>
        val funcRef = {
          val res = stmtToMacro(stateParam, ctrRef, stmt)
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
      }
    } else {
      stateParam
    }

    val blockRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        s"line${block.pos.line}col${block.pos.column}!${block.astNodeId}",
        program.stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        bodyRef,
        List(stateParam) ++ newParams
      )
    )

    (blockRef, newParams)
  }

  def ifelseToMacro(
    stateParam: Ref,
    ctrRef: Ref, // current module constructor
    ifelse: IfElseStmt
  ): (Ref, List[Ref]) = {
    val left = stmtToMacro(stateParam, ctrRef, ifelse.ifblock)
    val leftAppRef = Ref(program.stmts.length)
    program.stmts.addOne(Application(left._1, List(stateParam) ++ left._2))

    val right = stmtToMacro(stateParam, ctrRef, ifelse.elseblock)
    val rightAppRef = Ref(program.stmts.length)
    program.stmts.addOne(Application(right._1, List(stateParam) ++ right._2))

    val cond = exprToTerm(ifelse.cond)

    val iteRef = program.loadOrSaveObjectRef(ITEOp(), {
      program.stmts.addOne(TheoryMacro("ite"));
      Ref(program.stmts.length - 1)
    })

    val bodyRef = Ref(program.stmts.length)
    program.stmts.addOne(
      Application(iteRef, List(cond._1, leftAppRef, rightAppRef))
    )

    val macroRef = Ref(program.stmts.length)
    program.stmts.addOne(
      UserMacro(
        s"line${ifelse.pos.line}col${ifelse.pos.column}!${ifelse.astNodeId}",
        program.stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        bodyRef,
        List(stateParam) ++ cond._2 ++ left._2 ++ right._2
      )
    )

    (macroRef, cond._2 ++ left._2 ++ right._2)
  }

  def stmtToMacro(stateParam: Ref, ctrRef: Ref, stmt: Statement): (
    Ref,
    List[Ref]
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
          AssignStmt(h.toHavoc, FreshLit(getTypeFromId(h.toHavoc)))
        )
      }
      case n: ModuleNextCallStmt => {
        assignToMacro(
          stateParam,
          ctrRef,
          AssignStmt(n.expr, ModuleNextCallExpr(n.expr))
        )
      }
    }

  // encode a transition block and return a pointer to the function definition
  def transitionToTerm(
    funcName: String,
    stateParam: Ref,
    ctrRef: Ref,
    block: BlockStmt
  ): (Ref, List[Ref]) = {
    val res = stmtToMacro(
      stateParam,
      ctrRef,
      block
    )

    val um = program.stmts(res._1.loc).asInstanceOf[UserMacro]

    program.stmts
      .update(res._1.loc, UserMacro(funcName, um.sort, um.body, um.params))

    res
  }

  def executeControl(
    moduleId: Identifier,
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
            .stmts(program.loadSortRef(NamedType(moduleId)).get.loc)
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

          val negRef = program.loadOrSaveObjectRef(NegationOp(), {
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
          val k = p.k.getOrElse(IntLit(1)).literal.toInt

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

          val andRef = program.loadOrSaveObjectRef(ConjunctionOp(), {
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
            .stmts(program.loadSortRef(NamedType(moduleId)).get.loc)
            .asInstanceOf[middle.Module]
          val initRef = mod.init
          val nextRef = mod.next
          val specRef = mod.spec

          val initAppRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(initRef, initVariables))

          // apply spec to result of init
          val initSpecRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(specRef, List(initAppRef)))

          val negRef = program.loadOrSaveObjectRef(NegationOp(), {
            program.stmts.addOne(TheoryMacro("not"))
            Ref(program.stmts.length - 1)
          })

          val baseRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(negRef, List(initSpecRef)))

          program.assertions.addOne(baseRef)

          // Take k steps
          val k = p.k.getOrElse(IntLit(1)).literal.toInt

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
    val boolRef = program.loadOrSaveSortRef(BooleanType(), {
      program.stmts.addOne(TheorySort("Bool"));
      Ref(program.stmts.length - 1)
    })

    val specConjuncts = new ListBuffer[Ref]()

    val bodyRef = if (properties.length > 1) {
      val andRef = program.loadOrSaveObjectRef(ConjunctionOp(), {
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
      val trueRef = program.loadOrSaveObjectRef(BoolLit(true), {
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
    fields: List[
      (Identifier, InlineType)
    ] // variable we want to init and its type
  ): List[AssignStmt] =
    fields.foldLeft(List.empty: List[AssignStmt])((acc, f) =>
      createInitCallRhs(f._2) match {
        case Some(value) =>
          acc ++ List(AssignStmt(f._1, value))
        case None => acc
      }
    )

  def createInitCallRhs(typ: InlineType): Option[Expr] =
    typ match {
      case ArrayType(_, outType) => {
        createInitCallRhs(outType) match {
          case Some(value) =>
            Some(OperatorApplication(ConstArray(typ), List(value)))
          case None => None
        }
      }

      case NamedType(_) => {
        val sortRef = program.loadSortRef(typ).get
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
                FunctionApplication(
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

      case BooleanType() | IntegerType() =>
        None // don't need to be inited
    }

  def typeDeclToTerm(typ: TypeDecl): Unit =
    typ.typ match {
      case None =>
        program.loadSortRef(NamedType(typ.id)) match {
          case Some(_) => throw new TypeOverride(typ)
          case None => {
            program.stmts.addOne(UserSort(typ.id.name))
            program.saveSortRef(
              NamedType(typ.id),
              Ref(program.stmts.length - 1)
            )
          }
        }
      case Some(EnumType(variants)) => {
        program.loadSortRef(NamedType(typ.id)) match {
          case Some(_) => throw new TypeOverride(typ)
          case None => {
            // add datatype placeholder
            val dtRef = Ref(program.stmts.length)
            program.stmts.addOne(DataType(typ.id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val vRef = Ref(program.stmts.length)
              program.stmts.addOne(Constructor(i.name, dtRef, List.empty))
              program.saveObjectRef(Identifier(i.name), vRef)
              vRef
            }
            program.stmts.update(dtRef.loc, DataType(typ.id.name, constructors))
            program.saveSortRef(NamedType(typ.id), dtRef)
          }
        }
      }
      case Some(RecordType(elements)) => {
        program.loadSortRef(NamedType(typ.id)) match {
          case Some(_) => throw new TypeOverride(typ)
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
            program.saveObjectRef(typ.id, ctrRef)

            program.stmts.update(dtRef.loc, DataType(typ.id.name, List(ctrRef)))

            program.saveSortRef(NamedType(typ.id), dtRef)
          }
        }
      }
      case Some(_) => {
        // assign typ.id (lhs) to whatever id is pointing to
        program.loadSortRef(NamedType(typ.id)) match {
          case Some(_) =>
            throw new TypeOutOfScope(NamedType(typ.id))
          case None =>
            program.saveSortRef(NamedType(typ.id), typeUseToTerm(typ.typ.get))
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
    program.saveObjectRef(fd.id, funcRef)
  }

  def defineDeclToTerm(dd: DefineDecl): Unit = {
    program.pushCache()
    val params = dd.params.map { a =>
      val typeRef = typeUseToTerm(a._2)
      val selRef = Ref(program.stmts.length)
      program.stmts.addOne(FunctionParameter(a._1.name, typeRef))

      program.saveObjectRef(a._1, selRef)

      selRef
    }
    val typeRef = typeUseToTerm(dd.retTyp)
    val bodyRef = exprToTerm(dd.expr)._1 // defines cannot create new variables
    program.popCache()
    val funcRef = Ref(program.stmts.length)
    program.stmts.addOne(UserMacro(dd.id.name, typeRef, bodyRef, params))
    program.saveObjectRef(dd.id, funcRef)
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

    program.loadSortRef(NamedType(m.id)) match {
      case Some(_) => throw new ModuleOverride(m)
      case None    => program.saveSortRef(NamedType(m.id), moduleRef)
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
    program.saveObjectRef(m.id, constructorRef)

    program.pushCache()
    selectorTerms.map(p => program.saveObjectRef(Identifier(p._1), p._2))

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
    model.foreach { m =>
      m match {
        case td: TypeDecl     => typeDeclToTerm(td)
        case dd: DefineDecl   => defineDeclToTerm(dd)
        case fd: FunctionDecl => functionDeclToTerm(fd)
        case mod: ModuleDecl => {
          val params = moduleToTerm(mod)
          if (Some(mod.id.name) == main) {
            executeControl(mod.id, params._1, params._2, mod.cmds)
          }
        }
      }
    }

    program
  }
}
