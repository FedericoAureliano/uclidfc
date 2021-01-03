package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

import front._

class CacheStack() {
  // point type to program location (modules are types)
  val sortCache: Stack[HashMap[Type, Ref]] = new Stack[HashMap[Type, Ref]]()

  // point expr to program location
  val objectCache: Stack[HashMap[TermNode, Ref]] =
    new Stack[HashMap[TermNode, Ref]]()
}

class Interfaced(stmts: ArrayBuffer[Instruction]) extends Writable(stmts) {
  // after generating assertions, we will traverse to finds calls of ._1 and add assertions that call ._2 with the same arguments
  val inlineAssumes: HashMap[Ref, Ref] = new HashMap()
  val inlineAsserts: HashMap[Ref, Ref] = new HashMap()

  val cache = new CacheStack()
  pushCache()

  def addAssertion(ass: Ref, suffix: String): Unit = {
    val alreadyVisitedForAssertions = new HashSet[Ref]()
    var inlineID = 0
    val assumes = new ListBuffer[Ref]()
    val asserts = new ListBuffer[Ref]()

    def updateTerm(position: Ref, updates: Map[Ref, Ref]): Ref = {
      val newPos = stmts(position.loc) match {
        case Application(caller, args) => {
          val newArgs =
            args.map(a => updateTerm(a, updates))
          if (newArgs != args) {
            memoAddInstruction(Application(caller, newArgs))
          } else {
            position
          }
        }
        case a: Ref => updates.getOrElse(a, a)
        case _      => updates.getOrElse(position, position)
      }
      newPos
    }

    def searchInline(
      curr: Ref,
      updates: Map[Ref, Ref]
    ): Unit =
      if (!alreadyVisitedForAssertions.contains(curr)) {
        alreadyVisitedForAssertions.add(curr)
        stmts(curr.loc) match {
          case Application(caller, args) => {
            stmts(caller.loc) match {
              case UserMacro(_, _, body, params) => {
                val newArgs = args.map(a => updateTerm(a, updates))
                if (inlineAsserts.contains(caller)) {
                  // we found an assert!
                  val newName = Some(
                    s"Counterexample_In_Assertion!${inlineID}_${suffix}"
                  )
                  val newAppRef = memoAddInstruction(
                    Application(inlineAsserts(caller), newArgs),
                    newName
                  )
                  if (newAppRef.named == newName) {
                    inlineID += 1
                    asserts.addOne(newAppRef)
                  }
                }
                if (inlineAssumes.contains(caller)) {
                  // we found an assume!
                  val newName = Some(s"Assumption_Holds!${inlineID}_${suffix}")
                  val newAppRef = memoAddInstruction(
                    Application(inlineAssumes(caller), newArgs),
                    newName
                  )
                  if (newAppRef.named == newName) {
                    inlineID += 1
                    assumes.addOne(newAppRef)
                  }
                }
                // update bindings
                val bindings = params.zip(newArgs).toMap
                // recurse search into body
                searchInline(body, bindings)
              }
              case _ => // do nothing
            }
            // keep searching in children
            args.foreach(p => searchInline(p, updates))
          }
          case _ => // do nothing
        }
      }

    if (!inlineAsserts.isEmpty || !inlineAssumes.isEmpty) {
      searchInline(ass, Map.empty)
    }

    val combined = if (asserts.length > 0) {
      val orRef = memoAddInstruction(TheoryMacro("or"))

      val appRef = memoAddInstruction(
        Application(orRef, List(ass) ++ asserts.toList)
      )

      appRef
    } else {
      ass
    }

    assertionRefs.addOne(combined)
    assumptionRefs.addAll(assumes)
  }

  def pushCache(): Unit = {
    cache.sortCache.push(new HashMap[Type, Ref]())
    cache.objectCache.push(new HashMap[TermNode, Ref]())
  }

  def popCache(): Unit = {
    cache.sortCache.pop()
    cache.objectCache.pop()
    // don't pop auxParams, these need to accumulate
  }

  var uniqueId = 0

  def freshSymbolName(): String = {
    uniqueId += 1
    s"nondet!${uniqueId}"
  }

  def saveSortRef(typ: Type, sort: Ref): Unit =
    cache.sortCache.top.addOne((typ, sort))

  def loadSortRef(typ: Type): Option[Ref] = {
    cache.sortCache.foreach { cache =>
      cache.get(typ) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def saveObjectRef(term: TermNode, obj: Ref): Unit =
    cache.objectCache.top.addOne((term, obj))

  def loadObjectRef(term: TermNode): Option[Ref] = {
    cache.objectCache.foreach { cache =>
      cache.get(term) match {
        case Some(value) => return Some(value)
        case None        =>
      }
    }
    return None
  }

  def loadOrSaveObjectRef(term: TermNode, obj: => Ref): Ref =
    loadObjectRef(term) match {
      case Some(value) => value
      case None        => val r = obj; saveObjectRef(term, r); r
    }

  // encode a type use (adds to program if type not yet used)
  // and return a pointer to the type
  def typeUseToSortRef(
    t: Type
  ): Ref =
    t match {
      case BooleanType() => memoAddInstruction(TheorySort("Bool"))
      case IntegerType() => memoAddInstruction(TheorySort("Int"))
      case ArrayType(inType, outType) => {
        val args =
          (List(inType, outType)).map(arg => typeUseToSortRef(arg))
        memoAddInstruction(TheorySort("Array", args))
      }
      case NamedType(_) =>
        loadSortRef(t)
          .getOrElse(
            throw new TypeOutOfScope(t)
          )
      case _ =>
        throw new NotSupportedYet(t)
    }

  def getTypeFromExpr(exp: Expr): Type = {
    val termRef = exprToTerm(exp)._1
    val typeRef = inferTermType(termRef)
    sortToType(typeRef)
  }

  def sortToType(sortRef: Ref): InlineType =
    stmts(sortRef.loc) match {
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
  def exprToTerm(
    expr: Expr
  ): (Ref, List[Ref]) = {
    var newNondets: List[Ref] = List.empty
    expr match {
      case id: Identifier => {
        // find selector
        loadObjectRef(id) match {
          case Some(value) => (value, newNondets)
          case None        => throw new IdentifierOutOfScope(id)
        }
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
        val opRef = loadOrSaveObjectRef(
          PolymorphicSelect(id), {
            val exprCtrRef = {
              val res = exprToTerm(exp)
              stmts(
                inferTermType(res._1).loc
              ).asInstanceOf[AbstractDataType]
                .defaultCtr()
            }
            val ctr = stmts(exprCtrRef.loc).asInstanceOf[Constructor]
            ctr.selectors
              .find { s =>
                val sel = stmts(s.loc).asInstanceOf[Selector]
                sel.name == id.name
              }
              .getOrElse(throw new IdentifierOutOfScope(id))
          }
        )

        val expRef = {
          val res = exprToTerm(exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val appRef = memoAddInstruction(Application(opRef, List(expRef)))

        (appRef, newNondets)
      }

      case OperatorApplication(ConstArray(typ), exp :: Nil) => {
        // BTW: Z3 can handle a non constant argument to the as const function, but CVC4 can't (Dec. 14, 2020)
        val expRef = {
          val res = exprToTerm(exp)
          newNondets = newNondets ++ res._2
          res._1
        }

        val asConstAppRef = loadOrSaveObjectRef(
          ConstArray(typ), {
            val asConstRef = memoAddInstruction(TheoryMacro("as const"))
            val typeRef = typeUseToSortRef(typ)
            memoAddInstruction(Application(asConstRef, List(typeRef)))
          }
        )

        val appRef = memoAddInstruction(
          Application(asConstAppRef, List(expRef))
        )

        (appRef, newNondets)
      }

      case OperatorApplication(ForallOp(ids), operand :: Nil) => {
        pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          saveObjectRef(p._1, selRef)
          selRef
        }

        val opRef = memoAddInstruction(TheoryMacro("forall", selecorRefs))
        saveObjectRef(ForallOp(ids), opRef)

        val bodyRef = {
          val res = exprToTerm(operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = memoAddInstruction(Application(opRef, List(bodyRef)))

        popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(ExistsOp(ids), operand :: Nil) => {
        pushCache()
        val selecorRefs = ids.map { p =>
          val tyepRef = typeUseToSortRef(p._2)
          val selRef = memoAddInstruction(FunctionParameter(p._1.name, tyepRef))
          saveObjectRef(p._1, selRef)
          selRef
        }

        val opRef = memoAddInstruction(TheoryMacro("exists", selecorRefs))
        saveObjectRef(ExistsOp(ids), opRef)

        val bodyRef = {
          val res = exprToTerm(operand)
          newNondets = newNondets ++ res._2
          res._1
        }

        val resultRef = memoAddInstruction(Application(opRef, List(bodyRef)))

        popCache()

        (resultRef, newNondets)

      }
      case OperatorApplication(op, operands) => {
        val opRef = memoAddInstruction(TheoryMacro(op.name))

        val operandRefs = new ListBuffer[Ref]()
        operands.foreach { x =>
          val loc = exprToTerm(x)
          newNondets = newNondets ++ loc._2
          operandRefs.addOne(loc._1)
        }

        val appRef = memoAddInstruction(Application(opRef, operandRefs.toList))

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

        val appRef = memoAddInstruction(Application(opRef, operandRefs.toList))

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
        val mod = stmts(inferTermType(instanceRef).loc)
          .asInstanceOf[middle.Module]
        // find next function location
        val nextMacro =
          stmts(mod.next.loc).asInstanceOf[UserMacro]

        val extraArgs = nextMacro.params.tail

        val nextCallRef = memoAddInstruction(
          Application(mod.next, List(instanceRef) ++ extraArgs)
        )

        (nextCallRef, newNondets ++ extraArgs)
      }

      case ModuleInitCallExpr(expr) => {
        // get the instance
        val instanceRef = {
          val res = exprToTerm(expr)
          newNondets = newNondets ++ res._2
          res._1
        }
        // get the module it belongs to
        val mod = stmts(inferTermType(instanceRef).loc)
          .asInstanceOf[middle.Module]
        // find init function location
        val initMacro =
          stmts(mod.init.loc).asInstanceOf[UserMacro]

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
  def assignToMacro(
    stateParam: Ref,
    ctrRef: Ref, // this is the constructor to build the target object
    stmt: AssignStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
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
          case Identifier(_) => {
            // get the constructor
            val ctr = stmts(ctrRef.loc)
              .asInstanceOf[Constructor]

            val components: List[Ref] = ctr.selectors.map { s =>
              val sel = stmts(s.loc).asInstanceOf[Selector]
              lhs match {
                // if we are looking at the current selector, then process it, otherwise just return the identity
                case Identifier(name) if name == sel.name =>
                  val res = exprToTerm(rhs)
                  newParams ++= res._2
                  res._1
                case _ =>
                  loadObjectRef(Identifier(sel.name)) match {
                    case Some(value) => value
                    case None =>
                      throw new IdentifierOutOfScope(
                        Identifier(sel.name)
                      ) // todo: how to preserve position?
                  }
              }
            }
            // TODO how to check for bad names? If components all stayed the same?

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

            saveObjectRef(stmt, macroRef)

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
              stmts(
                inferTermType(res._1).loc
              ).asInstanceOf[AbstractDataType]
                .defaultCtr()
            }

            val exprCtr =
              stmts(exprCtrRef.loc).asInstanceOf[Constructor]

            // now get the components
            val components: List[Expr] = exprCtr.selectors.map { s =>
              val sel = stmts(s.loc).asInstanceOf[Selector]
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

  def blockToMacro(
    stateParam: Ref,
    ctrRef: Ref, // current module constructor
    block: BlockStmt
  ): (Ref, List[Ref]) = {
    var newParams: List[Ref] = List.empty
    var mostRecentParams: List[Ref] = List.empty

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
        stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
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
    val leftAppRef = memoAddInstruction(
      Application(left._1, List(stateParam) ++ left._2)
    )

    val right = stmtToMacro(stateParam, ctrRef, ifelse.elseblock)
    val rightAppRef = memoAddInstruction(
      Application(right._1, List(stateParam) ++ right._2)
    )

    val cond = exprToTerm(ifelse.cond)

    val iteRef = memoAddInstruction(TheoryMacro("ite"))

    val bodyRef = memoAddInstruction(
      Application(iteRef, List(cond._1, leftAppRef, rightAppRef))
    )

    val macroRef = memoAddInstruction(
      UserMacro(
        s"line${ifelse.pos.line}col${ifelse.pos.column}!${ifelse.astNodeId}",
        stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
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
          AssignStmt(h.toHavoc, FreshLit(getTypeFromExpr(h.toHavoc)))
        )
      }
      case n: ModuleNextCallStmt => {
        assignToMacro(
          stateParam,
          ctrRef,
          AssignStmt(n.expr, ModuleNextCallExpr(n.expr))
        )
      }
      case a: AssumeStmt => assumeToMacro(stateParam, ctrRef, a)
      case a: AssertStmt => assertToMacro(stateParam, ctrRef, a)
    }

  def assumeToMacro(stateParam: Ref, ctrRef: Ref, ass: AssumeStmt): (
    Ref,
    List[Ref]
  ) = {

    val bodyRef = exprToTerm(ass.pred)._1

    val boolRef = memoAddInstruction(TheorySort("Bool"))

    val funcName = s"line${ass.pos.line}col${ass.pos.column}!${ass.astNodeId}"
    // macro we'll use for the assert
    val specMacroRef = memoAddInstruction(
      UserMacro(
        funcName + "!inline",
        boolRef,
        bodyRef,
        List(stateParam)
      )
    )

    // identity macro
    val macroRef = memoAddInstruction(
      UserMacro(
        funcName,
        stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        stateParam,
        List(stateParam)
      )
    )

    // add macroname to watch list
    inlineAssumes.addOne((macroRef, specMacroRef))

    (macroRef, List.empty)
  }

  def assertToMacro(stateParam: Ref, ctrRef: Ref, ass: AssertStmt): (
    Ref,
    List[Ref]
  ) = {

    val bodyRef = exprToTerm(OperatorApplication(NegationOp(), List(ass.pred)))._1

    val boolRef = memoAddInstruction(TheorySort("Bool"))

    val funcName = s"line${ass.pos.line}col${ass.pos.column}!${ass.astNodeId}"
    // macro we'll use for the assert
    val specMacroRef = memoAddInstruction(
      UserMacro(
        funcName + "!inline",
        boolRef,
        bodyRef,
        List(stateParam)
      )
    )

    // identity macro
    val macroRef = memoAddInstruction(
      UserMacro(
        funcName,
        stmts(ctrRef.loc).asInstanceOf[Constructor].sort,
        stateParam,
        List(stateParam)
      )
    )

    // add macroname to watch list
    inlineAsserts.addOne((macroRef, specMacroRef))

    (macroRef, List.empty)
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

    val um = stmts(res._1.loc).asInstanceOf[UserMacro]

    memoUpdateInstruction(
      res._1,
      UserMacro(funcName, um.sort, um.body, um.params)
    )

    res
  }

  def executeControl(
    moduleId: Identifier,
    initParams: List[Ref],
    nextParams: List[Ref],
    cmds: List[Command]
  ): Unit =
    cmds.foreach(p =>
      p match {
        case c: SolverCommand =>
          c match {
            case Check() => checkQuery = true
            case GetValue(vars) => {
              options = options.appended(("produce-models", "true"))
              getValues = Some(vars.map(p => exprToTerm(p)._1))
            }
          }
        case SolverOption(name, option) =>
          options = options.appended((name, option))
        case ProofCommand(name, unwind) => {
          name.name match {
            case "induction" => {
              // create all the variables you need
              val initVariables = initParams.map { p =>
                stmts(p.loc) match {
                  case FunctionParameter(name, sort) => {
                    val vRef =
                      memoAddInstruction(UserFunction(name, sort))
                    vRef
                  }
                }
              }

              // get the module declaration
              val mod = stmts(loadSortRef(NamedType(moduleId)).get.loc)
                .asInstanceOf[middle.Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              // base case
              // apply init
              val initAppRef =
                memoAddInstruction(
                  Application(initRef, initVariables),
                  Some("State_At_Step!0")
                )

              // apply spec to result of init
              val initSpecRef =
                memoAddInstruction(Application(specRef, List(initAppRef)))

              val negRef = memoAddInstruction(TheoryMacro("not"))

              val baseRef =
                memoAddInstruction(
                  Application(negRef, List(initSpecRef)),
                  Some("Counterexample_In_Invariants_BaseCase")
                )

              addAssertion(baseRef, "Step!0")

              // induction step
              // holds on entry
              val entryRef =
                memoAddInstruction(
                  Application(specRef, List(initVariables.head))
                )
              // we can borrow the nondet state from init since we pop between asserts (this lets us just generate the auxiliary arguments when applying next)

              // Take k steps
              val k = unwind.getOrElse(IntLit(1)).literal.toInt

              var transRef = initVariables.head

              (1 to k).foreach {
                i =>
                  val args = nextParams.tail.map { p =>
                    stmts(p.loc) match {
                      case FunctionParameter(name, sort) => {
                        val vRef =
                          memoAddInstruction(
                            UserFunction(s"$name!step!$i", sort)
                          )
                        vRef
                      }
                    }
                  }
                  transRef = memoAddInstruction(
                    Application(nextRef, List(transRef) ++ args),
                    Some(s"State_At_Step!$i")
                  )
              }

              // holds on exit
              val exitRef =
                memoAddInstruction(Application(specRef, List(transRef)))

              val negExitRef =
                memoAddInstruction(Application(negRef, List(exitRef)))

              val andRef = memoAddInstruction(TheoryMacro("and"))

              val inductiveRef = memoAddInstruction(
                Application(andRef, List(entryRef, negExitRef)),
                Some("Counterexample_In_Invariants_Induction_Step")
              )

              addAssertion(inductiveRef, "Induction_Step")

            }
            case "unroll" => {
              // create all the variables you need
              val initVariables = initParams.map { p =>
                stmts(p.loc) match {
                  case FunctionParameter(name, sort) => {
                    val vRef =
                      memoAddInstruction(UserFunction(name, sort))
                    vRef
                  }
                }
              }

              // get the module declaration
              val mod = stmts(loadSortRef(NamedType(moduleId)).get.loc)
                .asInstanceOf[middle.Module]
              val initRef = mod.init
              val nextRef = mod.next
              val specRef = mod.spec

              val initAppRef =
                memoAddInstruction(
                  Application(initRef, initVariables),
                  Some("State_At_Step!0")
                )

              // apply spec to result of init
              val initSpecRef =
                memoAddInstruction(Application(specRef, List(initAppRef)))

              val negRef = memoAddInstruction(TheoryMacro("not"))

              val baseRef =
                memoAddInstruction(
                  Application(negRef, List(initSpecRef)),
                  Some("Counterexample_In_Invariants_Step!0")
                )

              addAssertion(baseRef, "Step!0")

              // Take k steps
              val k = unwind.getOrElse(IntLit(1)).literal.toInt

              var transRef = initAppRef

              (1 to k).foreach {
                i =>
                  val args = nextParams.tail.map { p =>
                    stmts(p.loc) match {
                      case FunctionParameter(name, sort) => {
                        val vRef =
                          memoAddInstruction(
                            UserFunction(s"$name!step!$i", sort)
                          )
                        vRef
                      }
                    }
                  }
                  transRef = memoAddInstruction(
                    Application(nextRef, List(transRef) ++ args),
                    Some(s"State_At_Step!$i")
                  )
                  // holds on exit
                  val exitRef =
                    memoAddInstruction(Application(specRef, List(transRef)))
                  val negExitRef =
                    memoAddInstruction(
                      Application(negRef, List(exitRef)),
                      Some(s"Counterexample_In_Invariants_Step!${i}")
                    )

                  addAssertion(negExitRef, s"Step!${i}")
              }
            }
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
    val boolRef = memoAddInstruction(TheorySort("Bool"))

    val specConjuncts = new ListBuffer[Ref]()

    val bodyRef = if (properties.length > 1) {
      val andRef = memoAddInstruction(TheoryMacro("and"))
      properties.foreach { d =>
        val t = exprToTerm(d.expr)._1 //specs shouldn't create new nondets
        specConjuncts.addOne(t)
        t
      }
      memoAddInstruction(Application(andRef, specConjuncts.toList))
    } else if (properties.length == 1) {
      exprToTerm(properties(0).expr)._1 //specs shouldn't create new nondets
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

  def createInitCalls(
    fields: List[
      (Identifier, InlineType)
    ] // variable we want to init and its type
  ): List[AssignStmt] =
    fields.foldLeft(List.empty: List[AssignStmt])((acc, f) =>
      createInitCallRhs(f._2, f._1) match {
        case Some(value) =>
          acc ++ List(AssignStmt(f._1, value))
        case None => acc
      }
    )

  def createInitCallRhs(typ: InlineType, starter: Expr): Option[Expr] =
    typ match {
      case ArrayType(inType, outType) => {
        val idx = inType.defaultVal() match {
          case Some(default) => default
          case None          => FreshLit(inType)
        }
        val newStarter = OperatorApplication(ArraySelect(), List(starter, idx))
        createInitCallRhs(outType, newStarter) match {
          case Some(value) =>
            Some(OperatorApplication(ConstArray(typ), List(value)))
          case None => None
        }
      }

      case NamedType(_) => {
        val sortRef = loadSortRef(typ).get
        stmts(sortRef.loc) match {
          case _: middle.Module => Some(ModuleInitCallExpr(starter))
          case adt: AbstractDataType => {
            val ctr =
              stmts(adt.defaultCtr().loc).asInstanceOf[Constructor]
            // val selTypes = ctr.selectors.map(s =>
            //   sortToType(stmts(s.loc).asInstanceOf[Selector].sort)
            // )
            // val components = selTypes.map(p => createInitCallRhs(p))

            val components = ctr.selectors.map { s =>
              val sel = stmts(s.loc).asInstanceOf[Selector]
              val newStarter =
                OperatorApplication(
                  PolymorphicSelect(Identifier(sel.name)),
                  List(starter)
                )
              val typ = sortToType(sel.sort)
              (newStarter, createInitCallRhs(typ, newStarter))
            }

            if (components.exists(p => p._2.isDefined)) {
              Some(
                FunctionApplication(
                  Identifier(ctr.name),
                  components.map(p =>
                    p._2 match {
                      case Some(value) => value
                      case None        => p._1
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

  def typeDeclToTerm(td: TypeDecl): Unit =
    td.typ match {
      case None =>
        loadSortRef(NamedType(td.id)) match {
          case Some(_) => throw new TypeOverride(td)
          case None => {
            val uRef = memoAddInstruction(UserSort(td.id.name))
            saveSortRef(
              NamedType(td.id),
              uRef
            )
          }
        }
      case Some(EnumType(variants)) => {
        loadSortRef(NamedType(td.id)) match {
          case Some(_) => throw new TypeOverride(td)
          case None => {
            // add datatype placeholder
            val dtRef = addInstruction(DataType(td.id.name, List.empty))
            // add constructor for each id
            val constructors = variants.map { i =>
              val vRef =
                memoAddInstruction(Constructor(i.name, dtRef, List.empty))
              saveObjectRef(Identifier(i.name), vRef)
              vRef
            }
            memoUpdateInstruction(dtRef, DataType(td.id.name, constructors))
            saveSortRef(NamedType(td.id), dtRef)
          }
        }
      }
      case Some(RecordType(elements)) => {
        loadSortRef(NamedType(td.id)) match {
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
            saveObjectRef(td.id, ctrRef)

            memoUpdateInstruction(dtRef, DataType(td.id.name, List(ctrRef)))

            saveSortRef(NamedType(td.id), dtRef)
          }
        }
      }
      case Some(ConjunctionComposition(_)) => {
        // create a module decl and then process that instead
        val conj = td.typ.get.asInstanceOf[ConjunctionComposition]

        val nextDecl = {
          // if either left or right is a module, then we need to call their nexts
          NextDecl(BlockStmt(conj.fields.map { f =>
            val sortRef = typeUseToSortRef(f._2)
            stmts(sortRef.loc) match {
              case _: Module => Some(ModuleNextCallStmt(Identifier(f._1)))
              case _         => None
            }
          }.flatten))
        }

        val decls =
          conj.fields.map(f => StateVarsDecl(List(Identifier(f._1)), f._2)) ++ List(
            nextDecl
          )

        val mod = ModuleDecl(td.id, decls, List.empty)

        moduleToTerm(mod, false)
      }
      case Some(DisjunctionComposition(_)) => {
        // create a module decl and then process that instead
        val conj = td.typ.get.asInstanceOf[DisjunctionComposition]

        val fields = new ListBuffer[(String, InlineType)]()
        fields.addAll(conj.fields)

        val nextDecl = {
          // if either left or right is a module, then we need to call their nexts
          // unlike conjunction composition, we nondeterministically choose when to step
          NextDecl(BlockStmt(fields.toList.foldLeft(List.empty[Statement]) {
            (acc, f) =>
              val sortRef = typeUseToSortRef(f._2)
              stmts(sortRef.loc) match {
                case _: Module => {
                  // create a fresh boolean variable
                  val name = freshSymbolName()
                  fields.addOne(name, BooleanType())

                  // use this fresh variable to decide when to step
                  val ifstmt = IfElseStmt(
                    Identifier(name),
                    ModuleNextCallStmt(Identifier(f._1)),
                    BlockStmt(List.empty)
                  )

                  // havoc the variable for next time
                  val havoc = HavocStmt(Identifier(name))

                  acc ++ List(ifstmt, havoc)
                }
                case _ => acc
              }
          }))
        }

        val decls = fields.toList.map(f =>
          StateVarsDecl(List(Identifier(f._1)), f._2)
        ) ++ List(nextDecl)

        val mod = ModuleDecl(td.id, decls, List.empty)

        moduleToTerm(mod, false)
      }
      case Some(_) => {
        // assign td.id (lhs) to whatever id is pointing to
        loadSortRef(NamedType(td.id)) match {
          case Some(_) =>
            throw new TypeOverride(td)
          case None =>
            saveSortRef(
              NamedType(td.id),
              typeUseToSortRef(td.typ.get)
            )
        }
      }
    }

  def functionDeclToTerm(fd: FunctionDecl): Unit = {
    val typeRefs =
      (List(fd.retTyp) ++ fd.argTypes).map(t => typeUseToSortRef(t))
    val funcRef = memoAddInstruction(
      UserFunction(fd.id.name, typeRefs.head, typeRefs.tail)
    )
    saveObjectRef(fd.id, funcRef)
  }

  def defineDeclToTerm(dd: DefineDecl): Unit = {
    pushCache()
    val params = dd.params.map { a =>
      val typeRef = typeUseToSortRef(a._2)
      val selRef = memoAddInstruction(FunctionParameter(a._1.name, typeRef))

      saveObjectRef(a._1, selRef)

      selRef
    }
    val typeRef = typeUseToSortRef(dd.retTyp)
    val bodyRef = exprToTerm(dd.expr)._1 // defines cannot create new variables
    popCache()
    val funcRef = memoAddInstruction(
      UserMacro(dd.id.name, typeRef, bodyRef, params)
    )
    saveObjectRef(dd.id, funcRef)
  }

  def synthesisDeclToTerm(sy: SynthesisDecl): Unit = {
    val params = sy.params.map { a =>
      val typeRef = typeUseToSortRef(a._2)
      val selRef = memoAddInstruction(FunctionParameter(a._1.name, typeRef))

      saveObjectRef(a._1, selRef)

      selRef
    }
    val typeRef = typeUseToSortRef(sy.retTyp)
    val funcRef = memoAddInstruction(
      Synthesis(sy.id.name, typeRef, params)
    )

    saveObjectRef(sy.id, funcRef)
  }

  // encode the module and return a pointer to the start of the encoding
  def moduleToTerm(
    m: ModuleDecl,
    execute: Boolean
  ): Unit = {

    // deal with type declarations
    m.typeDecls.foreach(t => typeDeclToTerm(t))

    // add placeholder for module and remember where it is
    val moduleRef = addInstruction(
      middle.Module(
        m.id.name,
        Ref(-1, None),
        Ref(-1, None),
        Ref(-1, None),
        Ref(-1, None)
      )
    )

    loadSortRef(NamedType(m.id)) match {
      case Some(_) => throw new ModuleOverride(m)
      case None    => saveSortRef(NamedType(m.id), moduleRef)
    }

    // input state
    val inputStateRef = memoAddInstruction(FunctionParameter("in", moduleRef))

    val fields =
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs)

    // create selectors and remember where they are
    val selectorTerms = new HashMap[String, Ref]()
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

    // functions cant be updated, but they are in the scope of the module
    m.functions.foreach(p => functionDeclToTerm(p))

    // defines and constant literals
    m.defines.foreach(p => defineDeclToTerm(p))

    // defines and constant literals
    m.synthesis.foreach(p => synthesisDeclToTerm(p))

    // add constructor and remember where it is
    val constructorRef = memoAddInstruction(
      Constructor(m.id.name, moduleRef, selectorRefs)
    )
    saveObjectRef(m.id, constructorRef)

    pushCache()
    selectorTerms.map(p => saveObjectRef(Identifier(p._1), p._2))

    // update module with constructor; will need to update it fully at the end
    memoUpdateInstruction(
      moduleRef,
      middle.Module(
        m.id.name,
        constructorRef,
        Ref(-1, None),
        Ref(-1, None),
        Ref(-1, None)
      )
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
    memoUpdateInstruction(
      moduleRef,
      middle.Module(m.id.name, constructorRef, initRef, nextRef, specRef)
    )

    if (execute) {
      executeControl(m.id, initParams, nextParams, m.cmds)
    }

    popCache()
  } // End Module to Term
}
