package interface.in

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

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
  ConstArray,
  EnumType,
  Expr,
  ExternalType,
  FuncApplication,
  HavocStmt,
  Identifier,
  IfElseStmt,
  InitDecl,
  IntegerType,
  Lhs,
  LhsId,
  Literal,
  MapType,
  ModuleCallExpr,
  ModuleCallStmt,
  ModuleInstanceType,
  ModuleType,
  NextDecl,
  OperatorApplication,
  RecordType,
  SkipStmt,
  StringType,
  SynonymType,
  Tuple,
  TupleType,
  Type,
  UndefinedType,
  UninterpretedType
}

package object ast {

  def modelToProgram(
    model: List[front.Module],
    main: Option[String]
  ): Program = {

    val program = new Program(ArrayBuffer[Instruction](), 0)

    // point type name to type location (modules are types)
    val typeLocation = new HashMap[String, Ref]()
    // point operator name to operator location
    val opLocation = new HashMap[String, Ref]()
    // point variable name to variable location
    val varLocation = new HashMap[String, Ref]()

    // 1. Add every module to the program.
    // 2. When we find the main module, point the head to it
    var head = Ref(0)
    model.foreach { m =>
      head = moduleToTerm(m)
      if (Some(m.id.name) == main) {
        program.head = head.loc
      }
    }

    // **** Helper Functions ****
    // Every helper function adds to the program (side effect) and
    // returns the position that should be pointed to be the caller

    // encode the module and return a pointer to the start of the encoding
    def moduleToTerm(m: front.Module): Ref = {
      // add placeholder for module and remember where it is
      val moduleRef = Ref(program.stmts.length)
      program.stmts.addOne(
        core.Module(m.id.name, Ref(-1), Ref(-1), Ref(-1), Ref(-1))
      )
      typeLocation.addOne((m.id.name, moduleRef))

      // add placeholder for constructor and remember where it is
      val constructorRef = Ref(program.stmts.length)
      program.stmts.addOne(
        Constructor(m.id.name + "!cntr", Ref(-1), List.empty)
      )

      // spec needs Bool, so add Bool if it's not already there
      typeLocation.getOrElseUpdate("Bool", {
        program.stmts.addOne(TheorySort("Bool")); Ref(program.stmts.length - 1)
      })

      // create selectors and remember where they are
      val selectorRefs = new ListBuffer[Ref]()
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs ++ m.constants).foreach {
        v =>
          selectorRefs.addOne(Ref(program.stmts.length))
          varDeclToTerm(v)
      }

      // update the constructor placeholder
      program.stmts.update(
        constructorRef.loc,
        Constructor(m.id.name + "!cntr", moduleRef, selectorRefs.toList)
      )

      // add init and next
      val initRef = Ref(program.stmts.length)
      val initBlock = m.init match {
        case Some(InitDecl(BlockStmt(vars, stmts))) => BlockStmt(vars, stmts)
        case Some(_) =>
          throw new IllegalArgumentException("must be a block statement")
        case None => BlockStmt(List.empty, List.empty)
      }
      transitionBlockToTerm(
        m.id.name + "!init",
        initBlock,
        moduleRef,
        constructorRef,
        selectorRefs.toList
      )

      val nextRef = Ref(program.stmts.length)
      val nextBlock = m.next match {
        case Some(NextDecl(BlockStmt(vars, stmts))) => BlockStmt(vars, stmts)
        case Some(_) =>
          throw new IllegalArgumentException("must be a block statement")
        case None => BlockStmt(List.empty, List.empty)
      }
      transitionBlockToTerm(
        m.id.name + "!next",
        nextBlock,
        moduleRef,
        constructorRef,
        selectorRefs.toList
      )

      // Add spec function
      val specRef = Ref(program.stmts.length)

      val specConjuncts = new ListBuffer[Ref]()
      program.stmts.addOne(
        UserMacro(
          m.id.name + "!spec",
          typeLocation.get("Bool").get,
          Ref(specRef.loc + 2),
          List(Ref(specRef.loc + 1))
        )
      )

      program.stmts.addOne(FunctionParameter("in", moduleRef))
      if (m.properties.length > 1) {
        program.stmts.addOne(
          Application(Ref(specRef.loc + 3), specConjuncts.toList)
        ) // PLACEHOLDER
        program.stmts.addOne(TheoryMacro("and"))
        m.properties.foreach { d =>
          specConjuncts.addOne(Ref(program.stmts.length))
          exprToTerm(d.expr, Ref(specRef.loc + 1), selectorRefs.toList)
        }
        program.stmts.update(
          specRef.loc + 2,
          Application(Ref(specRef.loc + 3), specConjuncts.toList)
        ) // UPDATING PLACEHOLDER
      } else if (m.properties.length == 1) {
        exprToTerm(
          m.properties(0).expr,
          Ref(specRef.loc + 1),
          selectorRefs.toList
        )
      } else {
        program.stmts.addOne(TheoryMacro("true"))
      }

      program.stmts.update(
        moduleRef.loc,
        core.Module(m.id.name, constructorRef, initRef, nextRef, specRef)
      )

      moduleRef
    } // End Module to Term

    // encode a transition block and return a pointer to the function definition
    def transitionBlockToTerm(
      funcName: String,
      block: BlockStmt,
      dtRef: Ref,
      ctRef: Ref,
      selectorRefs: List[Ref]
    ): Ref = {
      val transitionBlockPos = program.stmts.length
      // add the macro definition with forward references we will fill in later
      program.stmts.addOne(
        UserMacro(
          funcName,
          dtRef,
          Ref(transitionBlockPos + 2),
          List(Ref(transitionBlockPos + 1))
        )
      )

      // add the function argument
      program.stmts.addOne(FunctionParameter("in", dtRef))
      // add placeholder for body (call to constructor and components, which will be filled in later)
      program.stmts.addOne(
        Application(ctRef, List.empty)
      )

      // save references for later (the arguments to the constructor returned by the init function)
      val components = new ListBuffer[Ref]()
      selectorRefs.map(r => program.stmts(r.loc)).zipWithIndex.foreach {
        case (s: Selector, i: Int) => {
          // find the assignment
          val flat: List[(Lhs, Expr)] =
            block.stmts.foldLeft(List[(Lhs, Expr)]())((acc, p) =>
              p match {
                case AssignStmt(lhss, rhss) => acc ++ lhss.zip(rhss)
                case ModuleCallStmt(id) =>
                  acc ++ List(Tuple2(LhsId(id), ModuleCallExpr(id)))
                case _ =>
                  throw new IllegalArgumentException(
                    s"statement not supported yet: ${p}"
                  )
              }
            )
          val found = flat.filter(p => p._1.ident.name == s.name)
          if (found.length > 0) {
            // once you have the assignment, put the term in the right slot
            components.addOne(Ref(program.stmts.length))
            exprToTerm(
              found(0)._2,
              Ref(transitionBlockPos + 1),
              selectorRefs.toList
            )
          } else {
            // if there is no term, then just keep whatever was in the input
            components.addOne(Ref(program.stmts.length))
            program.stmts.addOne(
              Application(selectorRefs(i), List(Ref(transitionBlockPos + 1)))
            )
          }
        }
        case (_, _) =>
          throw new IllegalArgumentException("must be (s : Selector, i: Int)")
      }
      program.stmts.update(
        transitionBlockPos + 2,
        Application(ctRef, components.toList)
      ) // apply constructor to the expressions above (FILLING-IN PLACEHOLDER)

      Ref(transitionBlockPos)
    }

    // encode a term and return a pointer to the start of the term
    def exprToTerm(
      expr: Expr,
      in: Ref,
      selectors: List[Ref]
    ): Ref =
      expr match {
        case Identifier(name) => {
          // find selector
          val sel = selectors.filter(r =>
            program.stmts(r.loc) match {
              case Selector(n, _) => name == n
              case _              => throw new IllegalArgumentException("must be selector")
            }
          )(0)
          program.stmts.addOne(Application(sel, List(in)))
          Ref(program.stmts.length - 1)
        }
        case l: Literal => {
          program.stmts.addOne(TheoryMacro(l.toString()))
          Ref(program.stmts.length - 1)
        }
        case OperatorApplication(op, operands) => {
          val operandRefs = new ListBuffer[Ref]()
          val appRef = Ref(program.stmts.length)
          program.stmts.addOne(
            Application(Ref(appRef.loc + 1), operandRefs.toList)
          )
          program.stmts.addOne(TheoryMacro(op.toString()))

          operands.foreach { x =>
            val loc = exprToTerm(x, in, selectors)
            operandRefs.addOne(loc)
          }

          program.stmts.update(
            appRef.loc,
            Application(Ref(appRef.loc + 1), operandRefs.toList)
          )

          appRef
        }

        case ModuleCallExpr(id) => {
          // find next function location
          val nref = varLocation.get(id.name) match {
            case Some(r) => {
              // get the selector
              val sl = program.stmts(r.loc).asInstanceOf[Selector]
              // get the datatype
              val mod = program.stmts(sl.sort.loc).asInstanceOf[core.Module]
              mod.next
            }
            case None =>
              throw new IllegalArgumentException(s"module not found: ${id}")
          }

          // apply the next function
          val nextcall = Ref(program.stmts.length)
          program.stmts.addOne(
            Application(nref, List.empty)
          ) // fill in this empty list next

          // find selector
          val sel = varLocation.get(id.name).get
          val arg = Ref(program.stmts.length)
          program.stmts.addOne(Application(sel, List(in)))

          // fill in the placeholder from before
          program.stmts.update(nextcall.loc, Application(nref, List(arg)))

          nextcall
        }
        case _ => throw new IllegalArgumentException("not implemented yet")
      }

    // encode a type use (adds to program if type not yet used)
    // and return a pointer to the type
    def typeUseToTerm(t: Type): Ref =
      t match {
        case UninterpretedType(name) =>
          typeLocation.getOrElseUpdate(name.name, {
            program.stmts.addOne(UserSort(name.name));
            Ref(program.stmts.length - 1)
          })
        case BooleanType() =>
          typeLocation.getOrElseUpdate("Bool", {
            program.stmts.addOne(TheorySort("Bool"));
            Ref(program.stmts.length - 1)
          })
        case IntegerType() =>
          typeLocation.getOrElseUpdate("Int", {
            program.stmts.addOne(TheorySort("Int"));
            Ref(program.stmts.length - 1)
          })
        case StringType() =>
          typeLocation.getOrElseUpdate("String", {
            program.stmts.addOne(TheorySort("String"));
            Ref(program.stmts.length - 1)
          })
        case BitVectorType(width) => {
          typeLocation.getOrElseUpdate("String", {
            program.stmts.addOne(TheorySort("_ BitVec", List(Ref(1))));
            program.stmts.addOne(Numeral(width)); Ref(program.stmts.length - 2)
          })
        }
        case SynonymType(id) =>
          typeLocation.get(id.name) match {
            case Some(value) => value
            case None =>
              typeLocation.get(id.name + "!type") match {
                case Some(value) => value
                case None =>
                  throw new IllegalArgumentException(s"type not declared: ${t}")
              }
          }
        case _ =>
          throw new IllegalArgumentException(s"type not yet supported: ${t}")
      }

    // module variables are selectors.
    // Encode the selector and return a pointer to it
    def varDeclToTerm(v: (Identifier, Type)): Ref = {
      val loc = program.stmts.length
      varLocation.addOne(v._1.name, Ref(loc))
      program.stmts.addOne(Selector(v._1.name, Ref(-1))) // placegolder
      val placeholder = typeUseToTerm(v._2)
      program.stmts.update(loc, Selector(v._1.name, placeholder))

      Ref(loc)
    }

    // End helper function definitions and return the middle.core program we built
    program
  }
}
