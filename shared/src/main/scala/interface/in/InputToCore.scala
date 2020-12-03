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

  def modelToProgram(model: List[front.Module], main: Option[String]): Program = {

    val program = new Program(ArrayBuffer[Instruction](), 0)

    val moduleLocation = new HashMap[String, Ref]()
    val typeLocation = new HashMap[String, Ref]()
    val opLocation = new HashMap[String, Ref]()
    val varLocation = new Stack[HashMap[String, Ref]]()

    // 1. Add every module to the program. 
    // 2. When we find the main module, point the head to it
    model.foreach { m =>
      if (Some(m.id.name) == main) {
        program.head = program.stmts.length
      }
      moduleToTerm(m)
    }

    // **** Helper Functions ****

    def moduleToTerm(m: front.Module) = {

      // add placeholder for module and remember where it is
      val moduleRef = Ref(program.stmts.length)
      program.stmts.addOne(core.Module(m.id.name, Ref(-1), Ref(-1), Ref(-1), Ref(-1)))

      // add placeholder for datatype and remember where it is
      val datatypeRef = Ref(program.stmts.length)
      program.stmts.addOne(DataType(m.id.name + "!type", List(Ref(-1))))
      typeLocation.addOne((m.id.name + "!type", datatypeRef))

      // add placeholder for constructor and remember where it is
      val constructorRef = Ref(program.stmts.length)
      program.stmts.addOne(Constructor(m.id.name, Ref(-1), List.empty))

      // spec needs Bool, so add Bool if it's not already there
      typeLocation.getOrElseUpdate("Bool", {program.stmts.addOne(TheorySort("Bool")); Ref(program.stmts.length - 1)})

      // create selectors and remember where they are
      val selectorRefs = new ListBuffer[Ref]()
      (m.vars ++ m.sharedVars ++ m.inputs ++ m.outputs ++ m.constants).foreach {
        v => {
          selectorRefs.addOne(Ref(program.stmts.length))
          varDeclToTerm(v)
        }
      }

      // update the datatype placeholder and the constructor placeholder
      program.stmts.update(datatypeRef.loc, DataType(m.id.name + "!type", List(constructorRef)))
      program.stmts.update(constructorRef.loc, Constructor(m.id.name, datatypeRef, selectorRefs.toList))

      // add init and next
      val initRef = Ref(program.stmts.length)
      val initBlock = m.init match {
        case Some(InitDecl(BlockStmt(vars, stmts))) => BlockStmt(vars, stmts)
        case Some(_) => throw new IllegalArgumentException("must be a block statement")
        case None => BlockStmt(List.empty, List.empty)
      }
      parseTransitionBlock(m.id.name + "!init", initBlock, datatypeRef, constructorRef, selectorRefs.toList)

      val nextRef = Ref(program.stmts.length)
      val nextBlock = m.next match {
        case Some(NextDecl(BlockStmt(vars, stmts))) => BlockStmt(vars, stmts)
        case Some(_) => throw new IllegalArgumentException("must be a block statement")
        case None => BlockStmt(List.empty, List.empty)
      }
      parseTransitionBlock(m.id.name + "!next", nextBlock, datatypeRef, constructorRef, selectorRefs.toList)

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
      
      program.stmts.addOne(FunctionParameter("in", datatypeRef))
      if (m.properties.length > 1) {
        program.stmts.addOne(
          Application(Ref(specRef.loc + 3), specConjuncts.toList)
        ) // PLACEHOLDER
        program.stmts.addOne(TheoryMacro("and"))
        m.properties.foreach { d =>
          specConjuncts.addOne(specRef)
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

      program.stmts.update(moduleRef.loc, core.Module(m.id.name, Ref(1), initRef, nextRef, specRef))
    } // End Module to Term

    // define a helper function that does the work
    def parseTransitionBlock(funcName: String, block: BlockStmt, dtRef: Ref, ctRef: Ref, selectorRefs: List[Ref]) = {
      val parseStartPos = program.stmts.length
      // add the macro definition with forward references we will fill in later
      program.stmts.addOne(
        UserMacro(
          funcName,
          dtRef,
          Ref(parseStartPos + 2),
          List(Ref(parseStartPos + 1))
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
                case AssignStmt(lhss, rhss) => {
                  acc ++ lhss.zip(rhss)
                }
                case _ =>
                  throw new IllegalArgumentException(
                    "must be an assignment"
                  )
              }
            )
          val found = flat.filter(p => p._1.ident.name == s.name)
          if (found.length > 0) {
            // once you have the assignment, put the term in the right slot
            components.addOne(Ref(program.stmts.length))
            exprToTerm(
              found(0)._2,
              Ref(parseStartPos + 1),
              selectorRefs.toList
            )
          } else {
            // if there is no term, then just keep whatever was in the input
            components.addOne(Ref(program.stmts.length))
            program.stmts.addOne(Application(selectorRefs(i), List(Ref(parseStartPos + 1))))
          }
        }
        case (_, _) =>
          throw new IllegalArgumentException("must be (s : Selector, i: Int)")
      }
      program.stmts.update(
        parseStartPos + 2,
        Application(ctRef, components.toList)
      ) // apply constructor to the expressions above (FILLING-IN PLACEHOLDER)
    }

    def exprToTerm(
      expr: Expr,
      in: Ref,
      selectors: List[Ref]
    ) : Unit = {
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
        }
        case l: Literal => {
          program.stmts.addOne(TheoryMacro(l.toString()))
        }
        case OperatorApplication(op, operands) => {
          val operandRefs = new ListBuffer[Ref]()
          val appRef = Ref(program.stmts.length)
          program.stmts.addOne(Application(Ref(appRef.loc + 1), operandRefs.toList))
          program.stmts.addOne(TheoryMacro(op.toString()))

          operands.foreach { x =>
            operandRefs.addOne(Ref(program.stmts.length))
            exprToTerm(x, in, selectors)
          }

          program.stmts.update(appRef.loc, Application(Ref(appRef.loc + 1), operandRefs.toList))
        }

        case _ => throw new IllegalArgumentException("not implemented yet")
      }
    }

    def typeUseToTerm(t: Type) : Ref = {
      t match {
        case UninterpretedType(name) => typeLocation.getOrElseUpdate(name.name, {program.stmts.addOne(UserSort(name.name)); Ref(program.stmts.length - 1)})
        case BooleanType()           => typeLocation.getOrElseUpdate("Bool", {program.stmts.addOne(TheorySort("Bool")); Ref(program.stmts.length - 1)})
        case IntegerType()           => typeLocation.getOrElseUpdate("Int", {program.stmts.addOne(TheorySort("Int")); Ref(program.stmts.length - 1)})
        case StringType()            => typeLocation.getOrElseUpdate("String", {program.stmts.addOne(TheorySort("String")); Ref(program.stmts.length - 1)})
        case BitVectorType(width) => {
          typeLocation.getOrElseUpdate("String", {program.stmts.addOne(TheorySort("_ BitVec", List(Ref(1)))); program.stmts.addOne(Numeral(width)); Ref(program.stmts.length - 2)})
        }
        case _ =>
          throw new IllegalArgumentException(s"type not yet supported: ${t}")
      }
    }

    def varDeclToTerm(v: (Identifier, Type)) = {
      val loc = program.stmts.length
      program.stmts.addOne(Selector(v._1.name, Ref(-1))) // placegolder
      val placeholder = typeUseToTerm(v._2)
      program.stmts.update(loc, Selector(v._1.name, placeholder))
    } 

    // End helper function definitions and return the middle.core program we built
    program
  }
}
