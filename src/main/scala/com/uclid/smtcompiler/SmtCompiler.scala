package com.uclid.smtcompiler

import com.uclid.context.SyMTContext
import com.uclid.termgraph._

import java.util.HashMap
import scala.collection.mutable.{ListBuffer, Stack}

// val OPERATORS = Set(
//   "and", "or", "not", "=", "=>", ">=", ">", "<", "<=", "ite", 
//   "str.++", "str.indexof", "str.substr", "str.len", "str.contains", "str.prefixof", "str.suffixof", "str.replace", "str.at",
//   "bvadd", "bvsub", "bvand", "bvor", "bvmul", "bvudiv", "bvurem", "bvshl", "bvlshr", "bvnot", "bvneg", "bvult", "concat", 
//   "extract", "bv2nat", "nat2bv", "+", "-", "*", "forall", "exists", "select", "store")

object SmtCompiler {

  def removeComments(script: String): String = {
    val clean = new StringBuffer()

    var inComment = false
    script.foreach { c =>
      c match {
        case '\n'            => inComment = false; clean.append(" ")
        case ';'             => inComment = true
        case a if inComment  =>
        case a if !inComment => clean.append(a)
      }
    }

    clean.toString
  }

  def tokenize(script: String): Array[String] =
    script
      .replace("(", " ( ")
      .replace(")", " ) ")
      .trim()
      .split("\\s+")
      .toArray

  /** Translate smtlib string to termgraph in SMT context
    * 
    */
  def compile(script: String): SyMTContext = {
    var tokens : Array[String] = tokenize(removeComments(script))
    val global: HashMap[String, Int] = new HashMap()
    val local: Stack[Map[String, Int]] = new Stack()
    var termgraph = new TermGraph()
    var ctx = new SyMTContext(termgraph)

    def inLocal(name: String) : Boolean = {
      local.exists(local => {
        local.contains(name)
      })
    }

    def getLocal(name : String) : Int = {
      local.foreach(local => {
        if (local.contains(name)) {
          return local(name)
        }
      })
      throw new SmtParserError(s"$name not in local map!")
    }

    var pos = 0
    var nest = 0

    while (pos < tokens.length) {
      parseCommand()
    }

    def parseCommand(): Unit = {
      require(tokens(pos) == "(", tokens.slice(pos, pos + 2).mkString(" "))
      
      // we don't need much lookahead: just look at the next 3 tokens at a time
      tokens(pos) :: tokens(pos + 1) :: tokens(pos + 2) :: Nil match {
        case "(" :: "assert" :: _ =>
          pos += 2
          val term = parseTerm()
          ctx.addAssertion(term)
          pos += 1
        case "(" :: "declare-fun" :: _ =>
          pos += 2
          val funName = parseName()
          val params = parseSortList()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(funName, sortRef, params))
          global.put(funName, declRef)
          pos += 1
        case "(" :: "define-fun" :: _ =>
          pos += 2
          val funName = parseName()
          val params = parseParamList()
          val bindings = params.map(p => {
            val fp = ctx.termgraph.getStmt(p).asInstanceOf[FunctionParameter]
            (fp.name, p)
          }).toMap
          local.push(bindings)
          val sortRef = parseSort()
          val body = parseTerm()
          val declRef = ctx.termgraph.memoAddInstruction(UserMacro(funName, sortRef, body, params))
          global.put(funName, declRef)
          pos += 1
        case "(" :: "declare-const" :: _ =>
          pos += 2
          val constName = parseName()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(constName, sortRef))
          global.put(constName, declRef)
          pos += 1
        case "(" :: "check-sat" :: ")" :: _ =>
          pos += 3
          ctx.checkSat()
        case "(" :: "declare-sort" :: _ =>
          pos += 2
          val name = parseName()
          val arity = Numeral(parseName().toInt)
          val us = ctx.termgraph.memoAddInstruction(UserSort(name, arity))
          global.put(name, us)
          pos += 1
        case "(" :: "declare-datatypes" :: _ =>
          pos += 2
          val names = parseDTNames()
          val dts = names.map(n => {
            ctx.termgraph.memoAddInstruction(DataType(n, List.empty)) // will fill in the empty list later
          })
          val constructors : List[List[Int]] = parseConstructorListList(dts)
          (0 to names.length - 1).foreach(idx => {
            global.put(names(idx), dts(idx)) // add the data type to the map
            val newDT = ctx.termgraph.memoAddInstruction(DataType(names(idx), constructors(idx))) // filling in from before
            ctx.termgraph.memoUpdateInstruction(dts(idx), Ref(newDT))
          })
          pos += 1 // for closing paren
        case "(" :: "define-sort" :: _ =>
          pos += 2
          val name = parseName()
          assert(tokens(pos) == "(", "Sort definition name must be followed by open paren!")
          pos += 1
          assert(tokens(pos) == ")", "Parametric sorts not supported!")
          pos += 1
          val sort = parseSort()
          global.put(name, sort)
          pos += 1
        case "(" :: "set-option" :: optionName :: _ =>
          pos += 5
          print(s"Ignoring (set-option $optionName ...) command in query ... ")
        case "(" :: "set-logic" :: logic :: _ =>
          pos += 4 
          print(s"Ignoring (set-logic $logic) command in query ... ")
        case "(" :: "set-info" :: _ =>
          pos += 2
          var parentheses = 1
          while (parentheses > 0) {
            if (tokens(pos) == "(") {parentheses += 1}
            else if (tokens(pos) == ")") {parentheses -= 1}
            pos += 1
          }
        case "(" :: "exit" :: ")" :: _ =>
          pos = tokens.length
        case c =>
          throw new SmtParserError("Unexpected character around: " + c)
      }
    }

    def parseName(): String = {
      pos += 1
      tokens(pos - 1)
    }

    def parseTerm(): Int = {
      val saveNest = nest

      val path = new Stack[Instruction]()
      val child = 0

      while {
        tokens(pos) match {
          case "(" if tokens(pos + 1) == "let" => {
            pos += 2
            val bindings = parseLetList().toMap
            local.push(bindings)
            val t = parseTerm()
            path.push(ctx.termgraph.getStmt(t))
            pos += 1 // for close paren
          }
          case "(" if tokens(pos + 1) != "_" => {
            pos += 1
            nest += 1
            val (op, bindings) = parseOperator()
            local.push(bindings)
            path.push(Application(op, List.empty))
          }
          case ")" => {
            pos += 1
            nest -= 1
            local.pop()
            if (path.size > 1) {
              val mostRecent = ctx.termgraph.memoAddInstruction(path.pop())
              val parent = path.pop().asInstanceOf[Application]
              path.push(Application(parent.caller, parent.args ++ List(mostRecent)))
            } else {
              // this is the top level, so just let it be
            }
          }
          case atom => if (path.size > 0) {
              // add to parent
              val parent = path.pop().asInstanceOf[Application]
              var child = parseSymbol()
              if (ctx.termgraph.completeButUnapplied(child)) {
                child = ctx.termgraph.memoAddInstruction(Application(child, List.empty))
              }
              path.push(Application(parent.caller, parent.args ++ List(child)))
            } else {
              // there was no parent so just return 
              var curr = parseSymbol()
              if (ctx.termgraph.completeButUnapplied(curr)) {
                curr = ctx.termgraph.memoAddInstruction(Application(curr, List.empty))
              }
              return curr
            }
        }

        (nest > saveNest)
      } 
      do () 

      assert(path.size == 1)
      ctx.termgraph.memoAddInstruction(path.head)
    }

    def parseConstructorListList(sorts: List[Int]): List[List[Int]] = {
      val params = new ListBuffer[List[Int]]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      var count = 0
      while (tokens(pos) != ")") {
        params.addOne(parseConstructorList(sorts(count)))
        count += 1
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }


    def parseConstructorList(sort: Int): List[Int] = {
      val params = new ListBuffer[Int]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      while (tokens(pos) != ")") {
        params.addOne(parseConstructor(sort))
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }

    def parseConstructor(sort: Int): Int = {
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      val name = parseName()
      val selectors = if (tokens(pos) == "(") {
        parseSelectorsList()
      } else {
        List.empty
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      val ctr = ctx.termgraph.memoAddInstruction(Constructor(name, sort, selectors))
      global.put(name, ctr)
      ctr
    }

    def parseSortList(): List[Int] = {
      val params = new ListBuffer[Int]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      while (tokens(pos) != ")") {
        params.addOne(parseSort())
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }

    def parseDTNames(): List[String] = {
      val params = new ListBuffer[String]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      while (tokens(pos) != ")") {
        assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
        pos += 1
        params.addOne(parseName())
        assert(tokens(pos) == "0", s"Parametric datatypes are not supported! ${tokens(pos)}")
        pos += 1
        assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
        pos += 1
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }

    def parseParamList(): List[Int] = {
      val params = new ListBuffer[Int]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      while (tokens(pos) != ")") {
        params.addOne(parseParam())
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }

    def parseParam() : Int = {
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      val name = parseName()
      val sort = parseSort()
      val fp = ctx.termgraph.memoAddInstruction(FunctionParameter(name, sort))
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      fp
    }

    def parseSelectorsList(): List[Int] = {
      val selectors = new ListBuffer[Int]()
      while (tokens(pos) != ")") {
        selectors.addOne(parseSelector())
      }
      selectors.toList
    }

    def parseSelector() : Int = {
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      val name = parseName()
      val sort = parseSort()
      val fp = ctx.termgraph.memoAddInstruction(Selector(name, sort))
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      global.put(name, fp)
      fp
    }

    def parseLetList(): List[(String, Int)] = {
      val params = new ListBuffer[(String, Int)]()
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      while (tokens(pos) != ")") {
        params.addOne(parseLetPair())
      }
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      params.toList
    }

    def parseLetPair() : (String, Int) = {
      assert(tokens(pos) == "(", s"Expected ( but got ${tokens(pos)}!")
      pos += 1
      val name = parseName()
      val term = parseTerm()
      assert(tokens(pos) == ")", s"Expected ) but got ${tokens(pos)}!")
      pos += 1
      (name, term)
    }

    def parseOperator(): (Int, Map[String, Int]) = {
      tokens(pos) match {
        case "exists" | "forall" => {
          val name = tokens(pos)
          pos += 1
          val params = parseParamList()
          val bindings = params.map(p => {
            val fp = ctx.termgraph.getStmt(p).asInstanceOf[FunctionParameter]
            (fp.name, p)
          }).toMap
          (ctx.termgraph.memoAddInstruction(TheoryMacro(name, params)), bindings)
        }
        case other => (parseSymbol(), Map.empty)
      }
    }

    def parseSort(): Int = {
      tokens(pos) match {
        // more complicated sorts
        case "(" => tokens(pos + 1) :: tokens(pos + 2) :: tokens(pos + 3) :: tokens(pos + 4) :: Nil match {
          case "_" :: "BitVec" :: width :: ")" :: Nil => {
            pos += 5
            val w = ctx.termgraph.memoAddInstruction(Numeral(width.toInt))
            ctx.termgraph.memoAddInstruction(TheorySort("BitVec", List(w)))
          }
          case "Array" :: _ => {
            pos += 2
            val indexSort = parseSort()
            val outputSort = parseSort()
            // don't forget about the closing parenthsis
            pos += 1
            ctx.termgraph.memoAddInstruction(TheorySort("Array", List(indexSort, outputSort)))
          }
          case c => throw new SmtParserError(s"Expected bit-vector sort, got $c!")
        }
        // sorts that are just names
        case name => {
          pos += 1
          if (global.containsKey(name)) {
            global.get(name)
          } else {
            ctx.termgraph.memoAddInstruction(TheorySort(name))
          }
        }
      }
    }

    /** Interpreted symbols, globally declared functions, bound variables, ...
      * @return the termgraph reference to the parsed symbol
      */ 
    def parseSymbol(): Int = {
      tokens(pos) match {
        // more complicated symbols like "(_ bv10 32)"
        case "(" => tokens(pos + 1) :: tokens(pos + 2) :: tokens(pos + 3) :: tokens(pos + 4) :: Nil match {
          case "_" :: bvexpr :: width :: ")" :: Nil if (bvexpr.startsWith("bv") || bvexpr.endsWith("extend")) => {
            pos += 5
            val w = ctx.termgraph.memoAddInstruction(Numeral(width.toInt))
            ctx.termgraph.memoAddInstruction(TheoryMacro(bvexpr, List(w)))
          }
          case c => throw new SmtParserError(s"Expected bit-vector literal, got $c!")
        }
        case other => {
          pos += 1
          if (inLocal(tokens(pos - 1))) {
            getLocal(tokens(pos - 1))
          } else if (global.containsKey(tokens(pos - 1))) {
            global.get(tokens(pos - 1))
          } else {
            tokens(pos - 1) match {
              // case other if other.startsWith("\"") && other.endsWith("\"") =>
              //   ctx.termgraph.memoAddInstruction(TheoryMacro(other))
              // case other if other.startsWith("#") =>
              //   ctx.termgraph.memoAddInstruction(TheoryMacro(other))
              // case other if other.toIntOption.isDefined =>
              //   ctx.termgraph.memoAddInstruction(TheoryMacro(other))
              // case other if OPERATORS.contains(other) =>
              //   ctx.termgraph.memoAddInstruction(TheoryMacro(other))
              // case other =>
              //   throw new SmtParserError(s"Unknown symbol: $other")
              // TODO check that we know the symbol
              case other =>
                ctx.termgraph.memoAddInstruction(TheoryMacro(other))
            }
          }
        }
      }
    }

    ctx
  }
}
