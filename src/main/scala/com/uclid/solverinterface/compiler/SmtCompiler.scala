package com.uclid.solverinterface.compiler

import com.uclid.context.SyMTContext
import com.uclid.termgraph._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

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

  def tokenize(script: String): List[String] =
    script
      .replace("(", " ( ")
      .replace(")", " ) ")
      .trim()
      .split("\\s+")
      .toList

  /** Translate smtlib string to termgraph in SMT context
    * 
    */
  def compile(script: String): SyMTContext = {
    var tokens : List[String] = tokenize(removeComments(script))
    val global: HashMap[List[String], Int] = new HashMap()
    var termgraph = new TermGraph()
    var ctx = new SyMTContext(termgraph)

    var pos = 0
    while (pos < tokens.length) {
      val command = sexprUntilMatching(tokens.drop(pos))
      parseCommand(command, ctx, global)
      pos += command.length
    }
    ctx
  }

  /** Helper to get the position of the matching closing parenthesis
    * @param tokens list of tokens starting with "(" 
    * @return the sexpr that ends with the matching ")"
    */ 
  def sexprUntilMatching(tokens: List[String]): List[String] = {
    require(tokens.head == "(")
    var count = 1
    var endpos = 1
    while (count != 0) {
      tokens(endpos) match {
        case "(" => count += 1
        case ")" => count -= 1
        case _   =>
      }
      endpos += 1
    }
    tokens.slice(0, endpos)
  }.ensuring(sexpr => sexpr.last == ")" && sexpr.length > 0)

  /** Helper to split (A1 A2 ... An) into a list [A1, A2, ..., An], where Ai is a complete term (either an atom or a complete sexpr)
    * @param tokens list of tokens starting with "(" and ending in the matching closing parenthesis ")" 
    * @return A list of subterms [A1, A2, ..., An]
    */ 
  def splitIntoTerms(tokens: List[String]): List[List[String]] = {
    require(tokens.head == "(")
    require(tokens.last == ")")

    val subterms = new ListBuffer[List[String]]()

    var pos = 1
    while (pos < tokens.length)
      if (tokens(pos) == "(") {
        val subterm = sexprUntilMatching(tokens.drop(pos))
        subterms.addOne(subterm)
        pos += subterm.length
      } else if (tokens(pos) == ")") {
        pos += 1
      } else {
        subterms.addOne(List(tokens(pos)))
        pos += 1
      }

    subterms.toList

  }.ensuring(_.forall(p => p.length > 0))

  /** Helper to parse ((name1 sort1) (name2 sort2) .. (namen sortn))
    * @param tokens list of tokens starting with "(" and ending in the matching closing parenthesis ")" 
    * @return A list of name sort pairs, where sort has been internalized [(name1, IndexInTermGraph(sort1)), ..., (namen, IndexInTermGraph(sortn))]
    */ 
  def parseNamedParams(tokens : List[String]): List[(String, List[String])] = {
    require(tokens.head == "(")
    require(tokens.last == ")")

    splitIntoTerms(tokens).map { param =>
      val pair = splitIntoTerms(param)
      assert(pair.length == 2)
      (pair.head.head, pair.last)
    }
  }

  /** Parse a top-level smt-lib command
    * @param tokens list of tokens starting with "(" and ending in the matching closing parenthesis ")" 
    */ 
  def parseCommand(tokens : List[String], ctx: SyMTContext, global: HashMap[List[String], Int]): Unit = {
    require(tokens.head == "(")
    require(tokens.last == ")")

    tokens match {
      case "(" :: "assert" :: rest =>
        val term = parseTerm(rest.take(rest.length - 1), new HashMap[String, Int](), ctx, global)
        ctx.addAssertion(term)
      case "(" :: "check-sat" :: ")" :: Nil =>
        ctx.checkSat()
      case "(" :: "set-logic" :: logic :: ")" :: Nil =>
        print(s"Ignoring (set-logic $logic) command in query ... ")
      case "(" :: "declare-const" :: rest =>
        val constName = rest.head
        val sortList = if (rest.tail.length == 2) then List(rest.tail.head) else sexprUntilMatching(rest.tail)
        val sortRef = parseSort(sortList, ctx, global)
        val declRef = ctx.termgraph.memoAddInstruction(UserFunction(constName, sortRef))
        global(List(constName)) = declRef
      case "(" :: "declare-fun" :: funName :: rest =>
        val paramsTerm = sexprUntilMatching(rest)
        val params = splitIntoTerms(paramsTerm).map(p => {
          parseSort(p, ctx, global)
        })

        val sortList = rest.drop(paramsTerm.length)
        val sortTerm = if (sortList.head == "(") then sexprUntilMatching(sortList) else List(sortList.head)
        val sortRef = parseSort(sortTerm, ctx, global)

        val declRef = ctx.termgraph.memoAddInstruction(UserFunction(funName, sortRef, params))
        global(List(funName)) = declRef
      case "(" :: "declare-datatypes" :: rest =>
          val namesTerm = sexprUntilMatching(rest)
          val names = splitIntoTerms(namesTerm).map(p => {
              // the arity must always be zero
              assert(p(2) == "0", "Parametric datatypes not supported!")
              val name = p(1)
              val sortRef = ctx.termgraph.addInstruction(DataType(name, List.empty)) // placeholder
              global(List(name)) = sortRef
              (name, sortRef)
          })

          val bodiesTerm = sexprUntilMatching(rest.drop(namesTerm.length))
          val bodies : List[List[(String, List[(String, List[String])])]] = splitIntoTerms(bodiesTerm).map(body => {
              splitIntoTerms(body).map(ctrXsels => {
                val ctr :: sels = splitIntoTerms(ctrXsels)
                assert(ctr.length == 1)
                (ctr(0), sels.map(s => {
                  val name :: sort = splitIntoTerms(s)
                  assert(name.length == 1)
                  assert(sort.length == 1)
                  (name(0), sort(0))
                }))
              })
          })

          names.zip(bodies).foreach(dt => {
              val ctrs = dt._2.map(ctrXsels => {
                  val sels = ctrXsels._2.map(pair => {
                      val selRef = ctx.termgraph.memoAddInstruction(
                          Selector(pair._1, parseSort(pair._2, ctx, global))
                      )
                      global(List(pair._1)) = selRef
                      selRef
                  })
                  val ctrRef = ctx.termgraph.memoAddInstruction(Constructor(ctrXsels._1, dt._1._2, sels))
                  global(List(ctrXsels._1)) = ctrRef
                  ctrRef
              })
              ctx.termgraph.memoUpdateInstruction(dt._1._2, DataType(dt._1._1, ctrs))
          })
      case c =>
        throw new SmtParserError("Unexpected character around: " + c.take(if (c.length < 5) then c.length else 5))
    }
  }

  def parseTerm(
    tokens : List[String],
    local: HashMap[String, Int],
    ctx: SyMTContext, 
    global: HashMap[List[String], Int]
  ): Int = {
    require(tokens.length == 1 || tokens.head == "(")

    tokens match {
      case "(" :: "exists" :: rest =>
        val paramsTerm = sexprUntilMatching(rest)
        val paramRefs = parseNamedParams(paramsTerm).map(p => (p._1, ctx.termgraph.memoAddInstruction(FunctionParameter(p._1, parseSort(p._2, ctx, global)))))
        val existsRef = ctx.termgraph.memoAddInstruction(
          TheoryMacro("exists", paramRefs.map(p => p._2))
        )
        local ++= paramRefs.toMap
        val operandTokens = sexprUntilMatching(tokens.drop(paramsTerm.length + 2))
        val operandTerm = parseTerm(operandTokens, local, ctx, global)
        ctx.termgraph.memoAddInstruction(Application(existsRef, List(operandTerm)))
      case "(" :: "forall" :: rest =>
        val paramsTerm = sexprUntilMatching(rest)
        val paramRefs = parseNamedParams(paramsTerm).map(p => (p._1, ctx.termgraph.memoAddInstruction(FunctionParameter(p._1, parseSort(p._2, ctx, global)))))
        val forallRef = ctx.termgraph.memoAddInstruction(
          TheoryMacro("forall", paramRefs.map(p => p._2))
        )
        local ++= paramRefs.toMap
        val operandTokens = sexprUntilMatching(tokens.drop(paramsTerm.length + 2))
        val operandTerm = parseTerm(operandTokens, local, ctx, global)
        ctx.termgraph.memoAddInstruction(Application(forallRef, List(operandTerm)))
      case atom :: Nil =>
        parseSymbol(atom, local, ctx, global)
      case _ => {
        val operator :: operands = splitIntoTerms(tokens)
        ctx.termgraph.memoAddInstruction(
          Application(parseOperator(operator, local, ctx, global), operands.map(o => parseTerm(o, local, ctx, global)))
        )
      }
    }
  }

  def parseOperator(
    tokens : List[String],
    local: HashMap[String, Int],
    ctx: SyMTContext, 
    global: HashMap[List[String], Int]
  ): Int = {
    assert(tokens.length == 1, "TODO: handle more complicated ops (like as const)")
    parseSymbol(tokens.head, local, ctx, global)
  }

  /** Interpreted sorts, declared sorts, and datatypes
    * 
    * TODO: will need to generalize to take a list of strings for stuff like bitvectors 
    * 
    * @param sort the symbol to parse
    * @return the termgraph reference to the parsed sort
    */ 
  def parseSort(sort: List[String], ctx: SyMTContext, global: HashMap[List[String], Int]): Int =
    global.getOrElse(
      sort,
      sort match {
        case "Int" :: Nil =>
          ctx.termgraph.memoAddInstruction(TheorySort("Int"))
        case "Bool" :: Nil =>
          ctx.termgraph.memoAddInstruction(TheorySort("Bool"))
        case "String" :: Nil =>
          ctx.termgraph.memoAddInstruction(TheorySort("String"))
        case _ => throw new SmtParserError("Expected a sort but got " + sort)
      }
    )

  /** Interpreted symbols, globally declared functions, bound variables, ...
    * @param atom the symbol to parse
    * @param local the local bindings for bound variables
    * @return the termgraph reference to the parsed symbol
    */ 
  def parseSymbol(atom: String, local: HashMap[String, Int], ctx: SyMTContext, global: HashMap[List[String], Int]): Int =
    local.getOrElse(
      atom,
      global.getOrElse(
        List(atom),
        atom match {
          case "+" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("+"))
          case "-" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("-"))
          case "*" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("*"))

          case ">" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(">"))
          case "<" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(">"))
          case ">=" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(">"))
          case "<=" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(">"))
          case "=" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("="))
          case "and" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("and"))
          case "or" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("or"))

          case "false" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("false"))
          case "true" =>
            ctx.termgraph.memoAddInstruction(TheoryMacro("true"))
          case other if other.startsWith("\"") && other.endsWith("\"") =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(other))
          case other =>
            // must be an integer (add support for other stuff later)
            ctx.termgraph.memoAddInstruction(TheoryMacro(other.toInt.toString))
        }
      )
    )
}
