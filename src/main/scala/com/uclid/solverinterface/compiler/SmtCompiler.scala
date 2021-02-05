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
    val local: HashMap[String, Int] = new HashMap()
    var termgraph = new TermGraph()
    var ctx = new SyMTContext(termgraph)
    var pos = 0
    var nest = 0

    while (pos < tokens.length) {
      parseCommand()
    }

    def parseCommand(): Unit = {
      require(tokens(pos) == "(")

      tokens.drop(pos) match {
        case "(" :: "assert" :: _ =>
          pos += 2
          val term = parseTerm()
          ctx.addAssertion(term)
          pos += 1
        case "(" :: "check-sat" :: ")" :: _ =>
          pos += 3
          ctx.checkSat()
        case "(" :: "set-logic" :: logic :: ")" :: _ =>
          pos += 4 
          print(s"Ignoring (set-logic $logic) command in query ... ")
        case "(" :: "declare-const" :: _ =>
          pos += 2
          val constName = parseName()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(constName, sortRef))
          global(List(constName)) = declRef
          pos += 1
        case "(" :: "declare-fun" :: _ =>
          pos += 2
          val funName = parseName()
          val params = parseSortList()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(funName, sortRef, params))
          global(List(funName)) = declRef
          pos += 1
        case c =>
          throw new SmtParserError("Unexpected character around: " + c.take(if (c.length < 5) then c.length else 5))
      }
    }

    def parseName(): String = {
      pos += 1
      tokens(pos - 1)
    }

    def parseTerm(): Int = {
      val saveNest = nest
      tokens(pos) match {
        case "(" => {
          pos += 1
          nest += 1
          val op = parseOperator()
          val operands = new ListBuffer[Int]()
          while (nest > saveNest) {
            tokens(pos) match {
              case ")" => {
                pos += 1
                nest -= 1
              }
              case _ => operands.addOne(parseTerm())
            }
          }
          val ret = ctx.termgraph.memoAddInstruction(Application(op, operands.toList))
          ret
        }
        case atom => parseSymbol()
      }
    }
  
    def parseSortList(): List[Int] = {
      val saveNest = nest
      val params = new ListBuffer[Int]()
      while (nest > saveNest || tokens(pos) == "(") {
        tokens(pos) match {
          case "(" => {
            nest += 1
            pos += 1
          }
          case ")" => {
            pos += 1
            nest -= 1
          }
          case _ => {
            params.addOne(parseSort())
          }
        }
      }
      params.toList
    }

    def parseOperator(): Int = {
      // TODO: deal with more complicated cases
      parseSymbol()
    }

    def parseSort(): Int =
      global.getOrElse(
        tokens.drop(pos),
        tokens.drop(pos) match {
          case "Int" :: _ =>
            pos += 1
            ctx.termgraph.memoAddInstruction(TheorySort("Int"))
          case "Bool" :: _ =>
            pos += 1
            ctx.termgraph.memoAddInstruction(TheorySort("Bool"))
          case "String" :: _ =>
            pos += 1
            ctx.termgraph.memoAddInstruction(TheorySort("String"))
          case _ => throw new SmtParserError("Expected a sort but got " + tokens(pos))
        }
      )

    /** Interpreted symbols, globally declared functions, bound variables, ...
      * @return the termgraph reference to the parsed symbol
      */ 
    def parseSymbol(): Int =
      pos += 1
      local.getOrElse(
        tokens(pos - 1),
        global.getOrElse(
          List(tokens(pos - 1)),
          tokens(pos - 1) match {
            case "ite" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("ite"))

            case "+" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("+"))
            case "-" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("-"))
            case "*" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("*"))

            case "str.++" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.++"))
            case "str.indexof" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.indexof"))
            case "str.substr" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.substr"))
            case "str.len" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.len"))
            case "str.contains" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.contains"))
            case "str.prefixof" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.prefixof"))
            case "str.suffixof" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.suffixof"))
            case "str.replace" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.replace"))
            case "str.at" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("str.at"))

            case ">" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro(">"))
            case "<" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("<"))
            case ">=" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro(">="))
            case "<=" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("<="))
            case "=" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("="))

            case "and" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("and"))
            case "or" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("or"))
            case "not" =>
              ctx.termgraph.memoAddInstruction(TheoryMacro("not"))

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

    ctx
  }
}
