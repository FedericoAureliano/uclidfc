package com.uclid.solverinterface.compiler

import com.uclid.context.SyMTContext
import com.uclid.termgraph._

import java.util.HashMap
import scala.collection.mutable.{ListBuffer, Stack}

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
      
      tokens(pos) :: tokens(pos + 1) :: tokens(pos + 2) :: Nil match {
        case "(" :: "assert" :: _ =>
          pos += 2
          val term = parseTerm()
          ctx.addAssertion(term)
          pos += 1
        case "(" :: "check-sat" :: ")" :: _ =>
          pos += 3
          ctx.checkSat()
        case "(" :: "set-logic" :: logic :: _ =>
          pos += 4 
          print(s"Ignoring (set-logic $logic) command in query ... ")
        case "(" :: "declare-const" :: _ =>
          pos += 2
          val constName = parseName()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(constName, sortRef))
          global.put(constName, declRef)
          pos += 1
        case "(" :: "declare-fun" :: _ =>
          pos += 2
          val funName = parseName()
          val params = parseSortList()
          val sortRef = parseSort()
          val declRef = ctx.termgraph.memoAddInstruction(UserFunction(funName, sortRef, params))
          global.put(funName, declRef)
          pos += 1
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
          case "(" => {
            pos += 1
            nest += 1
            val op = parseOperator()
            path.push(Application(op, List.empty))
          }
          case ")" => {
            pos += 1
            nest -= 1
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
              path.push(Application(parent.caller, parent.args ++ List(parseSymbol())))
            } else {
              // there was no parent so just return 
              return parseSymbol()
            }
        }

        (nest > saveNest)
      } 
      do () 

      assert(path.size == 1)
      ctx.termgraph.memoAddInstruction(path.head)
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

    def parseSort(): Int = {
      pos += 1
      if (global.containsKey(tokens(pos - 1))) {
        global.get(tokens(pos - 1))
      } else {
        ctx.termgraph.memoAddInstruction(TheorySort(tokens(pos - 1)))
      }
    }

    /** Interpreted symbols, globally declared functions, bound variables, ...
      * @return the termgraph reference to the parsed symbol
      */ 
    def parseSymbol(): Int =
      pos += 1
      if (local.size() > 0 && local.containsKey(tokens(pos - 1))) {
        local.get(tokens(pos - 1))
      } else if (global.containsKey(tokens(pos - 1))) {
        global.get(tokens(pos - 1))
      } else {
        tokens(pos - 1) match {
          case other if other.startsWith("\"") && other.endsWith("\"") =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(other))
          case other =>
            ctx.termgraph.memoAddInstruction(TheoryMacro(other))
        }
      }
    ctx
  }
}
