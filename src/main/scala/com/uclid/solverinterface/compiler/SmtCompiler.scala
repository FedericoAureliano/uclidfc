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

  def compile(script: String): SyMTContext = {
    val termgraph = new TermGraph()
    val ctx = new SyMTContext(termgraph)

    val tokens = tokenize(removeComments(script))

    val global: HashMap[String, Int] = new HashMap()

    var pos = 0
    while (pos < tokens.length)
      pos = parseCommand(pos)

    /** Parse a top-level smt-lib command
      * @param start the position of the opening parenthesis "(" of the command
      * @return the position of the closing parenthesis ")" of the command
      */ 
    def parseCommand(start: Int): Int = {
      tokens.drop(pos) match {
        case "(" :: "assert" :: _ =>
          val newEndPos = sexprUntilMatching(pos)
          assert(newEndPos > pos)
          parseAssertion(pos, newEndPos)
          pos = newEndPos + 1
          assert(tokens(pos - 1) == ")")
          pos
        case "(" :: "check-sat" :: ")" :: _ =>
          ctx.checkSat()
          pos += 3
          assert(tokens(pos - 1) == ")")
          pos
        case "(" :: "set-logic" :: logic :: ")" :: _ =>
          print(s"Ignoring (set-logic $logic) command in query ... ")
          pos += 4
          assert(tokens(pos - 1) == ")")
          pos
        case "(" :: "declare-const" :: constName :: sortName :: ")" :: _ =>
          val sortRef = parseAtom(sortName, HashMap.empty)
          val declRef =
            termgraph.memoAddInstruction(UserFunction(constName, sortRef))
          global(constName) = declRef
          pos += 5
          assert(tokens(pos - 1) == ")")
          pos
        case "(" :: "declare-fun" :: funName :: "(" :: _ =>
          val paramsStart = pos + 3
          val paramsEnd = sexprUntilMatching(paramsStart) + 1
          val params = splitIntoTerms(paramsStart, paramsEnd).map(p =>
            parseTerm(p._1, p._2, HashMap.empty)
          )
          val sortRef = parseAtom(tokens(paramsEnd), HashMap.empty)
          val declRef =
            termgraph.memoAddInstruction(UserFunction(funName, sortRef, params))
          global(funName) = declRef
          pos = paramsEnd + 2 //the sort plus the ")"
          assert(tokens(pos - 1) == ")")
          pos
        case "(" :: "declare-datatypes" :: _ =>
            val namesStart = pos + 2
            val namesEnd = sexprUntilMatching(namesStart) + 1
            val names = splitIntoTerms(namesStart, namesEnd).map(p => {
                // the arity must always be zero
                assert(tokens(p._2-2) == "0", "Parametric datatypes not supported!")
                val name = tokens(p._1+1)
                val sortRef = termgraph.addInstruction(DataType(name, List.empty)) // placeholder
                global(name) = sortRef
                (name, sortRef)
            })

            val bodiesStart = namesEnd
            assert(tokens(bodiesStart) == "(")
            val bodiesEnd = sexprUntilMatching(bodiesStart) + 1
            val bodies : List[List[(String, List[(String, String)])]] = splitIntoTerms(bodiesStart, bodiesEnd).map((bodyStart, bodyEnd) => {
                splitIntoTerms(bodyStart, bodyEnd).map((ctrXSelsStart, ctrXSelsEnd) => {
                    val ctrXSels = splitIntoTerms(ctrXSelsStart, ctrXSelsEnd)
                    val sels = if (ctrXSels.length > 1) {
                        ctrXSels.tail.map((selStart, selEnd) => {
                            val pair = splitIntoTerms(selStart, selEnd).map { x =>
                            assert(x._1 + 1 == x._2)
                            tokens(x._1)
                            }
                            assert(pair.length == 2)
                            (pair.head, pair.last)
                        })
                    } else {
                        List.empty
                    }
                    (tokens(ctrXSels.head._1), sels)
                })
            })

            names.zip(bodies).foreach(dt => {
                val ctrs = dt._2.map(ctrXsels => {
                    val sels = ctrXsels._2.map(pair => {
                        val selRef = termgraph.memoAddInstruction(
                            Selector(pair._1, parseAtom(pair._2, HashMap.empty))
                        )
                        global(pair._1) = selRef
                        selRef
                    })
                    val ctrRef = termgraph.memoAddInstruction(Constructor(ctrXsels._1, dt._1._2, sels))
                    global(ctrXsels._1) = ctrRef
                    ctrRef
                })
                termgraph.memoUpdateInstruction(dt._1._2, DataType(dt._1._1, ctrs))
            })

            pos = bodiesEnd + 1
            assert(tokens(pos - 1) == ")")
            pos
        case c =>
          throw new SmtParserError("Unexpected character around: " + c.take(if (c.length < 5) then c.length else 5))
      }
    }

    def sexprUntilMatching(start: Int): Int = {
      require(tokens(start) == "(")
      var count = 1
      var endpos = start + 1
      while (count != 0) {
        tokens(endpos) match {
          case "(" => count += 1
          case ")" => count -= 1
          case _   =>
        }
        endpos += 1
      }
      endpos - 1
    }.ensuring(endPos => tokens(endPos) == ")" && endPos > start)

    def splitIntoTerms(startPos: Int, endPos: Int): List[(Int, Int)] = {
      require(startPos < endPos)
      val subterms = new ListBuffer[(Int, Int)]()

      var pos = startPos + 1
      while (pos < endPos - 1)
        if (tokens(pos) == "(") {
          val endpos = sexprUntilMatching(pos)
          subterms.addOne((pos, endpos + 1))
          pos = endpos + 1
        } else {
          subterms.addOne((pos, pos + 1))
          pos += 1
        }
      subterms.toList

    }.ensuring(_.forall(p => p._1 < p._2))

    def parseAssertion(startPos: Int, endPos: Int): Unit = {
      require(tokens(startPos) == "(")
      require(tokens(startPos + 1) == "assert")
      require(tokens(endPos) == ")")
      // get rid of "(" and "assert" and ")"
      val term = parseTerm(startPos + 2, endPos, new HashMap[String, Int]())
      ctx.addAssertion(term)
    }

    def parseTerm(
      startPos: Int,
      endPos: Int,
      local: HashMap[String, Int]
    ): Int = {
      assert(startPos < endPos)
      assert(startPos + 1 == endPos || tokens(startPos) == "(")

      if (startPos + 1 == endPos) {
        parseAtom(tokens(startPos), local)
      } else {
        val horizontalPairs = splitIntoTerms(startPos, endPos)
        val termRefs = new ListBuffer[Int]()
        var i = 0
        while (i < horizontalPairs.length) {
          tokens(horizontalPairs(i)._1) match {
            case "exists" =>
              val paramRefs = parseNamedParams(
                horizontalPairs(i + 1)._1,
                horizontalPairs(i + 1)._2 - 1
              )
              val existsRef = termgraph.memoAddInstruction(
                TheoryMacro("exists", paramRefs.map(p => p._2))
              )
              termRefs.append(existsRef)
              local ++= paramRefs.toMap
              i += 1
            case "forall" =>
              val paramRefs = parseNamedParams(
                horizontalPairs(i + 1)._1,
                horizontalPairs(i + 1)._2 - 1
              )
              val forallRef = termgraph.memoAddInstruction(
                TheoryMacro("forall", paramRefs.map(p => p._2))
              )
              termRefs.append(forallRef)
              local ++= paramRefs.toMap
              i += 1
            case _ =>
              termRefs.append(parseTerm(horizontalPairs(i)._1, horizontalPairs(i)._2, local))
          }
          i += 1
        }

        termgraph.memoAddInstruction(
          Application(termRefs.head, termRefs.tail.toList)
        )
      }
    }

    def parseNamedParams(startPos: Int, endPos: Int): List[(String, Int)] = {
      require(tokens(startPos) == "(")
      require(tokens(endPos) == ")")
      splitIntoTerms(startPos, endPos).map { p =>
        val pair = splitIntoTerms(p._1, p._2).map { x =>
          assert(x._1 + 1 == x._2)
          tokens(x._1)
        }
        assert(pair.length == 2)
        (
          pair.head,
          termgraph.memoAddInstruction(
            FunctionParameter(pair.head, parseAtom(pair.last, HashMap.empty))
          )
        )
      }
    }

    def parseAtom(atom: String, local: HashMap[String, Int]): Int =
      local.getOrElse(
        atom,
        global.getOrElse(
          atom,
          atom match {
            case "+" =>
              termgraph.memoAddInstruction(TheoryMacro("+"))
            case "-" =>
              termgraph.memoAddInstruction(TheoryMacro("-"))
            case "*" =>
              termgraph.memoAddInstruction(TheoryMacro("*"))

            case ">" =>
              termgraph.memoAddInstruction(TheoryMacro(">"))
            case "<" =>
              termgraph.memoAddInstruction(TheoryMacro(">"))
            case ">=" =>
              termgraph.memoAddInstruction(TheoryMacro(">"))
            case "<=" =>
              termgraph.memoAddInstruction(TheoryMacro(">"))
            case "=" =>
              termgraph.memoAddInstruction(TheoryMacro("="))
            case "and" =>
              termgraph.memoAddInstruction(TheoryMacro("and"))
            case "or" =>
              termgraph.memoAddInstruction(TheoryMacro("or"))

            case "Int" =>
              termgraph.memoAddInstruction(TheorySort("Int"))
            case "Bool" =>
              termgraph.memoAddInstruction(TheorySort("Bool"))
            case "String" =>
              termgraph.memoAddInstruction(TheorySort("String"))

            case "false" =>
              termgraph.memoAddInstruction(TheoryMacro("false"))
            case "true" =>
              termgraph.memoAddInstruction(TheoryMacro("true"))
            case other if other.startsWith("\"") && other.endsWith("\"") =>
              termgraph.memoAddInstruction(TheoryMacro(other))
            case other =>
              // must be an integer (add support for other stuff later)
              termgraph.memoAddInstruction(TheoryMacro(other.toInt.toString))
          }
        )
      )

    ctx
  }
}
