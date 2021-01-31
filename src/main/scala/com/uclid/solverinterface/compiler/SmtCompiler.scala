package com.uclid.solverinterface.compiler

import com.uclid.solverinterface.SyMTContext
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
      tokens.drop(pos) match {
        case "(" :: "assert" :: _ =>
          val newEndPos = sexprUntilMatching(pos)
          assert(newEndPos > pos)
          parseAssertion(pos, newEndPos)
          pos = newEndPos
        case "(" :: "check-sat" :: ")" :: _ =>
          ctx.checkSat()
          pos += 3
        case "(" :: "set-logic" :: logic :: ")" :: _ =>
          print(s"Ignoring (set-logic $logic) command in query ... ")
          pos += 4
        case "(" :: "declare-const" :: constName :: sortName :: ")" :: _ =>
          val sortRef = parseAtom(sortName, HashMap.empty)
          val declRef =
            termgraph.memoAddInstruction(UserFunction(constName, sortRef))
          global(constName) = declRef
          pos += 5
        case _ => pos += 1
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
      //   println(
      //     "SplitIntoTerms: " + tokens.slice(startPos, endPos).mkString(" ")
      //   )
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

      //   println("SplitIntoTerms Result:\n" + subterms.map(p => tokens.slice(p._1, p._2).mkString(" ")).mkString("\n"))

      subterms.toList

    }.ensuring(_.forall(p => p._1 < p._2))

    def parseAssertion(startPos: Int, endPos: Int): Unit = {
      //   println(
      //     "parseAssertion: " + tokens.slice(startPos, endPos).mkString(" ")
      //   )
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
      //   println(
      //     "parseTerm: " + tokens.slice(startPos, endPos).mkString(" ")
      //   )
      assert(startPos < endPos)
      assert(startPos + 1 == endPos || tokens(startPos) == "(")

      if (startPos + 1 == endPos) {
        parseAtom(tokens(startPos), local)
      } else {
        val horizontalPairs = splitIntoTerms(startPos, endPos)
        termgraph.memoAddInstruction(
          Application(
            parseTerm(horizontalPairs.head._1, horizontalPairs.head._2, local),
            horizontalPairs.tail.map(t => parseTerm(t._1, t._2, local))
          )
        )
      }
    }

    def parseAtom(atom: String, local: HashMap[String, Int]): Int =
      // println(
      //   "parseAtom: " + tokens(pos)
      // )
      local.getOrElse(
        atom,
        global.getOrElse(
          atom,
          atom match {
            case "false" =>
              termgraph.memoAddInstruction(TheoryMacro("false"))
            case "true" =>
              termgraph.memoAddInstruction(TheoryMacro("true"))
            case "=" =>
              termgraph.memoAddInstruction(TheoryMacro("="))
            case "+" =>
              termgraph.memoAddInstruction(TheoryMacro("+"))
            case "-" =>
              termgraph.memoAddInstruction(TheoryMacro("-"))
            case "Int" =>
              termgraph.memoAddInstruction(TheorySort("Int"))
            case other =>
              // must be some kind of literal
              // TODO: fail if not
              termgraph.memoAddInstruction(TheoryMacro(other))
          }
        )
      )

    ctx
  }
}
