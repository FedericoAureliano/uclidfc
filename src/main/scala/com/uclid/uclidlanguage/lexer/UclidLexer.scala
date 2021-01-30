package com.uclid.uclidlanguage.lexer

import com.uclid.uclidlanguage.compiler.{Location, UclidLexerError}

import scala.util.parsing.combinator.RegexParsers

object UclidLexer extends RegexParsers {
  /** Method called to handle whitespace before parsers.
    *
    *  skips anything matching `whiteSpace` starting from the current offset.
    *
    *  @param source  The input being parsed.
    *  @param offset  The offset into `source` from which to match.
    *  @return        The offset to be used for the next parser.
    */
  override protected def handleWhiteSpace(
    source: java.lang.CharSequence,
    offset: Int
  ): Int = {
    var oldCount = -1
    var count = 0
    var inLineComment = false
    var inLongComment = false
    while (count > oldCount && offset + count < source.length) {
      assert(!inLineComment || !inLongComment)
      oldCount = count

      source.charAt(offset + count) match {

        case '/' =>
          if (inLineComment || inLongComment) {
            // if we're in a comment already then just skip
            count += 1
          } else {
            // if we're not in a comment, then should we start a comment?
            source.charAt(offset + count + 1) match {
              case '/' => inLineComment = true; count += 2
              case '*' => inLongComment = true; count += 2
              case _   => // no we should not
            }
          }

        case '*' =>
          if (inLongComment) {
            // if we're in a long comment already then we may want to exit it
            source.charAt(offset + count + 1) match {
              case '/' => inLongComment = false; count += 2
              case _   => count += 1
            }
          } else if (inLineComment) {
            count += 1
          } else {
            // we're not in either comment
          }

        // if we have a newline, then consume the character and exit line comment mode
        case '\n' => inLineComment = false; count += 1
        // if we have a whitespace, then consume the character no matter what
        case a if whiteSpace.matches(a.toString)  => count += 1
        case _          =>
          // if we're in a comment, then consume the character
          if (inLineComment || inLongComment) {
            count += 1
          }
      }
    }
    assert(
      (!inLineComment && !inLongComment) || offset + count == source.length
    )
    offset + count
  }

  def apply(code: String): Either[UclidLexerError, List[UclidToken]] =
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(UclidLexerError(Location(next.pos.line, next.pos.column), msg))
      case Error(msg, next) =>
        Left(UclidLexerError(Location(next.pos.line, next.pos.column), msg))
      case Failure(msg, next) =>
        Left(UclidLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }

  lazy val reserved: Parser[UclidToken] =
    lparenParser |
      rparenParser |
      commaParser |
      lbracketParser |
      rbracketParser |
      lbraceParser |
      rbraceParser |
      semicolonParser |
      coloncolonParser |
      colonParser |
      periodParser |
      arrowParser |
      biimplParser |
      implParser |
      eqParser |
      assignParser |
      andParser |
      orParser |
      addParser |
      subParser |
      mulParser |
      leParser |
      geParser |
      ltParser |
      gtParser |
      neqParser |
      notParser |
      primeParser |
      booleanKwParser |
      integerKwParser |
      varKwParser |
      sharedvarKwParser |
      constKwParser |
      functionKwParser |
      defineKwParser |
      ifKwParser |
      thenKwParser |
      elseKwParser |
      typeKwParser |
      inputKwParser |
      outputKwParser |
      initKwParser |
      nextKwParser |
      moduleKwParser |
      controlKwParser |
      invariantKwParser |
      caseKwParser |
      esacKwParser |
      defaultKwParser |
      enumKwParser |
      recordKwParser |
      forallKwParser |
      existsKwParser |
      havocKwParser |
      assumeKwParser |
      assertKwParser |
      set_solver_optionKwParser |
      synthesisKwParser |
      print_cexKwParser |
      checkKwParser |
      traceKwParser |
      axiomKwParser |
      letKwParser

  def identifierParser: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def strliteralParser: Parser[STRLITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      STRLITERAL(content)
    }
  }

  def intliteralParser: Parser[INTLITERAL] = positioned {
    "[0-9]+".r ^^ { str => INTLITERAL(str) }
  }

  def boolliteralParser: Parser[BOOLLITERAL] = positioned {
    "true".r ^^ { str => BOOLLITERAL(str) } |
      "false".r ^^ { str => BOOLLITERAL(str) }
  }

  def lparenParser = positioned("(" ^^ (_ => LPARENTHESIS()))
  def rparenParser = positioned(")" ^^ (_ => RPARENTHESIS()))
  def commaParser = positioned("," ^^ (_ => COMMA()))
  def lbracketParser = positioned("[" ^^ (_ => LBRACKET()))
  def rbracketParser = positioned("]" ^^ (_ => RBRACKET()))
  def lbraceParser = positioned("{" ^^ (_ => LBRACE()))
  def rbraceParser = positioned("}" ^^ (_ => RBRACE()))
  def semicolonParser = positioned(";" ^^ (_ => SEMICOLON()))
  def coloncolonParser = positioned("::" ^^ (_ => COLONCOLON()))
  def colonParser = positioned(":" ^^ (_ => COLON()))
  def periodParser = positioned("." ^^ (_ => PERIOD()))
  def arrowParser = positioned("->" ^^ (_ => ARROW()))
  def biimplParser = positioned("<==>" ^^ (_ => OPBIIMPL()))
  def implParser = positioned("==>" ^^ (_ => OPIMPL()))
  def eqParser = positioned("==" ^^ (_ => OPEQ()))
  def assignParser = positioned("=" ^^ (_ => ASSIGN()))
  def andParser = positioned("&&" ^^ (_ => OPAND()))
  def orParser = positioned("||" ^^ (_ => OPOR()))
  def addParser = positioned("+" ^^ (_ => OPADD()))
  def subParser = positioned("-" ^^ (_ => OPSUB()))
  def mulParser = positioned("*" ^^ (_ => OPMUL()))
  def leParser = positioned("<=" ^^ (_ => OPLE()))
  def geParser = positioned(">=" ^^ (_ => OPGE()))
  def ltParser = positioned("<" ^^ (_ => OPLT()))
  def gtParser = positioned(">" ^^ (_ => OPGT()))
  def neqParser = positioned("!=" ^^ (_ => OPNE()))
  def notParser = positioned("!" ^^ (_ => OPNOT()))
  def primeParser = positioned("'" ^^ (_ => OPPRIME()))
  def booleanKwParser = positioned("boolean" ^^ (_ => KWBOOLEAN()))
  def integerKwParser = positioned("integer" ^^ (_ => KWINTEGER()))
  def varKwParser = positioned("var" ^^ (_ => KWVAR()))
  def sharedvarKwParser = positioned("sharedvar" ^^ (_ => KWSHAREDVAR()))
  def constKwParser = positioned("const" ^^ (_ => KWCONST()))
  def functionKwParser = positioned("function" ^^ (_ => KWFUNC()))
  def defineKwParser = positioned("define" ^^ (_ => KWDEF()))
  def ifKwParser = positioned("if" ^^ (_ => KWIF()))
  def thenKwParser = positioned("then" ^^ (_ => KWTHEN()))
  def elseKwParser = positioned("else" ^^ (_ => KWELSE()))
  def typeKwParser = positioned("type" ^^ (_ => KWTYPE()))
  def inputKwParser = positioned("input" ^^ (_ => KWINPUT()))
  def outputKwParser = positioned("output" ^^ (_ => KWOUTPUT()))
  def initKwParser = positioned("init" ^^ (_ => KWINIT()))
  def nextKwParser = positioned("next" ^^ (_ => KWNEXT()))
  def moduleKwParser = positioned("module" ^^ (_ => KWMODULE()))
  def controlKwParser = positioned("control" ^^ (_ => KWCONTROL()))
  def invariantKwParser = positioned("invariant" ^^ (_ => KWINVARIANT()))
  def caseKwParser = positioned("case" ^^ (_ => KWCASE()))
  def esacKwParser = positioned("esac" ^^ (_ => KWESAC()))
  def defaultKwParser = positioned("default" ^^ (_ => KWDEFAULT()))
  def enumKwParser = positioned("enum" ^^ (_ => KWENUM()))
  def recordKwParser = positioned("record" ^^ (_ => KWRECORD()))
  def forallKwParser = positioned("forall" ^^ (_ => KWFORALL()))
  def existsKwParser = positioned("exists" ^^ (_ => KWEXISTS()))
  def havocKwParser = positioned("havoc" ^^ (_ => KWHAVOC()))
  def assumeKwParser = positioned("assume" ^^ (_ => KWASSUME()))
  def assertKwParser = positioned("assert" ^^ (_ => KWASSERT()))

  def set_solver_optionKwParser = positioned {
    "set_solver_option" ^^ (_ => KWOPTION())
  }
  def synthesisKwParser = positioned("synthesis" ^^ (_ => KWSYNTHESIS()))
  def print_cexKwParser = positioned("print_cex" ^^ (_ => KWGETVALUE()))
  def checkKwParser = positioned("check" ^^ (_ => KWCHECK()))
  def traceKwParser = positioned("trace" ^^ (_ => KWTRACE()))
  def axiomKwParser = positioned("axiom" ^^ (_ => KWAXIOM()))
  def letKwParser = positioned("let" ^^ (_ => KWLET()))

  def tokens: Parser[List[UclidToken]] =
    phrase(
      rep1(
        reserved |
        intliteralParser |
        strliteralParser |
        boolliteralParser |
        identifierParser
      )
    )
}
