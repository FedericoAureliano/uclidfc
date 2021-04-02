package com.uclid.uclidcompiler.lexer

import com.uclid.uclidcompiler.{Location, UclidLexerError}

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
    while count > oldCount && offset + count < source.length do {
      assert(!inLineComment || !inLongComment)
      oldCount = count

      source.charAt(offset + count) match {

        case '/' =>
          if inLineComment || inLongComment then {
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
          if inLongComment then {
            // if we're in a long comment already then we may want to exit it
            source.charAt(offset + count + 1) match {
              case '/' => inLongComment = false; count += 2
              case _   => count += 1
            }
          } else if inLineComment then {
            count += 1
          } else {
            // we're not in either comment
          }

        // if we have a newline, then consume the character and exit line comment mode
        case '\n' => inLineComment = false; count += 1
        // if we have a whitespace, then consume the character no matter what
        case a if whiteSpace.matches(a.toString) => count += 1
        case _                                   =>
          // if we're in a comment, then consume the character
          if inLineComment || inLongComment then {
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
    kwLparen |
      kwRparen |
      kwComma |
      kwLbracket |
      kwRbracket |
      kwLbrace |
      kwRbrace |
      kwSemicolon |
      kwColoncolon |
      kwColon |
      kwPeriod |
      kwArrow |
      kwBiimpl |
      kwImpl |
      kwEq |
      kwAssign |
      kwAnd |
      kwOr |
      kwAdd |
      kwSub |
      kwMul |
      kwLe |
      kwGe |
      kwLt |
      kwGt |
      kwNeq |
      kwNot |
      kwPrime |
      kwBoolean |
      kwInteger |
      kwVar |
      kwSharedvar |
      kwConst |
      kwFunction |
      kwDefine |
      kwIf |
      kwThen |
      kwElse |
      kwType |
      kwInput |
      kwOutput |
      kwInit |
      kwNext |
      kwModule |
      kwControl |
      kwInvariant |
      kwCase |
      kwEsac |
      kwDefault |
      kwEnum |
      kwRecord |
      kwForall |
      kwExists |
      kwHavoc |
      kwAssume |
      kwAssert |
      kwSet_solver_option |
      kwSynthesis |
      kwPrint_cex |
      kwCheck |
      kwAxiom |
      kwLet |
      intliteralParser |
      strliteralParser |
      boolliteralParser

  lazy val identifierParser: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  lazy val strliteralParser: Parser[STRLITERAL] = positioned {
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

  def kwLparen = positioned("(" ^^ (_ => LPARENTHESIS()))
  def kwRparen = positioned(")" ^^ (_ => RPARENTHESIS()))
  def kwComma = positioned("," ^^ (_ => COMMA()))
  def kwLbracket = positioned("[" ^^ (_ => LBRACKET()))
  def kwRbracket = positioned("]" ^^ (_ => RBRACKET()))
  def kwLbrace = positioned("{" ^^ (_ => LBRACE()))
  def kwRbrace = positioned("}" ^^ (_ => RBRACE()))
  def kwSemicolon = positioned(";" ^^ (_ => SEMICOLON()))
  def kwColoncolon = positioned("::" ^^ (_ => COLONCOLON()))
  def kwColon = positioned(":" ^^ (_ => COLON()))
  def kwPeriod = positioned("." ^^ (_ => PERIOD()))
  def kwArrow = positioned("->" ^^ (_ => ARROW()))
  def kwBiimpl = positioned("<==>" ^^ (_ => OPBIIMPL()))
  def kwImpl = positioned("==>" ^^ (_ => OPIMPL()))
  def kwEq = positioned("==" ^^ (_ => OPEQ()))
  def kwAssign = positioned("=" ^^ (_ => ASSIGN()))
  def kwAnd = positioned("&&" ^^ (_ => OPAND()))
  def kwOr = positioned("||" ^^ (_ => OPOR()))
  def kwAdd = positioned("+" ^^ (_ => OPADD()))
  def kwSub = positioned("-" ^^ (_ => OPSUB()))
  def kwMul = positioned("*" ^^ (_ => OPMUL()))
  def kwLe = positioned("<=" ^^ (_ => OPLE()))
  def kwGe = positioned(">=" ^^ (_ => OPGE()))
  def kwLt = positioned("<" ^^ (_ => OPLT()))
  def kwGt = positioned(">" ^^ (_ => OPGT()))
  def kwNeq = positioned("!=" ^^ (_ => OPNE()))
  def kwNot = positioned("!" ^^ (_ => OPNOT()))
  def kwPrime = positioned("'" ^^ (_ => OPPRIME()))
  def kwBoolean = positioned("boolean\\b".r ^^ (_ => KWBOOLEAN()))
  def kwInteger = positioned("integer\\b".r ^^ (_ => KWINTEGER()))
  def kwVar = positioned("var\\b".r ^^ (_ => KWVAR()))
  def kwSharedvar = positioned("sharedvar\\b".r ^^ (_ => KWSHAREDVAR()))
  def kwConst = positioned("const\\b".r ^^ (_ => KWCONST()))
  def kwFunction = positioned("function\\b".r ^^ (_ => KWFUNC()))
  def kwDefine = positioned("define\\b".r ^^ (_ => KWDEF()))
  def kwIf = positioned("if\\b".r ^^ (_ => KWIF()))
  def kwThen = positioned("then\\b".r ^^ (_ => KWTHEN()))
  def kwElse = positioned("else\\b".r ^^ (_ => KWELSE()))
  def kwType = positioned("type\\b".r ^^ (_ => KWTYPE()))
  def kwInput = positioned("input\\b".r ^^ (_ => KWINPUT()))
  def kwOutput = positioned("output\\b".r ^^ (_ => KWOUTPUT()))
  def kwInit = positioned("init\\b".r ^^ (_ => KWINIT()))
  def kwNext = positioned("next\\b".r ^^ (_ => KWNEXT()))
  def kwModule = positioned("module\\b".r ^^ (_ => KWMODULE()))
  def kwControl = positioned("control\\b".r ^^ (_ => KWCONTROL()))
  def kwInvariant = positioned("invariant\\b".r ^^ (_ => KWINVARIANT()))
  def kwCase = positioned("case\\b".r ^^ (_ => KWCASE()))
  def kwEsac = positioned("esac\\b".r ^^ (_ => KWESAC()))
  def kwDefault = positioned("default\\b".r ^^ (_ => KWDEFAULT()))
  def kwEnum = positioned("enum\\b".r ^^ (_ => KWENUM()))
  def kwRecord = positioned("record\\b".r ^^ (_ => KWRECORD()))
  def kwForall = positioned("forall\\b".r ^^ (_ => KWFORALL()))
  def kwExists = positioned("exists\\b".r ^^ (_ => KWEXISTS()))
  def kwHavoc = positioned("havoc\\b".r ^^ (_ => KWHAVOC()))
  def kwAssume = positioned("assume\\b".r ^^ (_ => KWASSUME()))
  def kwAssert = positioned("assert\\b".r ^^ (_ => KWASSERT()))

  def kwSet_solver_option = positioned {
    "set_solver_option\\b".r ^^ (_ => KWOPTION())
  }
  def kwSynthesis = positioned("synthesis\\b".r ^^ (_ => KWSYNTHESIS()))
  def kwPrint_cex = positioned("print_cex\\b".r ^^ (_ => KWGETVALUE()))
  def kwCheck = positioned("check\\b".r ^^ (_ => KWCHECK()))
  def kwAxiom = positioned("axiom\\b".r ^^ (_ => KWAXIOM()))
  def kwLet = positioned("let\\b".r ^^ (_ => KWLET()))

  lazy val tokens: Parser[List[UclidToken]] =
    phrase(
      rep1(
        reserved |
          identifierParser
      )
    )
}
