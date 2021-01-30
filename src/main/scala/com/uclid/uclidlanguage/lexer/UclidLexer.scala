package com.uclid.uclidlanguage.lexer

import com.uclid.uclidlanguage.compiler.{Location, UclidLexerError}

import scala.util.parsing.combinator.RegexParsers

object UclidLexer extends RegexParsers {
  override protected val whiteSpace = """\s+""".r

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

        case '/' => {
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
        }

        case '*' => {
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
        }

        // if we have a newline, then consume the character and exit line comment mode
        case '\n' => inLineComment = false; count += 1
        // if we have a whitespace, then consume the character no matter what
        case ' ' | '\t' => count += 1
        case _          =>
          // if we're in a comment, then consume the character
          if (inLineComment || inLongComment) {
            count += 1
          }
      }
    }
    assert((!inLineComment && !inLongComment) || offset + count == source.length)
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

  def tokens: Parser[List[UclidToken]] =
    phrase(
      rep1(
          lparenthesis |
          rparenthesis |
          comma |
          lbracket |
          rbracket |
          lbrace |
          rbrace |
          semicolon |
          opBiimpl |
          opImpl |
          opEq |
          opAssign |
          coloncolon |
          colon |
          period |
          arrow |
          opAnd |
          opOr |
          opAdd |
          opSub |
          opMul |
          opLe |
          opGe |
          opLt |
          opGt |
          opNeq |
          opNot |
          opPrime |
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
          kwTrace |
          kwAxiom |
          kwLet |
          intliteral |
          strliteral |
          boolliteral |
          identifier 
      )
    )

  def identifier: Parser[IDENTIFIER] = positioned {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => IDENTIFIER(str) }
  }

  def strliteral: Parser[STRLITERAL] = positioned {
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      STRLITERAL(content)
    }
  }

  def intliteral: Parser[INTLITERAL] = positioned {
    "[0-9]+".r ^^ { str => INTLITERAL(str) }
  }

  def boolliteral: Parser[BOOLLITERAL] = positioned {
    "true".r ^^ { str => BOOLLITERAL(str) } |
    "false".r ^^ { str => BOOLLITERAL(str) }
  }

  def lparenthesis = positioned("(" ^^ (_ => LPARENTHESIS()))
  def rparenthesis = positioned(")" ^^ (_ => RPARENTHESIS()))
  def comma = positioned("," ^^ (_ => COMMA()))
  def lbracket = positioned("[" ^^ (_ => LBRACKET()))
  def rbracket = positioned("]" ^^ (_ => RBRACKET()))
  def lbrace = positioned("{" ^^ (_ => LBRACE()))
  def rbrace = positioned("}" ^^ (_ => RBRACE()))
  def semicolon = positioned(";" ^^ (_ => SEMICOLON()))
  def opBiimpl = positioned("<==>" ^^ (_ => OPBIIMPL()))
  def opImpl = positioned("==>" ^^ (_ => OPIMPL()))
  def opEq = positioned("==" ^^ (_ => OPEQ()))
  def opAssign = positioned("=" ^^ (_ => ASSIGN()))
  def coloncolon = positioned("::" ^^ (_ => COLONCOLON()))
  def colon = positioned(":" ^^ (_ => COLON()))
  def period = positioned("." ^^ (_ => PERIOD()))
  def arrow = positioned("->" ^^ (_ => ARROW()))
  def opAnd = positioned("&&" ^^ (_ => OPAND()))
  def opOr = positioned("||" ^^ (_ => OPOR()))
  def opAdd = positioned("+" ^^ (_ => OPADD()))
  def opSub = positioned("-" ^^ (_ => OPSUB()))
  def opMul = positioned("*" ^^ (_ => OPMUL()))
  def opLe = positioned("<=" ^^ (_ => OPLE()))
  def opGe = positioned(">=" ^^ (_ => OPGE()))
  def opLt = positioned("<" ^^ (_ => OPLT()))
  def opGt = positioned(">" ^^ (_ => OPGT()))
  def opNeq = positioned("!=" ^^ (_ => OPNE()))
  def opNot = positioned("!" ^^ (_ => OPNOT()))
  def opPrime = positioned("'" ^^ (_ => OPPRIME()))
  def kwBoolean = positioned("boolean" ^^ (_ => KWBOOLEAN()))
  def kwInteger = positioned("integer" ^^ (_ => KWINTEGER()))
  def kwVar = positioned("var" ^^ (_ => KWVAR()))
  def kwSharedvar = positioned("sharedvar" ^^ (_ => KWSHAREDVAR()))
  def kwConst = positioned("const" ^^ (_ => KWCONST()))
  def kwFunction = positioned("function" ^^ (_ => KWFUNC()))
  def kwDefine = positioned("define" ^^ (_ => KWDEF()))
  def kwIf = positioned("if" ^^ (_ => KWIF()))
  def kwThen = positioned("then" ^^ (_ => KWTHEN()))
  def kwElse = positioned("else" ^^ (_ => KWELSE()))
  def kwType = positioned("type" ^^ (_ => KWTYPE()))
  def kwInput = positioned("input" ^^ (_ => KWINPUT()))
  def kwOutput = positioned("output" ^^ (_ => KWOUTPUT()))
  def kwInit = positioned("init" ^^ (_ => KWINIT()))
  def kwNext = positioned("next" ^^ (_ => KWNEXT()))
  def kwModule = positioned("module" ^^ (_ => KWMODULE()))
  def kwControl = positioned("control" ^^ (_ => KWCONTROL()))
  def kwInvariant = positioned("invariant" ^^ (_ => KWINVARIANT()))
  def kwCase = positioned("case" ^^ (_ => KWCASE()))
  def kwEsac = positioned("esac" ^^ (_ => KWESAC()))
  def kwDefault = positioned("default" ^^ (_ => KWDEFAULT()))
  def kwEnum = positioned("enum" ^^ (_ => KWENUM()))
  def kwRecord = positioned("record" ^^ (_ => KWRECORD()))
  def kwForall = positioned("forall" ^^ (_ => KWFORALL()))
  def kwExists = positioned("exists" ^^ (_ => KWEXISTS()))
  def kwHavoc = positioned("havoc" ^^ (_ => KWHAVOC()))
  def kwAssume = positioned("assume" ^^ (_ => KWASSUME()))
  def kwAssert = positioned("assert" ^^ (_ => KWASSERT()))

  def kwSet_solver_option = positioned {
    "set_solver_option" ^^ (_ => KWOPTION())
  }
  def kwSynthesis = positioned("synthesis" ^^ (_ => KWSYNTHESIS()))
  def kwPrint_cex = positioned("print_cex" ^^ (_ => KWGETVALUE()))
  def kwCheck = positioned("check" ^^ (_ => KWCHECK()))
  def kwTrace = positioned("trace" ^^ (_ => KWTRACE()))
  def kwAxiom = positioned("axiom" ^^ (_ => KWAXIOM()))
  def kwLet = positioned("let" ^^ (_ => KWLET()))

}
