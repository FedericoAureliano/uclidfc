package com.uclid.uclidlanguage.lexer

import com.uclid.uclidlanguage.compiler.{Location, UclidLexerError}

import scala.util.parsing.combinator.RegexParsers

object UclidLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def apply(code: String): Either[UclidLexerError, List[UclidToken]] =
    parse(tokens, code) match {
      case NoSuccess(msg, next) =>
        Left(UclidLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }

  def tokens: Parser[List[UclidToken]] =
    phrase(
      rep1(
        identifier |
          strliteral |
          intliteral |
          opAnd |
          opOr |
          opAdd |
          opSub |
          opMul |
          opBiimpl |
          opImpl |
          opLy |
          opGt |
          opLe |
          opGe |
          opEq |
          opNeq |
          opNot |
          opMinus |
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
          kwLet
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
    """"[^"]*"""".r ^^ { str =>
      val content = str.substring(1, str.length - 1)
      INTLITERAL(content)
    }
  }

  def opAnd = positioned("&&" ^^ (_ => OPAND()))
  def opOr = positioned("||" ^^ (_ => OPOR()))
  def opAdd = positioned("+" ^^ (_ => OPADD()))
  def opSub = positioned("-" ^^ (_ => OPSUB()))
  def opMul = positioned("*" ^^ (_ => OPMUL()))
  def opBiimpl = positioned("<==>" ^^ (_ => OPBIIMPL()))
  def opImpl = positioned("==>" ^^ (_ => OPIMPL()))
  def opLy = positioned("<" ^^ (_ => OPLT()))
  def opGt = positioned(">" ^^ (_ => OPGT()))
  def opLe = positioned("<=" ^^ (_ => OPLE()))
  def opGe = positioned(">=" ^^ (_ => OPGE()))
  def opEq = positioned("==" ^^ (_ => OPEQ()))
  def opNeq = positioned("!=" ^^ (_ => OPNE()))
  def opNot = positioned("!" ^^ (_ => OPNOT()))
  def opMinus = positioned("-" ^^ (_ => OPMINUS()))
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
