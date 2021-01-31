package com.uclid.uclidinterface.compiler.lexer


import scala.util.parsing.input.Positional

trait UclidToken extends Positional

case class IDENTIFIER(str: String) extends UclidToken
case class BOOLLITERAL(str: String) extends UclidToken
case class INTLITERAL(str: String) extends UclidToken
case class STRLITERAL(str: String) extends UclidToken

// "(",
case class LPARENTHESIS() extends UclidToken

// ")",
case class RPARENTHESIS() extends UclidToken

// ",",
case class COMMA() extends UclidToken

// "[",
case class LBRACKET() extends UclidToken

// "]",
case class RBRACKET() extends UclidToken

// "{",
case class LBRACE() extends UclidToken

// "}",
case class RBRACE() extends UclidToken

// ";",
case class SEMICOLON() extends UclidToken

// "==",
case class OPEQ() extends UclidToken

// "=",
case class ASSIGN() extends UclidToken

// ":",
case class COLON() extends UclidToken

// ".",
case class PERIOD() extends UclidToken

// "::",
case class COLONCOLON() extends UclidToken

// "->",
case class ARROW() extends UclidToken

case class OPAND() extends UclidToken
case class OPOR() extends UclidToken
case class OPADD() extends UclidToken
case class OPSUB() extends UclidToken
case class OPMUL() extends UclidToken
case class OPBIIMPL() extends UclidToken
case class OPIMPL() extends UclidToken
case class OPLT() extends UclidToken
case class OPGT() extends UclidToken
case class OPLE() extends UclidToken
case class OPGE() extends UclidToken
case class OPNE() extends UclidToken
case class OPNOT() extends UclidToken
case class OPMINUS() extends UclidToken
case class OPPRIME() extends UclidToken
case class FALSE() extends UclidToken
case class TRUE() extends UclidToken
case class KWBOOLEAN() extends UclidToken
case class KWINTEGER() extends UclidToken
case class KWSHAREDVAR() extends UclidToken
case class KWVAR() extends UclidToken
case class KWIF() extends UclidToken
case class KWTHEN() extends UclidToken
case class KWELSE() extends UclidToken
case class KWINPUT() extends UclidToken
case class KWOUTPUT() extends UclidToken
case class KWCONST() extends UclidToken
case class KWFUNC() extends UclidToken
case class KWDEF() extends UclidToken
case class KWMODULE() extends UclidToken
case class KWTYPE() extends UclidToken
case class KWCONTROL() extends UclidToken
case class KWINIT() extends UclidToken
case class KWNEXT() extends UclidToken
case class KWINVARIANT() extends UclidToken
case class KWCASE() extends UclidToken
case class KWESAC() extends UclidToken
case class KWDEFAULT() extends UclidToken
case class KWENUM() extends UclidToken
case class KWRECORD() extends UclidToken
case class KWFORALL() extends UclidToken
case class KWEXISTS() extends UclidToken
case class KWHAVOC() extends UclidToken
case class KWASSUME() extends UclidToken
case class KWASSERT() extends UclidToken
case class KWOPTION() extends UclidToken
case class KWSYNTHESIS() extends UclidToken
case class KWGETVALUE() extends UclidToken
case class KWCHECK() extends UclidToken
case class KWTRACE() extends UclidToken
case class KWAXIOM() extends UclidToken
case class KWLET() extends UclidToken