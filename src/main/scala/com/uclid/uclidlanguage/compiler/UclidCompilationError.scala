package com.uclid.uclidlanguage.compiler

sealed trait UclidCompilationError

case class UclidLexerError(location: Location, msg: String) extends UclidCompilationError
case class UclidParserError(location: Location, msg: String) extends UclidCompilationError

case class Location(line: Int, column: Int) {
  override def toString = s"$line:$column"
}
