package com.uclid.solverinterface.compiler

sealed trait SmtCompilationError extends Throwable

case class SmtLexerError(msg: String)
    extends SmtCompilationError

case class SmtParserError(msg: String)
    extends SmtCompilationError