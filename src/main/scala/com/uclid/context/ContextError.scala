package com.uclid.context

sealed trait ContextError extends Throwable

case class SemanticError(msg: String) extends ContextError
