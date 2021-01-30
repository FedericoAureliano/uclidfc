package com.uclid.solverinterface

sealed trait ContextError extends Throwable

case class SemanticError(msg: String) extends ContextError