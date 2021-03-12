package com.uclid.context.solver

sealed trait SolverError extends Throwable

case class SolverMismatchError(msg: String) extends SolverError
