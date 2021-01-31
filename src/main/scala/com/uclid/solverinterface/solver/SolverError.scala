package com.uclid.solverinterface.solver

sealed trait SolverError extends Throwable

case class SolverMismatchError(msg: String) extends SolverError
