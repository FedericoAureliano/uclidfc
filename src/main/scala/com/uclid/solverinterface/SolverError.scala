package com.uclid.solverinterface

sealed trait SolverError extends Throwable

case class SolverMismatchError(msg: String) extends SolverError