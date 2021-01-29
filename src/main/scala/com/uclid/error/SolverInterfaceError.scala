package com.uclid.error

abstract class UclidSolverInterfaceError(msg: String)
    extends java.lang.RuntimeException(msg) {}

class SolverMismatchError(msg: String) extends UclidSolverInterfaceError(msg) {}
