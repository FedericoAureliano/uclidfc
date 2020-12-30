package uclid

abstract class UclidJvmError(msg: String)
    extends java.lang.RuntimeException(msg) {}

class SolverMismatchError(msg: String) extends UclidJvmError(msg) {}
