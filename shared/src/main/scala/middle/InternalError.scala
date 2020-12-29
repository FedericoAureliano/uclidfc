package middle

abstract class InternalError(msg: String)
    extends java.lang.RuntimeException(msg) {}

class RefOutOfBoundsError(msg: String) extends InternalError(msg) {}

class NotSupportedSynthesis(msg: String) extends InternalError(msg) {}
