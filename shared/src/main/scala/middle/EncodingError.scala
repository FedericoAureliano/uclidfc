package middle

abstract class EncodingError(msg: String)
    extends java.lang.RuntimeException(msg) {}

class RefOutOfBoundsError(msg: String) extends EncodingError(msg) {}

class NotSupportedSynthesis(msg: String) extends EncodingError(msg) {}
