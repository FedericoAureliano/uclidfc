package com.uclid.error

abstract class EncodingError(msg: String)
    extends java.lang.RuntimeException(msg) {}

class RefOutOfBoundsError(msg: String) extends EncodingError(msg) {}

class NotSupportedSynthesis(msg: String) extends EncodingError(msg) {}

class NotSupportedYet(msg: String) extends EncodingError(msg) {}

class FuzzingError(msg: String) extends EncodingError(msg) {}

class VariableOverride(msg: String) extends EncodingError(msg) {}

class UnreachableError(msg: String) extends EncodingError(msg) {}
