package com.uclid.termgraph

import scala.collection.mutable.{ArrayBuffer, HashMap}

// Essentially an AST node or edge.
abstract class Instruction

abstract class AbstractDataType extends Instruction {
  def defaultCtr(): Int
}

// Points to another position in the array
case class Ref(loc: Int) extends Instruction {}

/*
Numerals are used for e.g. fixed-width bit-vectors
 */
case class Numeral(value: Int) extends Instruction {}

/*
Theory sorts are interpreted sorts like "Int." For example, 32-bit bit-vectors are represented by

[tst] BitVec  #1
[num] 32

The params can point to other sorts or numerals.
 */
case class TheorySort(name: String, params: List[Int] = List.empty)
    extends Instruction {}

/*
User sorts are uninterpreted sorts
 */
case class UserSort(name: String, arity: Numeral = Numeral(0))
    extends Instruction {}

/*
Function parameters, must be arity 0
 */
case class FunctionParameter(name: String, sort: Int) extends Instruction {}

/*
Theory macros are interpreted functions, like "+", "1", and "#b11110000".
Quantifiers are special in that they bind parameters. For example, (forall ((x Int)) (> 0 x)) is

[app] #4      #1
[app] #2      #3    #5
[tmo] >
[tmo] 0
[tmo] forall  #5
[fpr] x       #6
[tst] Int
 */
case class TheoryMacro(name: String, params: List[Int] = List.empty)
    extends Instruction {}

/*
User macros are function definitions like f(x) = x + x, which would be in LIR

[umo] f   #4    #1    #3
[app] #2  #3    #3
[tmo] +
[fpr] x   #4
[tst] Int
 */
case class UserMacro(
  name: String,
  sort: Int,
  body: Int,
  params: List[Int] = List.empty
) extends Instruction {}

/*
User functions are uninterpreted functions. For example, f(x : Int) : Int would be

[umo] f   #2    #1
[fpr] x   #2
[tst] Int
 */
case class UserFunction(name: String, sort: Int, params: List[Int] = List.empty)
    extends Instruction {}

case class Synthesis(
  name: String,
  sort: Int,
  params: List[Int] = List.empty
) extends Instruction {}

case class Constructor(
  name: String,
  sort: Int,
  selectors: List[Int] = List.empty
) extends Instruction {}

case class Selector(name: String, sort: Int) extends Instruction {}

/*
For algebraic datatypes like enums, records, and so on. For example a record R = {x: Int, y: Real} is

[adt] R     #1
[ctr] r     #0    #2    #3
[slr] x     #4
[slr] y     #5
[tst] Int
[tst] Real
 */
case class DataType(name: String, constructors: List[Int])
    extends AbstractDataType {
  override def defaultCtr(): Int = constructors.head
}

/*
A module is a record with associated init function, next function, and spec function.
 */
case class Module(name: String, ct: Int, init: Int, next: Int, spec: Int)
    extends AbstractDataType {
  override def defaultCtr(): Int = ct
}

case class Application(function: Int, args: List[Int]) extends Instruction {}