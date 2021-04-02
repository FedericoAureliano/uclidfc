package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

trait Probed() extends AbstractTermGraph {

  def isSynthesisQuery(entryPoints: List[Int]): Boolean =
    val marks = mark(entryPoints)
    getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[Synthesis])
      .length > 0

  def featuresList(entryPoints: List[Int]): List[String] =
    featureMap(entryPoints).toList.map((k, v) => s"$k: $v")

  def featureMap(entryPoints: List[Int]): Map[String, String] = {
    val combinedSeq : Map[String, Option[Any]] = Map(
      ("Logic", Some(queryLogic(entryPoints))),
      ("Term graph size", numberOfNodes()),
      ("Number of asserts", Some(entryPoints.size)),
      ("Number of variables", numberOfVariables(entryPoints)),
      ("Number of free bits", numberOfBits(entryPoints)),
      (
        "Number of integer variables",
        numberOfIntegerVariables(entryPoints)
      ),
      (
        "Number of bit-vector variables",
        numberOfBitVecVariables(entryPoints)
      ),
      (
        "Number of array variables",
        numberOfArrayVariables(entryPoints)
      ),
      (
        "Number of nullary variables",
        numberOfNullaryVariables(entryPoints)
      ),
      ("Number of multi-ary variables", numberOfUFs(entryPoints)),
      ("Number of uninterpreted sorts", numberOfUSorts()),
      ("Largest integer literal", largestIntegerLiteral(entryPoints)),
      ("Sum of integer literals", sumIntegerLiteral(entryPoints)),
      ("Largest bit-vector literal", largestBVliteral(entryPoints)),
      ("Sum of bit-vector literals", sumBVliteral(entryPoints)),
      ("Number of unique integer literals", numberOfIntegerLiterals()),
      ("Number of unique BV literals", numberOfBVLiterals()),
      ("Number of free bits", numberOfBits(entryPoints)),
      (
        "Max consecutive quantifier alternations",
        maxQuantifierAlternations()
      ),
      ("Max nested stores", maxNestedStores()),
      ("Max Arity", maxArity(entryPoints)),
      ("Avg Arity", avgArity(entryPoints))
    ) ++ countOperators(entryPoints)

    combinedSeq.collect {
      case (key, Some(value)) => key -> value.toString()
    } 
  }

  def numberOfNodes(): Option[Int] = Some(getStmts().length)

  def numberOfVariables(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfBitVecVariables(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]
      )
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort)
          .asInstanceOf[TheorySort]
          .name == "BitVec"
      )
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfIntegerVariables(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]
      )
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort)
          .asInstanceOf[TheorySort]
          .name == "Int"
      )
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfArrayVariables(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]
      )
      .filter((p, i) =>
        getStmt(p.asInstanceOf[UserFunction].sort)
          .asInstanceOf[TheorySort]
          .name == "Array"
      )
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfNullaryVariables(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .filter((p, i) => p.asInstanceOf[UserFunction].params.size == 0)
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfUFs(entryPoints: List[Int]): Option[Int] = {
    val marks = mark(entryPoints)
    val value = getStmts().zipWithIndex
      .filter((p, i) => marks(i) && p.isInstanceOf[UserFunction])
      .filter((p, i) => p.asInstanceOf[UserFunction].params.size > 0)
      .length
    if value > 0 then Some(value) else None
  }

  def numberOfIntegerLiterals(): Option[Int] =
    val value = getStmts()
      .filter(p => p.isInstanceOf[TheoryMacro])
      .filter(p => p.asInstanceOf[TheoryMacro].name.forall(_.isDigit))
      .length
    if value > 0 then Some(value) else None

  def numberOfBVLiterals(): Option[Int] =
    val value = getStmts()
      .filter(p => p.isInstanceOf[TheoryMacro])
      .filter(p => BVString2Value(p.asInstanceOf[TheoryMacro].name) != None)
      .length
    if value > 0 then Some(value) else None

  def numberOfUSorts(): Option[Int] =
    val value = getStmts().filter(p => p.isInstanceOf[UserSort]).length
    if value > 0 then Some(value) else None

  def BVString2Value(bitvec: String): Option[Long] =
    if bitvec.startsWith("bv") then {
      bitvec.stripPrefix("bv").toLongOption
    } else if bitvec.startsWith("#b") then {
      try Some(java.lang.Long.parseUnsignedLong(bitvec.stripPrefix("#b"), 2))
      catch {
        case _ => None
      }
    } else if bitvec.startsWith("#x") then {
      try Some(java.lang.Long.parseUnsignedLong(bitvec.stripPrefix("#x"), 16))
      catch {
        case _ => None
      }
    } else {
      None;
    }

// number of free bits
// theory macro with name as const, param0 is thing, param1 is

// Application(TheoryMacro(“as const”, TYPE), BODY)
// Type is the type of the constant
// Body is a literal constant

// returns None if type has infinite number of values, Probably doesn't work for datatypes
  def getBitsInType(sort: Instruction): Option[Int] =
    sort match {
      case TheorySort =>
        sort.asInstanceOf[TheorySort].name match {
          case "Array" =>
            val indexMax = getBitsInType(
              getStmt(sort.asInstanceOf[TheorySort].params(0))
            )
            val elementMax = getBitsInType(
              getStmt(sort.asInstanceOf[TheorySort].params(1))
            )
            if indexMax == None || elementMax == None then { None }
            else { Some(indexMax.getOrElse(0) * elementMax.getOrElse(0)) }
          case "BitVec" =>
            Some(
              getStmt(sort.asInstanceOf[TheorySort].params.head)
                .asInstanceOf[Numeral]
                .value
            )
          case "Integer" => None
          case "Bool"    => Some(2)
          case _         => None
        }
      case UserSort => Some(sort.asInstanceOf[UserSort].arity.value)
      case _        => None
    }

  def numberOfBits(entryPoints: List[Int]): Option[Int] = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case UserFunction(name, sort, params) =>
            if params.size == 0 then {
              getBitsInType(getStmt(sort)) match {
                case Some(value) => sum += value
                case None        =>
              }
            }
          case _ =>
        }
      )
    if sum > 0 then Some(sum) else None
  }

  def countConsecutiveQuantifiers(
    expr: Instruction,
    count: Int,
    previousQuantifier: String
  ): Option[Int] = {
    var maxIncrement: Int = 0;
    val value = if expr.isInstanceOf[Application] then {
      getStmt(expr.asInstanceOf[Application].function) match {
        case TheoryMacro("exists", _) | TheoryMacro("forall", _) =>
          maxIncrement = countConsecutiveQuantifiers(
            getStmt(expr.asInstanceOf[Application].args.head),
            count,
            getStmt(expr.asInstanceOf[Application].function)
              .asInstanceOf[TheoryMacro]
              .name
          ).getOrElse(0)
          if
            previousQuantifier != getStmt(
              expr.asInstanceOf[Application].function
            ).asInstanceOf[TheoryMacro].name
          then
            maxIncrement = maxIncrement + 1;
        case _ =>
          if expr.asInstanceOf[Application].args.isEmpty then
            maxIncrement = 0
          else {
            maxIncrement = expr
              .asInstanceOf[Application]
              .args
              .filter(p => getStmt(p).isInstanceOf[Application])
              .map(arg =>
                countConsecutiveQuantifiers(
                  getStmt(arg).asInstanceOf[Application],
                  count,
                  previousQuantifier
                ).getOrElse(0)
              )
              .max
          }
      }
      count + maxIncrement
    } else
      count
    
    if value > 0 then Some(value) else None
  }

  def maxQuantifierAlternations(): Option[Int] = {
    var max: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case Application(function, predicate) =>
            getStmt(function) match {
              case TheoryMacro("forall", _) | TheoryMacro("exists", _) =>
                var new_alternations = countConsecutiveQuantifiers(
                  getStmt(predicate.head),
                  0,
                  getStmt(function).asInstanceOf[TheoryMacro].name
                ).getOrElse(0)
                if new_alternations > max then
                  max = new_alternations
              case _ => max
            }
          case _ => max
        }
      )
    
    if max > 0 then Some(max) else None
  }

  def countNestedStores(expr: Instruction, count: Int): Option[Int] = {
    var maxIncrement: Int = 0;
    val value = if expr.isInstanceOf[Application] then {
      getStmt(expr.asInstanceOf[Application].function) match {
        case TheoryMacro("store", _) =>
          maxIncrement = expr
            .asInstanceOf[Application]
            .args
            .filter(p => getStmt(p).isInstanceOf[Application])
            .map(arg =>
              countNestedStores(getStmt(arg).asInstanceOf[Application], count).getOrElse(0)
            )
            .max + 1
        case _ =>
          if expr.asInstanceOf[Application].args.isEmpty then
            maxIncrement = 0
          else {
            maxIncrement = expr
              .asInstanceOf[Application]
              .args
              .filter(p => getStmt(p).isInstanceOf[Application])
              .map(arg =>
                countNestedStores(getStmt(arg).asInstanceOf[Application], count).getOrElse(0)
              )
              .max
          }
      }
      count + maxIncrement
    } else
      count

    if value > 0 then Some(value) else None
  }

  def maxNestedStores(): Option[Int] = {
    var max: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case Application(function, predicate) =>
            getStmt(function) match {
              case TheoryMacro("store", _) =>
                var newNestings = countNestedStores(getStmt(predicate.head), 1).getOrElse(0)
                if newNestings > max then
                  max = newNestings
              case _ => max
            }
          case _ => max
        }
      )
    
    if max > 0 then Some(max) else None
  }

  def sumIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            name.toIntOption match {
              case Some(value) => sum += value
              case None        =>
            }
          case _ =>
        }
      )
    if sum > 0 then Some(sum) else None
  }

  def sumBVliteral(entryPoints: List[Int]): Option[Long] = {
    var sum: Long = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            BVString2Value(name) match {
              case Some(value) => sum += value
              case None        =>
            }
          case _ =>
        }
      )
    
    if sum > 0 then Some(sum) else None
  }

  def largestBVliteral(entryPoints: List[Int]): Option[Long] = {
    var max: Option[Long] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            BVString2Value(name) match {
              case Some(value) =>
                if value >= max.getOrElse(value) then {
                  max = Some(value)
                }
              case None =>
            }
          case _ =>
        }
      )
    max
  }

  def largestIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            name.toIntOption match {
              case Some(value) =>
                if value >= max.getOrElse(value) then {
                  max = Some(value)
                }
              case None =>
            }
          case _ =>
        }
      )
    max
  }

  // UF probes
  def maxArity(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case UserFunction(_, _, params) =>
            //value = # args to function
            if params.length >= max.getOrElse(params.length) then {
              max = Some(params.length)
            }
          case _ =>
        }
      )
    max
  }

  def avgArity(entryPoints: List[Int]): Option[Float] = {
    var avg: Float = 0.0
    var count: Float = 0.0
    getStmts()
      .foreach(inst =>
        inst match {
          case UserFunction(_, _, params) =>
            // only count arity of things that have arity > 0
            if params.length > 0 then {
              count = count + 1
              avg = (1 / count) * params.length + ((count - 1) / count) * avg
            }
          case _ =>
        }
      )
    
    if avg > 0 then Some(avg) else None
  }

  def countOperators(entryPoints: List[Int]): Map[String, Option[Int]] = {
    var foralls: Int = 0;
    var exists: Int = 0;
    var quants: Int = 0;
    var quantified: Int = 0;
    var select: Int = 0;
    var store: Int = 0;
    var asConst: Int = 0;

    val marks = mark(entryPoints)
    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if marked then {
          inst match {
            case Application(function, args) =>
              (function :: args).foreach { pos =>
                getStmt(pos) match {
                  case TheoryMacro("forall", params) =>
                    foralls += 1; quants += 1; quantified += params.size;
                  case TheoryMacro("exists", params) =>
                    exists += 1; quants += 1; quantified += params.size;
                  case TheoryMacro("store", params)    => store += 1;
                  case TheoryMacro("select", params)   => select += 1;
                  case TheoryMacro("as const", params) => select += 1;
                  case _                               =>
                }
              }
            case _ =>
          }
        }
      )
    Map(
      ("Number of foralls", foralls),
      ("Number of exists", exists),
      ("Number of quantifiers", quants),
      ("Number of quantified variables", quantified),
      ("Number of selects", select),
      ("Number of stores", store),
      ("Number of as consts", asConst)
    ).collect {
      case (key, value) if value > 0 => key -> Some(value)
    }
  }

  def queryLogic(entryPoints: List[Int]): String = {
    var uf = false
    var a = false
    var dt = false
    var i = false
    var linear = true
    var qf = true
    var s = false
    var bv = false
    var sy = false

    val marks = mark(entryPoints)

    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if marked then {
          inst match {
            case _: AbstractDataType => dt = true
            case Application(function, args) =>
              getStmt(function) match {
                case TheoryMacro("*", _) =>
                  if
                    args.filter { a =>
                      getStmt(a) match {
                        case TheoryMacro(name, _) =>
                          name.toIntOption.isDefined
                        case _ => false
                      }
                    }.length < args.length - 1
                  then {
                    linear = false
                  }
                case _ =>
              }
            case TheoryMacro("exists", _) => qf = false
            case TheoryMacro("forall", _) => qf = false
            case TheoryMacro(name, _) =>
              if name.toIntOption.isDefined then { i = true }
            case UserFunction(_, _, params) =>
              if params.length > 0 then { uf = true }
            case TheorySort("Array", _)  => a = true
            case TheorySort("Int", _)    => i = true
            case TheorySort("String", _) => s = true
            case TheorySort("BitVec", _) => bv = true
            case Synthesis(_, _, _)      => sy = true
            case UserSort(_, _)          => uf = true
            case _                       =>
          }
        }
      )

    val out = s"${if qf && !sy then { "QF_" }
    else { "" }}${if uf then { "UF" }
    else { "" }}${if bv then { "BV" }
    else { "" }}${if s then { "S" }
    else { "" }}${if a then { "A" }
    else { "" }}${if dt then { "DT" }
    else { "" }}${if linear && i then { "L" }
    else if !linear && i then { "N" }
    else { "" }}${if i then { "IA" }
    else { "" }}"

    if out == "QF_" then {
      "QF_UF"
    } else {
      out
    }
  }
}
