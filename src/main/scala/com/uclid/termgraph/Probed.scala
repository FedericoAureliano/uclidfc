package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

trait Probed() extends AbstractTermGraph {

  private var synthesis : Option[Boolean] = None

  def isSynthesisQuery(entryPoints: List[Int] = List.empty) : Boolean = {
    if (synthesis.isDefined) {
      synthesis.get
    } else {
      synthesis = Some(logicComponents(entryPoints).toMap.getOrElse("SY", 0) > 0)
      synthesis.get
    }
  }

  def featuresList(entryPoints: List[Int]): List[String] = 
    List(
      "Term graph size: " + numberOfNodes().toString,
      "Number of variables: " + numberOfVariables(entryPoints).toString,
      "Largest integer literal: " + largestIntegerLiteral(entryPoints).toString,
      "Logic components:\n" + logicComponents(entryPoints)
        .map((logic, fraction) => s"---- $logic: $fraction")
        .mkString("\n"),
      "Max Arity: " + maxArity(entryPoints).toString,
      "Avg Arity: " + avgArity(entryPoints).toString
    )

  def numberOfNodes(): Int = getStmts().length
  def numberOfMemoEntries(): Int = memo.keys.toList.length
  def numberOfVariables(entryPoints: List[Int]): Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).length
  }

  // these probes are completely untested.. 
  def numberOfNullaryVariables(): Int = 
    getStmts().filter(p => p.isInstanceOf[UserFunction]).filter(p =>
    p.asInstanceOf[UserFunction].params.size==0).length
  
  def numberOfUFs(): Int = 
    getStmts().filter(p => p.isInstanceOf[UserFunction]).filter(p => 
    p.asInstanceOf[UserFunction].params.size>0).length
  
  def numberOfIntegerLiterals(): Int = 
    getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => 
    p.asInstanceOf[TheoryMacro].name.forall(_.isDigit)).length

  def numberOfForalls(): Int = 
      getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => p.asInstanceOf[TheoryMacro].name == "forall").length
      
  def numberOfExists(): Int = 
      getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => p.asInstanceOf[TheoryMacro].name == "exists").length

  def numberOfQuantfiers(): Int = 
      numberOfForalls() + numberOfExists()
 
  def numerOfUSorts(): Int = getStmts().filter(p => p.isInstanceOf[UserSort]).length

  def numberOfQuantifiedVars(): Int = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro("forall", params) => sum += params.length
          case TheoryMacro("exists", params) => sum += params.length
          case _ =>
        }
      )
    sum
  }

  
  def countConsecutiveQuantifiers(expr: Application, count: Int, previousQuantifier: String): Int = 
  {
    var result: Int = count
    var isQuant: Boolean = false;

    getStmt(expr.caller) match {
      case TheoryMacro("exists", _) | TheoryMacro("forall", _)=> 
      {
        isQuant=true
        if(previousQuantifier != getStmt(expr.caller).asInstanceOf[TheoryMacro].name)  
          result +=1;
      }
      case _ => isQuant=false
    }
    if(isQuant && getStmt(expr.args.head).isInstanceOf[Application])
      countConsecutiveQuantifiers(getStmt(expr.args.head).asInstanceOf[Application], result, previousQuantifier)
    else
      result 
  }

  def sumQuantifierAlternations(): Int = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case Application(function, predicate) => 
            getStmt(function)  match {
              case TheoryMacro("forall", _) | TheoryMacro("exists", _) => 
                sum = sum + countConsecutiveQuantifiers(
                            getStmt(predicate.head).asInstanceOf[Application], 1, 
                            getStmt(function).asInstanceOf[TheoryMacro].name)
              case _ => sum
            }
          case _ => sum
        })
      sum
  }  

  def maxQuantifierAlternations(): Int = {
    var max: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case Application(function, predicate) => 
            getStmt(function)  match {
              case TheoryMacro("forall", _) | TheoryMacro("exists", _)=> 
                var new_alternations = countConsecutiveQuantifiers(
                                        getStmt(predicate.head).asInstanceOf[Application], 1, 
                                        getStmt(function).asInstanceOf[TheoryMacro].name)
                if(new_alternations > max)
                  max = new_alternations
              case _ => max
            }
          case _ => max
        })
      max
  }   


  def sumIntegerLiteral(entryPoints: List[Int]): Int = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            name.toIntOption match {
              case Some(value) => sum += value
              case None =>
            }
          case _ =>
        }
      )
    sum
  }

  def largestIntegerLiteral(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            name.toIntOption match {
              case Some(value) =>
                if (value >= max.getOrElse(value)) {
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
  def maxArity(entryPoints: List[Int]): Int = {
    var max: Option[Int] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case UserFunction(_, _, params) =>
            //value = # args to function
            if (params.length >= max.getOrElse(params.length)) {
              max = Some(params.length)
            }
          case _ =>
        }
      )
      max.getOrElse(0)
  }


  def avgArity(entryPoints: List[Int]): Float = {
    var avg: Float = 0.0
    var count: Float = 0.0
    getStmts()
      .foreach(inst =>
        inst match {
          case UserFunction(_, _, params) => {
            // only count arity of things that have arity > 0
            if (params.length > 0) {
              count = count + 1
              avg = (1 / count) * params.length + ((count - 1) / count) * avg
            }
          }
          case _ =>
        }
      )
      avg
  }

  
  def logicComponents(entryPoints: List[Int]): List[(String, Int)] = {
    var q = 0
    var uf = 0
    var a = 0
    var dt = 0
    var lia = 0
    var nia = 0
    var s = 0
    var bv = 0
    var sy = 0

    val marks = mark(entryPoints)

    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if (marked) {
          inst match {
            case Application(caller, args) =>
              (caller :: args).foreach(pos => {
                getStmt(pos) match {
                  case TheoryMacro("*", _) =>
                    if (
                      args.filter { a =>
                        getStmt(a) match {
                          case TheoryMacro(name, _) =>
                            name.toIntOption.isDefined
                          case _ => false
                        }
                      }.length < args.length - 1
                    ) {
                      nia += 1
                    } else {
                      lia += 1
                    }
              
                  // TODO: add all string ops
                  case TheoryMacro("str.++", _) => s += 1
                  case TheoryMacro("str.indexof", _) => s += 1
                  case TheoryMacro("str.substr", _) => s += 1
                  case TheoryMacro("str.len", _) => s += 1
                  case TheoryMacro("str.contains", _) => s += 1
                  case TheoryMacro("str.prefixof", _) => s += 1
                  case TheoryMacro("str.suffixof", _) => s += 1
                  case TheoryMacro("str.replace", _) => s += 1
                  case TheoryMacro("str.at", _) => s += 1

                  // TODO: add all bitvector ops
                  case TheoryMacro("bvadd", _) => bv += 1
                  case TheoryMacro("bvsub", _) => bv += 1
                  case TheoryMacro("bvand", _) => bv += 1
                  case TheoryMacro("bvor", _) => bv += 1
                  case TheoryMacro("bvmul", _) => bv += 1
                  case TheoryMacro("bvudiv", _) => bv += 1
                  case TheoryMacro("bvurem", _) => bv += 1
                  case TheoryMacro("bvshl", _) => bv += 1
                  case TheoryMacro("bvlshr", _) => bv += 1
                  case TheoryMacro("bvnot", _) => bv += 1
                  case TheoryMacro("bvneg", _) => bv += 1
                  case TheoryMacro("bvult", _) => bv += 1
                  case TheoryMacro("concat", _) => bv += 1
                  case TheoryMacro("extract", _) => bv += 1
                  case TheoryMacro("bv2nat", _) => bv += 1
                  case TheoryMacro("nat2bv", _) => bv += 1

                  case TheoryMacro("+", _) => lia += 1
                  case TheoryMacro("-", _) => lia += 1
                  case TheoryMacro("forall", _) => q += 1
                  case TheoryMacro("exists", _) => q += 1
                  case TheoryMacro("select", _) => a += 1
                  case TheoryMacro("store", _) => a += 1
                  case UserFunction(_, _, args) => if (args.length > 0) uf += 1
                  case Constructor(_, _, _) => dt += 1
                  case Selector(_, _) => dt += 1
                  case _                   =>
                }
              })
            case _ : Synthesis => sy += 1
            case _ =>
          }
        }
      )

    List(
      ("SY", sy),
      ("Q", q),
      ("UF", uf),
      ("A", a),
      ("DT", dt),
      ("LIA", lia),
      ("NIA", nia),
      ("S", s),
      ("BV", bv)
    )
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
        if (marked) {
          inst match {
            case _: AbstractDataType => dt = true
            case Application(caller, args) =>
              getStmt(caller) match {
                case TheoryMacro("*", _) =>
                  if (
                    args.filter { a =>
                      getStmt(a) match {
                        case TheoryMacro(name, _) =>
                          name.toIntOption.isDefined
                        case _ => false
                      }
                    }.length < args.length - 1
                  ) {
                    linear = false
                  }
                case _ =>
              }
            case TheoryMacro("exists", _) => qf = false
            case TheoryMacro("forall", _) => qf = false
            case TheoryMacro(name, _) =>
              if (name.toIntOption.isDefined) { i = true }
            case UserFunction(_, _, params) =>
              if (params.length > 0) { uf = true }
            case TheorySort("Array", _) => a = true
            case TheorySort("Int", _)   => i = true
            case TheorySort("String", _)   => s = true
            case TheorySort("BitVec", _)   => bv = true
            case Synthesis(_, _, _)     => sy = true
            case UserSort(_, _) => uf = true
            case _                      =>
          }
        }
      )

    val out = s"${if (qf && !sy) { "QF_" }
    else { "" }}${if (uf) { "UF" }
    else { "" }}${if (bv) { "BV" }
    else { "" }}${if (s) { "S" }
    else { "" }}${if (a) { "A" }
    else { "" }}${if (dt) { "DT" }
    else { "" }}${if (linear && i) { "L" }
    else if (!linear && i) { "N" }
    else { "" }}${if (i) { "IA" }
    else { "" }}"

    if (out == "QF_") {
      "QF_UF"
    } else {
      out
    }
  }
}
