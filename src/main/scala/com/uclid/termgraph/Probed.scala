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
    featureMap(entryPoints).toList.map((k, v) => s"$k: $v")

  def featureMap(entryPoints: List[Int]) : Map[String, String] = {
    val combinedSeq = (Map(("Term graph size", numberOfNodes().toString),
      ("Number of variables", numberOfVariables(entryPoints).toString),
      ("Number of Integer variables", numberOfIntegerVariables(entryPoints).toString),
      ("Number of Bitvector variables", numberOfBitVecVariables(entryPoints).toString),
      ("Number of Array variables", numberOfArrayVariables(entryPoints).toString),
      ("Number of nullary variables", numberOfNullaryVariables(entryPoints).toString),
      ("Number of multi-ary variables", numberOfUFs(entryPoints).toString),
      ("Number of uninterpreted sorts", numberOfUSorts().toString),
      ("Largest integer literal", largestIntegerLiteral(entryPoints).toString),
      ("Sum of integer literals", sumIntegerLiteral(entryPoints).toString),
      ("Largest BitVec literal", largestBVliteral(entryPoints).toString),
      ("Sum of BitVec literals", sumBVliteral(entryPoints).toString),
      ("Number of unique integer literals", numberOfIntegerLiterals().toString),
      ("Number of unique BV literals", numberOfBVLiterals().toString),
      ("Number of quantifiers", numberOfQuantfiers().toString),
      ("Number of exists", numberOfExists().toString),
      ("Number of foralls", numberOfForalls().toString),
      ("Number of quantified vars", numberOfQuantifiedVars().toString),
      ("Max consecutive quantifier alternations", maxQuantifierAlternations().toString),
      ("Max Arity", maxArity(entryPoints).toString),
      ("Avg Arity", avgArity(entryPoints).toString)) ++ logicComponents(entryPoints))

    combinedSeq.map((k, v) => (k, v.toString))
  }

  def numberOfNodes(): Int = getStmts().length
  def numberOfVariables(entryPoints: List[Int]): Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).length
  }

  def numberOfBitVecVariables(entryPoints: List[Int]) : Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).asInstanceOf[TheorySort].name == "BitVec"
    ).length
  }

  def numberOfIntegerVariables(entryPoints: List[Int]) : Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).asInstanceOf[TheorySort].name == "Int"
    ).length
  }

  def numberOfArrayVariables(entryPoints: List[Int]) : Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).isInstanceOf[TheorySort]).filter((p,i) =>
    getStmt(p.asInstanceOf[UserFunction].sort).asInstanceOf[TheorySort].name == "Array"
    ).length
  }

  def numberOfNullaryVariables(entryPoints: List[Int]) : Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).filter((p,i) =>
    p.asInstanceOf[UserFunction].params.size==0).length
  }
  
  def numberOfUFs(entryPoints: List[Int]): Int = {
    val marks = mark(entryPoints)
    getStmts().zipWithIndex.filter((p, i) => marks(i) && p.isInstanceOf[UserFunction]).filter((p,i) => 
    p.asInstanceOf[UserFunction].params.size>0).length
  }
  
  def numberOfIntegerLiterals(): Int = 
    getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => 
    p.asInstanceOf[TheoryMacro].name.forall(_.isDigit)).length

  def numberOfBVLiterals(): Int = 
    getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => 
    BVString2Value(p.asInstanceOf[TheoryMacro].name)!=None).length

  def numberOfForalls(): Int = 
      getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => p.asInstanceOf[TheoryMacro].name == "forall").length
      
  def numberOfExists(): Int = 
      getStmts().filter(p => p.isInstanceOf[TheoryMacro]).filter(p => p.asInstanceOf[TheoryMacro].name == "exists").length

  def numberOfQuantfiers(): Int = 
      numberOfForalls() + numberOfExists()
 
  def numberOfUSorts(): Int = getStmts().filter(p => p.isInstanceOf[UserSort]).length

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

  def BVString2Value(bitvec: String): Option[Int] = {
      if (bitvec.startsWith("bv")){
        bitvec.stripPrefix("bv").toIntOption
      } else if (bitvec.startsWith("#b")) {
        try {
          Some(Integer.parseInt(bitvec.stripPrefix("#b"),2))
        } catch {
          case _ => None
        }
      } else if (bitvec.startsWith("#x")) {
        try {
          Some(Integer.parseInt(bitvec.stripPrefix("#x"),16))
        } catch {
          case _ => None
        }
      } else {
        None;
      }
  }


// returns -1 if type has infinite number of values
  // def getMaxOfType(sort: TheorySort): Some(Int) = {
  //   sort.name match{
  //     case "Array" => {
  //       getMaxOfType(getStmt(sort.args.head)) + getMaxOfType(getStmt(sort.args.)) 
  //     }
  //     case "BitVec" => getStmt(sort.params.head).toInt
  //     case "Integer" => -1
  //     case "Bool" => 2
  //     case _ => 
  //   }
  // }

  // def maxArraySize(): Some(Int) = {
  //   var max: Option[Int] = None
  //   getStmts()
  //     .foreach(inst =>
  //       inst match {
  //         case UserFunction(_, sort, _) =>
  //           //value = # args to function
  //           if (getStmt(sort).isInstanceOf[TheorySort])
  //             if (getStmt(sort).asInstanceOf[TheorySort].name=="Array")
  //             {
  //               val size = getStmt(getStmt(sort).asInstanceOf[TheorySort].params.head)
                
  //             }
  //             max = Some(params.length)
  //           }
  //         case _ =>
  //       }
  //     )
  //     max.getOrElse(0)
  // }

  
  def countConsecutiveQuantifiers(expr: Instruction, count: Int, previousQuantifier: String): Int = {
    var maxIncrement: Int = 0;
    if(expr.isInstanceOf[Application])
    {
      getStmt(expr.asInstanceOf[Application].function) match {
        case TheoryMacro("exists", _) | TheoryMacro("forall", _)=> 
        {
          maxIncrement = countConsecutiveQuantifiers(getStmt(expr.asInstanceOf[Application].args.head), count, getStmt(expr.asInstanceOf[Application].function).asInstanceOf[TheoryMacro].name);
          if(previousQuantifier != getStmt(expr.asInstanceOf[Application].function).asInstanceOf[TheoryMacro].name)  
            maxIncrement= maxIncrement+1;
        }
        case _ => 
        {
          if(expr.asInstanceOf[Application].args.isEmpty)
            maxIncrement=0
          else
          {
            maxIncrement = expr.asInstanceOf[Application].args.filter(p => getStmt(p).isInstanceOf[Application]).map(arg 
              => countConsecutiveQuantifiers(getStmt(arg).asInstanceOf[Application], count, previousQuantifier)).max
          }
        }
      }
      count + maxIncrement
    }
    else
      count  
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
                                        getStmt(predicate.head), 0, 
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

  def sumBVliteral(entryPoints: List[Int]): Int = {
    var sum: Int = 0
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            BVString2Value(name) match {
              case Some(value) => sum += value
              case None =>
            }
          case _ =>
        }
      )
    sum
  }

  def largestBVliteral(entryPoints: List[Int]): Option[Int] = {
    var max: Option[Int] = None
    getStmts()
      .foreach(inst =>
        inst match {
          case TheoryMacro(name, _) =>
            BVString2Value(name) match {
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

  
  def logicComponents(entryPoints: List[Int]): Map[String, Int] = {
    var q = 0
    var uf = 0
    var a = 0
    var dt = 0
    var lia = 0
    var nia = 0
    var s = 0
    var bv = 0
    var sy = 0
    var p = 0

    val marks = mark(entryPoints)

    marks
      .zip(getStmts())
      .foreach((marked, inst) =>
        if (marked) {
          inst match {
            case Application(function, args) =>
              (function :: args).foreach(pos => {
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
                  case TheoryMacro("str.replace", _) => s += 1
                  case TheoryMacro("str.at", _) => s += 1

                  // TODO: add all bitvector ops
                  case TheoryMacro("bvadd", _) => bv += 1
                  case TheoryMacro("bvsub", _) => bv += 1
                  case TheoryMacro("bvand", _) => bv += 1
                  case TheoryMacro("bvor", _) => bv += 1
                  case TheoryMacro("bvxor", _) => bv += 1
                  case TheoryMacro("bvmul", _) => bv += 1
                  case TheoryMacro("bvsdiv", _) => bv += 1
                  case TheoryMacro("bvudiv", _) => bv += 1
                  case TheoryMacro("bvurem", _) => bv += 1
                  case TheoryMacro("bvsrem", _) => bv += 1
                  case TheoryMacro("bvumod", _) => bv += 1
                  case TheoryMacro("bvsmod", _) => bv += 1
                  case TheoryMacro("bvshl", _) => bv += 1
                  case TheoryMacro("bvlshr", _) => bv += 1
                  case TheoryMacro("bvashr", _) => bv += 1
                  case TheoryMacro("bvnot", _) => bv += 1
                  case TheoryMacro("bvneg", _) => bv += 1
                  case TheoryMacro("concat", _) => bv += 1
                  case TheoryMacro("extract", _) => bv += 1
                  case TheoryMacro("bv2nat", _) => bv += 1
                  case TheoryMacro("nat2bv", _) => bv += 1
                  case TheoryMacro("zero_extend", _) => bv + 1 
                  case TheoryMacro("sign_extend", _) => bv + 1 
                  case TheoryMacro("rotate_left", _) => bv + 1 
                  case TheoryMacro("rotate_right", _) => bv + 1 
                  case TheoryMacro("repeat", _) => bv + 1

                  case TheoryMacro("+", _) => lia += 1
                  case TheoryMacro("-", _) => lia += 1
                  
                  case TheoryMacro("forall", _) => q += 1
                  case TheoryMacro("exists", _) => q += 1

                  case TheoryMacro("select", _) => a += 1
                  case TheoryMacro("store", _) => a += 1
                  
                  case UserFunction(_, _, args) => if (args.length > 0) uf += 1
                  case Constructor(_, _, _) => dt += 1
                  case Selector(_, _) => dt += 1

                  case TheoryMacro("bvult", _) => p += 1
                  case TheoryMacro("bvslt", _) => p += 1
                  case TheoryMacro("bvule", _) => p += 1
                  case TheoryMacro("bvsle", _) => p += 1
                  case TheoryMacro("bvugt", _) => p += 1
                  case TheoryMacro("bvsgt", _) => p += 1
                  case TheoryMacro("bvuge", _) => p += 1
                  case TheoryMacro("bvsge", _) => p += 1
                  case TheoryMacro("str.contains", _) => p += 1
                  case TheoryMacro("str.prefixof", _) => p += 1
                  case TheoryMacro("str.suffixof", _) => p += 1
                  case TheoryMacro("=", _) => p += 1
                  case TheoryMacro(">=", _) => p += 1
                  case TheoryMacro(">", _) => p += 1
                  case TheoryMacro("<=", _) => p += 1
                  case TheoryMacro("<", _) => p += 1
                  case TheoryMacro("=>", _) => p += 1
                  case TheoryMacro("and", _) => p += 1
                  case TheoryMacro("or", _) => p += 1
                  case TheoryMacro("not", _) => p += 1

                  case _                   =>
                }
              })
            case _ : Synthesis => sy += 1
            case _ =>
          }
        }
      )

    Map(
      ("SY", sy),
      ("Q", q),
      ("UF", uf),
      ("A", a),
      ("DT", dt),
      ("LIA", lia),
      ("NIA", nia),
      ("S", s),
      ("BV", bv),
      ("B", p)
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
            case Application(function, args) =>
              getStmt(function) match {
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
