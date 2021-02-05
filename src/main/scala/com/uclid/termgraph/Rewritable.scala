package com.uclid.termgraph

import com.uclid.context._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait Rewritable() extends AbstractTermGraph {

  def plusMinusZero() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(plus, args)  if stmts(plus) == TheoryMacro("+", List.empty) => {
          val newArgs : List[Int] = args.foldLeft(List.empty)((acc, a) => {
            stmts(a) match {
              case TheoryMacro("0", _) => acc
              case _ => acc ++ List(a)
            }
          })
          if (newArgs.length == 0) {
            memoUpdateInstruction(p, TheoryMacro("0", List.empty))
          } else {
            memoUpdateInstruction(p, Application(plus, newArgs))
          }
        }
        case Application(minus, x::y::Nil)  if stmts(minus) == TheoryMacro("-", List.empty) => {
          if (stmts(x) == TheoryMacro("0", List.empty)) {
            memoUpdateInstruction(p, Application(minus, y :: Nil))
          } else if (stmts(y) == TheoryMacro("0", List.empty)) {
            memoUpdateInstruction(p, stmts(x))
          }
        }
        case _ =>
      }
    }
  }

  def indexOfGteZeroGadget() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(gte, t::zero::Nil) if stmts(zero) == TheoryMacro("0", List.empty) => {
          stmts(gte) match {
            case TheoryMacro(">=", _) => {
              stmts(t) match {
                case Application(indexof, x::needle::offset::Nil) if offset == zero => {
                  stmts(indexof) match {
                    case TheoryMacro("str.indexof", _) => {
                      val contains = memoAddInstruction(TheoryMacro("str.contains", List.empty))
                      memoUpdateInstruction(p, Application(contains, x::needle::Nil))
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
  }


  def assertionOverConjunction(assertion: Assert) : List[Command] = {
    var changeHappened = true
    var newCommands : List[Command] = List(assertion)
    while (changeHappened) {
      changeHappened = false
      newCommands = newCommands.foldLeft(List.empty)((acc1, c) => 
        c match {
          case ass : Assert => {
            stmts(ass.t) match {
              case Application(op, operands) => {
                stmts(op) match {
                  case TheoryMacro("and", _) => {
                    changeHappened = true
                    acc1 ++ operands.foldLeft(List.empty)((acc2, o) => Assert(o) :: acc2)
                  }
                  case _ => acc1 ++ List(c)
                }
              }
              case _ => acc1 ++ List(c)
            }
          }
          case _ => acc1 ++ List(c)
        })
    }
    newCommands
  }

  def containsOverConcat() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(contains, haystack::needle::Nil) if stmts(needle).isInstanceOf[TheoryMacro] && stmts(needle).asInstanceOf[TheoryMacro].name.length == 3 => {
          stmts(contains) match {
            case TheoryMacro("str.contains", _) => {
              stmts(haystack) match {
                case Application(concat, strings) => {
                  stmts(concat) match {
                    case TheoryMacro("str.++", _) => {
                      // we have a contains over a concat. Make it a disjunction
                      val orRef = memoAddInstruction(TheoryMacro("or"))
                      val components = strings.map(s => {
                        memoAddInstruction(Application(contains, List(s, needle)))
                      })
                      memoUpdateInstruction(p, Application(orRef, components))
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
  }

  def containsOverReplace() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(contains, haystack::needle::Nil) if stmts(needle).isInstanceOf[TheoryMacro] && stmts(needle).asInstanceOf[TheoryMacro].name.length == 3 => {
          stmts(contains) match {
            case TheoryMacro("str.contains", _) => {
              stmts(haystack) match {
                case Application(replace, x::oldChar::newChar::Nil) if stmts(oldChar).isInstanceOf[TheoryMacro] && stmts(oldChar).asInstanceOf[TheoryMacro].name.length == 3 && stmts(newChar).isInstanceOf[TheoryMacro] && stmts(newChar).asInstanceOf[TheoryMacro].name.length == 3 => {
                  stmts(replace) match {
                    case TheoryMacro("str.replace", _) => {
                      if (oldChar == needle && newChar != needle) {
                        memoUpdateInstruction(p, TheoryMacro("false", List.empty))
                      } else if (oldChar == needle && newChar == needle) {
                        memoUpdateInstruction(p, Application(contains, List(x, needle)))
                      } else if (oldChar != needle && newChar == needle) {
                        val orRef = memoAddInstruction(TheoryMacro("or"))
                        memoUpdateInstruction(p, Application(orRef, List(memoAddInstruction(Application(contains, List(x, needle))), memoAddInstruction(Application(contains, List(x, oldChar))))))
                      } else {
                        // they are both not equal so just ingore them altogether
                        memoUpdateInstruction(p, Application(contains, List(x, needle)))
                      }
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
  }

  def lengthOverSubstring() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(length, t :: Nil) => {
          stmts(length) match {
            case TheoryMacro("str.len", _) => {
              stmts(t) match {
                case Application(substr, x::start::finish::Nil) => {
                  stmts(substr) match {
                    case TheoryMacro("str.substr", _) => {
                      val sub = memoAddInstruction(TheoryMacro("-"))
                      val zero = memoAddInstruction(TheoryMacro("0"))
                      val lt = memoAddInstruction(TheoryMacro("<"))
                      val ite = memoAddInstruction(TheoryMacro("ite"))

                      val lenX = memoAddInstruction(Application(length, List(x)))
                      val trimStart = memoAddInstruction(Application(sub, List(lenX, start)))
                      val trimEnd = memoAddInstruction(Application(sub, List(finish, lenX)))
                      val total = memoAddInstruction(Application(sub, List(trimStart, trimEnd)))

                      val condition = memoAddInstruction(Application(lt, List(total, zero)))
                      memoUpdateInstruction(p, Application(ite, List(condition, zero, total)))
                    }
                    case _ =>
                  }
                }
                case _ =>
              }
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
  }

  // returns auxiliary assertions
  def indexOfSubstringGadget() : List[Assert] = {
    (0 to stmts.length - 1).foldLeft(List.empty) { (acc, p) =>
      stmts(p) match {
        case Application(indexof, t :: c :: offset :: Nil) => {
          stmts(indexof) match {
            case TheoryMacro("str.indexof", _) => {
              stmts(t) match {
                case Application(substr, x::k::lenMinusK::Nil) => {
                  stmts(substr) match {
                    case TheoryMacro("str.substr", _) => {
                      stmts(lenMinusK) match {
                        case Application(minus, lenx :: j :: Nil) if j == k && stmts(minus) == TheoryMacro("-", List.empty) => {
                          stmts(lenx) match {
                            case Application(len, r::Nil) if r == x => {
                              stmts(len) match {
                                case TheoryMacro("str.len", _) => {
                                  val stringSort = memoAddInstruction(TheorySort("String"))
                                  val gte = memoAddInstruction(TheoryMacro(">="))
                                  val implies = memoAddInstruction(TheoryMacro("=>"))
                                  val and = memoAddInstruction(TheoryMacro("and"))
                                  val eq = memoAddInstruction(TheoryMacro("="))
                                  val concat = memoAddInstruction(TheoryMacro("str.++"))

                                  val y = memoAddInstruction(UserFunction(freshSymbolName(), stringSort))
                                  memoUpdateInstruction(t, UserFunction(freshSymbolName(), stringSort)) // this is z
                                  val yz = memoAddInstruction(Application(concat, List(y, t)))
                                  val xeqyz = memoAddInstruction(Application(eq, List(x, yz)))
                                  
                                  val leny = memoAddInstruction(Application(len, List(y)))
                                  val lenx = memoAddInstruction(Application(len, List(x)))
                                  val lenyeqk = memoAddInstruction(Application(eq, List(leny, k)))
                                  
                                  val rhs = memoAddInstruction(Application(and, List(xeqyz, lenyeqk)))
                                  val lhs = memoAddInstruction(Application(gte, List(lenx, k)))
                                  val aux = memoAddInstruction(Application(implies, List(lhs, rhs)))

                                  Assert(aux) :: acc
                                }
                                case _ => acc
                              }
                            }
                            case _ => acc
                          }
                        }
                        case _ => acc
                      }
                    }
                    case _ => acc
                  }
                }
                case _ => acc
              }
            }
            case _ => acc
          }
        }
        case _ => acc
      }
    }
  }

  /** Rewrite quantifiers over enums to disjunctions/conjunctions
    *
    * This function rewrites
    * "forall (x : [some enum], ...) :: e" to "forall (...) :: e[x/x_1] /\ ... /\ e[x/x_n]"
    * "exists (x : [some enum], ...) :: e" to "exists (...) :: e[x/x_1] \/ ... \/ e[x/x_n]"
    * where x_i are the elements of [some enum]. If the resulting quantifier is empty,
    * then it is eliminated.
    */
  def blastEnumQuantifier(): Unit = {
    var changeHappened = true
    while (changeHappened) {
      changeHappened = false
      (0 to stmts.length - 1).foreach { p =>
        val update = blastEnumQuantifier(p)
        changeHappened = update || changeHappened
      }
    }
  }

  /** Match "forall (x : [some enum], ...) :: e" or "exists (x : [some enum], ...) :: e" and call helper to do rewrite.
    *
    * @param app candidate to match; if app is not of the correct form then nothing happens.
    * @return true iff a change was made
    *
    * TODO: handle case where enum quantifier is not in first position
    */
  private def blastEnumQuantifier(app: Int): Boolean =
    stmts(app) match {
      case Application(quant, body :: Nil)
          if stmts(quant).isInstanceOf[TheoryMacro] =>
        stmts(quant).asInstanceOf[TheoryMacro] match {
          case TheoryMacro("forall", v :: vs) =>
            val copies = blastEnumQuantifier(body, v).getOrElse(
              return false
            )
            val andRef = memoAddInstruction(TheoryMacro("and"))
            val new_body = memoAddInstruction(Application(andRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, stmts(new_body))
              return true
            } else {
              memoUpdateInstruction(quant, TheoryMacro("forall", vs))
              memoUpdateInstruction(body, stmts(new_body))
              return true
            }
          case TheoryMacro("exists", v :: vs) =>
            val copies = blastEnumQuantifier(body, v).getOrElse(
              return false
            )
            val orRef = memoAddInstruction(TheoryMacro("or"))
            val new_body = memoAddInstruction(Application(orRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, stmts(new_body))
              return true
            } else {
              memoUpdateInstruction(quant, TheoryMacro("exists", vs))
              memoUpdateInstruction(body, stmts(new_body))
              return true
            }
          case _ => return false
        }
      case _ => return false
    }

  /** Copy quantifier body and plug in all variants of [some enum]
    *
    * "forall (v : [some enum], ...) :: body"
    * "exists (v : [some enum], ...) :: body"
    *
    * @param body
    * @param v
    * @return |[some enum]| copies of body, each with a different variant of [some enum] plugged in for v.
    */
  private def blastEnumQuantifier(body: Int, v: Int): Option[List[Int]] = {
    val param = stmts(v).asInstanceOf[FunctionParameter]
    val variants = stmts(param.sort) match {
      case DataType(_, constructors) =>
        constructors.map { p =>
          val ctr = stmts(p).asInstanceOf[Constructor]
          if (ctr.selectors.length > 0) {
            // not an enum
            return None
          }
          memoAddInstruction(TheoryMacro(ctr.name))
        }
      case _ => return None
    }
    // make n copies of the body
    val copies = variants.map { x =>
      val newBody = copyTerm(body)
      // v is the bound variable we want to replace
      // x is the enum variant we want to plug in for v
      val replaceMap = HashMap((v, x))
      updateTerm(newBody, replaceMap)
      newBody
    }
    Some(copies)
  }

  /** Add prefix to all names in query
    *
    * @param prefix prefix to add to all names
    */
  def prefixNames(prefix: String): Unit =
    (0 to stmts.length - 1).foreach { i =>
      stmts(i) match {
        case Constructor(name, sort, selectors) =>
          memoUpdateInstruction(
            i,
            Constructor(name, sort, selectors)
          )
        case DataType(name, constructors) =>
          memoUpdateInstruction(i, DataType(prefix + name, constructors))
        case FunctionParameter(name, sort) =>
          memoUpdateInstruction(i, FunctionParameter(prefix + name, sort))
        case Module(name, ct, init, next, spec) =>
          memoUpdateInstruction(
            i,
            Module(prefix + name, ct, init, next, spec)
          )
        case Ref(loc, named) =>
          memoUpdateInstruction(i, Ref(loc, prefix + named))
        case Selector(name, sort) =>
          memoUpdateInstruction(i, Selector(prefix + name, sort))
        case Synthesis(name, sort, params) =>
          memoUpdateInstruction(
            i,
            Synthesis(prefix + name, sort, params)
          )
        case UserFunction(name, sort, params) =>
          memoUpdateInstruction(
            i,
            UserFunction(prefix + name, sort, params)
          )
        case UserMacro(name, sort, body, params) =>
          memoUpdateInstruction(
            i,
            UserMacro(prefix + name, sort, body, params)
          )
        case UserSort(name, arity) =>
          memoUpdateInstruction(i, UserSort(prefix + name, arity))
        case _ => // do nothing, instruction doesn't have a name to prefix.
      }
    }

  /** Increment all references by ``offset''
    *
    * @param offset integer value to increase references by
    */
  def incrementRefs(offset: Int): Unit = {

    def bump(r: Int): Int = r + offset

    def bumpList(rs: List[Int]): List[Int] =
      rs.map(r => bump(r))

    (0 to stmts.length - 1).foreach { i =>
      stmts(i) match {
        case Application(caller, args) =>
          memoUpdateInstruction(
            i,
            Application(bump(caller), bumpList(args))
          )
        case Constructor(name, sort, selectors) =>
          memoUpdateInstruction(
            i,
            Constructor(name, bump(sort), bumpList(selectors))
          )
        case DataType(name, constructors) =>
          memoUpdateInstruction(
            i,
            DataType(name, bumpList(constructors))
          )
        case FunctionParameter(name, sort) =>
          memoUpdateInstruction(
            i,
            FunctionParameter(name, bump(sort))
          )
        case Module(name, ct, init, next, spec) =>
          memoUpdateInstruction(
            i,
            Module(name, bump(ct), bump(init), bump(next), bump(spec))
          )
        case Ref(loc, named) =>
          memoUpdateInstruction(i, Ref(bump(loc), named))
        case Selector(name, sort) =>
          memoUpdateInstruction(i, Selector(name, bump(sort)))
        case Synthesis(name, sort, params) =>
          memoUpdateInstruction(
            i,
            Synthesis(name, bump(sort), bumpList(params))
          )
        case TheoryMacro(name, params) =>
          memoUpdateInstruction(
            i,
            TheoryMacro(name, bumpList(params))
          )
        case TheorySort(name, params) =>
          memoUpdateInstruction(
            i,
            TheorySort(name, bumpList(params))
          )
        case UserFunction(name, sort, params) =>
          memoUpdateInstruction(
            i,
            UserFunction(name, bump(sort), bumpList(params))
          )
        case UserMacro(name, sort, body, params) =>
          memoUpdateInstruction(
            i,
            UserMacro(name, bump(sort), bump(body), bumpList(params))
          )
        case _ => // do nothing, no refs to bump
      }
    }
  }

  /** Copy an application term by inserting new copies of all instructions and returning a map describing the copy
    *
    * @param pos the starting location
    * @return the new location
    */
  protected def copyTerm(pos: Int): Int = {
    assert(
      stmts(pos).isInstanceOf[Application],
      s"should be an application but is ${stmts(pos)}"
    )
    val map = copyTermHelper(pos)
    updateTerm(map(pos), map)
    map(pos)
  }

  /** Copies the term but instead of updating references, returns a map describing the changes
    *
    * @param pos start location of the term
    * @param map map from locations to locations describing the changes
    * @return the map describing the term copy
    */
  private def copyTermHelper(
    pos: Int,
    map: HashMap[Int, Int] = HashMap.empty
  ): HashMap[Int, Int] = {
    stmts(pos) match {
      case Application(caller, args) =>
        val newCaller = copyTermHelper(caller, map).getOrElse(caller, caller)
        val newArgs = args.map(a => copyTermHelper(a, map).getOrElse(a, a))
        val newPos: Int = addInstruction(Application(newCaller, newArgs))
        map.addOne((pos, newPos))
      case _ =>
    }
    map
  }

  /** Updates the references in an application using the map
    *
    * @param pos starting location
    * @param map the changes we want to make: whenever we see x we will replace it with map(x)
    */
  protected def updateTerm(pos: Int, map: HashMap[Int, Int]): Unit =
    stmts(pos) match {
      case Application(caller, args) =>
        memoUpdateInstruction(
          pos,
          Application(
            map.getOrElse(caller, caller),
            args.map(a => map.getOrElse(a, a))
          )
        )
        updateTerm(caller, map)
        args.foreach(a => updateTerm(a, map))
      case _ =>
    }
}
