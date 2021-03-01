package com.uclid.termgraph

import com.uclid.context._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}

trait Rewritable() extends AbstractTermGraph {

  def optimizeLevel0() : Unit = {
    inlineMacros()
  }
  
  def inlineMacros(bound: Int = -1) : Unit = {
    var count = bound
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        case Application(umacroPos, args) if stmts(umacroPos).isInstanceOf[UserMacro] => {
          val umacro = stmts(umacroPos).asInstanceOf[UserMacro]
          val bindings = umacro.params.zip(args).toMap
          val inlined = copyTerm(umacro.body)
          updateTerm(inlined, bindings)
          memoUpdateInstruction(p, Ref(inlined, None))
          count = count - 1
          if (count == 0) {
            return
          }
        }
        case _ =>
      }
    }
  }

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

  def indexOfGTZGadgets() : Unit = {
    (0 to stmts.length - 1).foreach { p =>
      stmts(p) match {
        // index of c in x >= 0 means x contains c 
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
        // index of c in x < 0 means x does not contain c 
        case Application(lt, t::zero::Nil) if stmts(zero) == TheoryMacro("0", List.empty) => {
          stmts(lt) match {
            case TheoryMacro("<", _) => {
              stmts(t) match {
                case Application(indexof, x::needle::offset::Nil) if offset == zero => {
                  stmts(indexof) match {
                    case TheoryMacro("str.indexof", _) => {
                      val contains = memoAddInstruction(TheoryMacro("str.contains", List.empty))
                      val not = memoAddInstruction(TheoryMacro("not", List.empty))
                      val containsApp = memoAddInstruction(Application(contains, x::needle::Nil))
                      memoUpdateInstruction(p, Application(not, List(containsApp)))
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
        // index of c in x = -1 means x does not contain c 
        case Application(eq, t::minusOne::Nil) if stmts(eq) == TheoryMacro("=") => {
          stmts(minusOne) match {
            case Application(minus, one::Nil) if stmts(minus) == TheoryMacro("-") && stmts(one) == TheoryMacro("1") => {
              stmts(t) match {
                case Application(indexof, x::needle::zero::Nil) if stmts(zero) == TheoryMacro("0", List.empty) => {
                  stmts(indexof) match {
                    case TheoryMacro("str.indexof", _) => {
                      val contains = memoAddInstruction(TheoryMacro("str.contains", List.empty))
                      val not = memoAddInstruction(TheoryMacro("not", List.empty))
                      val containsApp = memoAddInstruction(Application(contains, x::needle::Nil))
                      memoUpdateInstruction(p, Application(not, List(containsApp)))
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

  /**
    * "(replace c1 with c2 in x) contains c3" as "x contains c3" if c1 = c3 and c2 = c3; as "false" if c1 = c3 and c2 != c3; as "x contains c3" if c1 != c3 and c2 != c3; and "x contains c3 or x contains c1" if c1 != c3 and c2 == c3.
    */
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
          memoAddInstruction(Application(p, List.empty))
        }
      case _ => return None
    }
    // make n copies of the body
    val copies = variants.map { x =>
      val newBody = copyTerm(body)
      // v is the bound variable we want to replace
      // x is the enum variant we want to plug in for v
      val replaceMap = HashMap((v, x))
      updateTerm(newBody, replaceMap.toMap)
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
          named match {
            case None => memoUpdateInstruction(i, Ref(loc, None))
            case Some(name) => memoUpdateInstruction(i, Ref(loc, Some(prefix + name)))
          }
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
    // assert(
    //   stmts(pos).isInstanceOf[Application],
    //   s"should be an application but is ${stmts(pos)}"
    // )
    val map = copyTermHelper(pos)
    if (map.contains(pos)) {
      updateTerm(map(pos), map.toMap)
      map(pos)
    } else {
      pos
    }
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
        val newArgs = args.map(a => copyTermHelper(a, map).getOrElse(a, a))
        val newPos: Int = addInstruction(Application(caller, newArgs))
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
  protected def updateTerm(pos: Int, map: Map[Int, Int]): Unit = {
    assert(stmts(pos).isInstanceOf[Application] || stmts(pos).isInstanceOf[Ref])
    map.get(pos) match {
      case Some(value) =>
        memoUpdateInstruction(pos, Ref(value, None))
      case None => stmts(pos) match {
        case Application(caller, args) =>
          args.foreach(a => updateTerm(a, map))
          map.get(caller) match {
            case None =>
            case Some(value) => if (completeButUnapplied(value)) {
              memoUpdateInstruction(pos, Application(value, args))
            } else {
              // we're trying to replace caller---it better not have children
              assert(args.length == 0)
              memoUpdateInstruction(pos, stmts(value))
            }
          }
        case Ref(loc, named) =>
          updateTerm(loc, map)
          map.get(loc) match {
            case None =>
            case Some(value) => memoUpdateInstruction(pos, Ref(value, None))
          }
      }
    }
  }
}
