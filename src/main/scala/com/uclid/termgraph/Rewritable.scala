package com.uclid.termgraph

import com.uclid.context._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}

trait Rewritable() extends AbstractTermGraph {

  def optimizeLevel0(startingPoints: List[Int]): Unit = {
    var inlineResult = true
    var elimResult = true
    var keepGoing = inlineResult || elimResult
    var locations : List[Int] = terms(startingPoints).zipWithIndex.map((a, b) => if (a) {Some(b)} else {None}).flatten.toList
    while (keepGoing) { 
      // do this iteratively in a loop to avoid big blowup from inlining (which creates many destruct/construct applications)

      // inline one macro
      inlineResult = inlineMacros(locations, 1)
      // reduce all "select y from construct {x=a ... y=b ... z=c}" to "b"
      elimResult = eliminateDestructConstruct(locations, -1)

      keepGoing = inlineResult || elimResult
      locations = terms(startingPoints).zipWithIndex.map((a, b) => if (a) {Some(b)} else {None}).flatten.toList
    }

    splitRecords(locations)
  }

  def splitRecords(locations: List[Int]): Unit = {
    locations.foreach(p => {
      getStmt(p) match {
        case u : UserFunction if u.params.length == 0 && getStmt(u.sort).isInstanceOf[AbstractDataType] => {
          val toReplace = getStmt(u.sort) match {
            case DataType(_, ctRef::Nil) if getStmt(ctRef).asInstanceOf[Constructor].selectors.length > 0 => {
              val ct = getStmt(ctRef).asInstanceOf[Constructor]
              ct.selectors.map(s => {
                val sel = getStmt(s).asInstanceOf[Selector]
                val fresh = memoAddInstruction(Application(memoAddInstruction(UserFunction(Util.freshSymbolName(), sel.sort)), List.empty))
                // now replace all "select s from u" terms with "fresh"
                val selSfromU = memoAddInstruction(Application(s, List(memoAddInstruction(Application(p, List.empty)))))
                (selSfromU, fresh)
              })
            }
            case Module(_, ctRef, _, _, _) => {
              val ct = getStmt(ctRef).asInstanceOf[Constructor]
              ct.selectors.map(s => {
                val sel = getStmt(s).asInstanceOf[Selector]
                val fresh = memoAddInstruction(Application(memoAddInstruction(UserFunction(Util.freshSymbolName(), sel.sort)), List.empty))
                // now replace all "select s from u" terms with "fresh"
                val selSfromU = memoAddInstruction(Application(s, List(memoAddInstruction(Application(p, List.empty)))))
                (selSfromU, fresh)
              })
            }
            case _ => List.empty
          }
          locations.foreach(l => if (getStmt(l).isInstanceOf[Ref] || getStmt(l).isInstanceOf[Application]) {
              copyUpdateTerm(l, toReplace.toMap)
            })
        }
        case _ => 
      }
    })
  }

  def inlineMacros(locations: List[Int], bound: Int): Boolean = {
    var changed = false
    var count = bound
    locations.foreach { p =>
      getStmt(p) match {
        case Application(umacroPos, args)
            if getStmt(umacroPos).isInstanceOf[UserMacro] =>
          val umacro = getStmt(umacroPos).asInstanceOf[UserMacro]
          val bindings = umacro.params.map(p => {
            memoAddInstruction(Application(p, List.empty))
          }).zip(args).filter((a, b) => a != b).toMap
          if (!bindings.isEmpty) {
            val inlined = copyUpdateTerm(umacro.body, bindings)
            memoUpdateInstruction(p, Ref(inlined))
            changed = true
            count = count - 1
            if (count == 0) {
              return changed
            }
          }
        case _ =>
      }
    }
    changed
  }

  def eliminateDestructConstruct(locations: List[Int], bound: Int): Boolean = {
    var changed = false
    var count = bound
    locations.foreach { p =>
      getStmt(p) match {
        case Application(selRef, app :: Nil)
            if getStmt(selRef).isInstanceOf[Selector] =>
          getStmt(app) match {
            case Application(ctrRef, args)
                if getStmt(ctrRef).isInstanceOf[Constructor] =>
              val ctr = getStmt(ctrRef).asInstanceOf[Constructor]
              val index = ctr.selectors.indexOf(selRef)
              if (index >= 0) {
                memoUpdateInstruction(p, Ref(args(index)))
                changed = true
                count = count - 1
                if (count == 0) {
                  return changed
                }
              }
            case r: Ref =>
              var appRef = r.loc
              while (getStmt(appRef).isInstanceOf[Ref])
                appRef = getStmt(appRef).asInstanceOf[Ref].loc
              getStmt(appRef) match {
                case Application(ctrRef, args)
                    if getStmt(ctrRef).isInstanceOf[Constructor] =>
                  val ctr = getStmt(ctrRef).asInstanceOf[Constructor]
                  val index = ctr.selectors.indexOf(selRef)
                  if (index >= 0) {
                    memoUpdateInstruction(p, Ref(args(index)))
                    changed = true
                    count = count - 1
                    if (count == 0) {
                      return changed
                    }
                  }
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
    }
    changed
  }

  def plusMinusZero(): Unit = {
    (0 to getStmts().length - 1).foreach { p =>
      getStmt(p) match {
        case Application(plus, args)
            if getStmt(plus) == TheoryMacro("+", List.empty) =>
          val newArgs: List[Int] = args.foldLeft(List.empty) { (acc, a) =>
            getStmt(a) match {
              case TheoryMacro("0", _) => acc
              case _                   => acc ++ List(a)
            }
          }
          if (newArgs.length == 0) {
            memoUpdateInstruction(p, Ref(memoAddInstruction(TheoryMacro("0", List.empty))))
          } else if (newArgs.length < args.length) {
            memoUpdateInstruction(p, Ref(memoAddInstruction(Application(plus, newArgs))))
          }
        case Application(minus, x :: y :: Nil)
            if getStmt(minus) == TheoryMacro("-", List.empty) =>
          if (getStmt(x) == TheoryMacro("0", List.empty)) {
            memoUpdateInstruction(p, Ref(memoAddInstruction(Application(minus, y :: Nil))))
          } else if (getStmt(y) == TheoryMacro("0", List.empty)) {
            memoUpdateInstruction(p, Ref(x))
          }
        case _ =>
      }
    }
  }

  def indexOfGTZGadgets(): Unit = {
    (0 to getStmts().length - 1).foreach { p =>
      getStmt(p) match {
        // index of c in x >= 0 means x contains c
        case Application(gte, t :: zero :: Nil)
            if getStmt(zero) == TheoryMacro("0", List.empty) =>
          getStmt(gte) match {
            case TheoryMacro(">=", _) =>
              getStmt(t) match {
                case Application(indexof, x :: needle :: offset :: Nil)
                    if offset == zero =>
                  getStmt(indexof) match {
                    case TheoryMacro("str.indexof", _) =>
                      val contains = memoAddInstruction(
                        TheoryMacro("str.contains", List.empty)
                      )
                      memoUpdateInstruction(
                        p,
                        Ref(memoAddInstruction(Application(contains, x :: needle :: Nil)))
                      )
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
          }
        // index of c in x < 0 means x does not contain c
        case Application(lt, t :: zero :: Nil)
            if getStmt(zero) == TheoryMacro("0", List.empty) =>
          getStmt(lt) match {
            case TheoryMacro("<", _) =>
              getStmt(t) match {
                case Application(indexof, x :: needle :: offset :: Nil)
                    if offset == zero =>
                  getStmt(indexof) match {
                    case TheoryMacro("str.indexof", _) =>
                      val contains = memoAddInstruction(
                        TheoryMacro("str.contains", List.empty)
                      )
                      val not =
                        memoAddInstruction(TheoryMacro("not", List.empty))
                      val containsApp = memoAddInstruction(
                        Application(contains, x :: needle :: Nil)
                      )
                      memoUpdateInstruction(
                        p,
                        Ref(memoAddInstruction(Application(not, List(containsApp))))
                      )
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
          }
        // index of c in x = -1 means x does not contain c
        case Application(eq, t :: minusOne :: Nil)
            if getStmt(eq) == TheoryMacro("=") =>
          getStmt(minusOne) match {
            case Application(minus, one :: Nil)
                if getStmt(minus) == TheoryMacro("-") && getStmt(
                  one
                ) == TheoryMacro("1") =>
              getStmt(t) match {
                case Application(indexof, x :: needle :: zero :: Nil)
                    if getStmt(zero) == TheoryMacro("0", List.empty) =>
                  getStmt(indexof) match {
                    case TheoryMacro("str.indexof", _) =>
                      val contains = memoAddInstruction(
                        TheoryMacro("str.contains", List.empty)
                      )
                      val not =
                        memoAddInstruction(TheoryMacro("not", List.empty))
                      val containsApp = memoAddInstruction(
                        Application(contains, x :: needle :: Nil)
                      )
                      memoUpdateInstruction(
                        p,
                        Ref(memoAddInstruction(Application(not, List(containsApp))))
                      )
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
    }
  }

  def assertionOverConjunction(assertion: Assert): List[Command] = {
    var changeHappened = true
    var newCommands: List[Command] = List(assertion)
    while (changeHappened) {
      changeHappened = false
      newCommands = newCommands.foldLeft(List.empty)((acc1, c) =>
        c match {
          case ass: Assert =>
            getStmt(ass.t) match {
              case Application(op, operands) =>
                getStmt(op) match {
                  case TheoryMacro("and", _) =>
                    changeHappened = true
                    acc1 ++ operands.foldLeft(List.empty)((acc2, o) =>
                      Assert(o) :: acc2
                    )
                  case _ => acc1 ++ List(c)
                }
              case _ => acc1 ++ List(c)
            }
          case _ => acc1 ++ List(c)
        }
      )
    }
    newCommands
  }

  def containsOverConcat(): Unit = {
    (0 to getStmts().length - 1).foreach { p =>
      getStmt(p) match {
        case Application(contains, haystack :: needle :: Nil)
            if getStmt(needle).isInstanceOf[TheoryMacro] && getStmt(needle)
              .asInstanceOf[TheoryMacro]
              .name
              .length == 3 =>
          getStmt(contains) match {
            case TheoryMacro("str.contains", _) =>
              getStmt(haystack) match {
                case Application(concat, strings) =>
                  getStmt(concat) match {
                    case TheoryMacro("str.++", _) =>
                      // we have a contains over a concat. Make it a disjunction
                      val orRef = memoAddInstruction(TheoryMacro("or"))
                      val components = strings.map { s =>
                        memoAddInstruction(
                          Application(contains, List(s, needle))
                        )
                      }
                      memoUpdateInstruction(p, Ref(memoAddInstruction(Application(orRef, components))))
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
          }
        case _ =>
      }
    }
  }

  /** "(replace c1 with c2 in x) contains c3" as "x contains c3" if c1 = c3 and c2 = c3; as "false" if c1 = c3 and c2 != c3; as "x contains c3" if c1 != c3 and c2 != c3; and "x contains c3 or x contains c1" if c1 != c3 and c2 == c3.
    */
  def containsOverReplace(): Unit = {
    (0 to getStmts().length - 1).foreach { p =>
      getStmt(p) match {
        case Application(contains, haystack :: needle :: Nil)
            if getStmt(needle).isInstanceOf[TheoryMacro] && getStmt(needle)
              .asInstanceOf[TheoryMacro]
              .name
              .length == 3 =>
          getStmt(contains) match {
            case TheoryMacro("str.contains", _) =>
              getStmt(haystack) match {
                case Application(replace, x :: oldChar :: newChar :: Nil)
                    if getStmt(oldChar).isInstanceOf[TheoryMacro] && getStmt(
                      oldChar
                    ).asInstanceOf[TheoryMacro].name.length == 3 && getStmt(
                      newChar
                    ).isInstanceOf[TheoryMacro] && getStmt(newChar)
                      .asInstanceOf[TheoryMacro]
                      .name
                      .length == 3 =>
                  getStmt(replace) match {
                    case TheoryMacro("str.replace", _) =>
                      if (oldChar == needle && newChar != needle) {
                        memoUpdateInstruction(
                          p,
                          Ref(memoAddInstruction(TheoryMacro("false", List.empty)))
                        )
                      } else if (oldChar == needle && newChar == needle) {
                        memoUpdateInstruction(
                          p,
                          Ref(memoAddInstruction(Application(contains, List(x, needle))))
                        )
                      } else if (oldChar != needle && newChar == needle) {
                        val orRef = memoAddInstruction(TheoryMacro("or"))
                        memoUpdateInstruction(
                          p,
                          Ref(memoAddInstruction(
                            Application(
                              orRef,
                              List(
                                memoAddInstruction(
                                  Application(contains, List(x, needle))
                                ),
                                memoAddInstruction(
                                  Application(contains, List(x, oldChar))
                                )
                              )
                            )
                          ))
                        )
                      } else {
                        // they are both not equal so just ingore them altogether
                        memoUpdateInstruction(
                          p,
                          Ref(memoAddInstruction(Application(contains, List(x, needle))))
                        )
                      }
                    case _ =>
                  }
                case _ =>
              }
            case _ =>
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
      (0 to getStmts().length - 1).foreach { p =>
        changeHappened = blastEnumQuantifier(p) || changeHappened
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
  private def blastEnumQuantifier(app: Int): Boolean = {
    getStmt(app) match {
      case Application(quant, body :: Nil)
          if getStmt(quant).isInstanceOf[TheoryMacro] =>
        getStmt(quant).asInstanceOf[TheoryMacro] match {
          case TheoryMacro("forall", v :: vs) =>
            val copies = blastEnumQuantifier(body, v).getOrElse(
              return false
            )
            val andRef = memoAddInstruction(TheoryMacro("and"))
            val new_body = memoAddInstruction(Application(andRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, Ref(new_body))
              return true
            } else {
              val newQuant = memoAddInstruction(TheoryMacro("forall", vs))
              memoUpdateInstruction(body, Ref(new_body))
              memoUpdateInstruction(app, Ref(memoAddInstruction(Application(newQuant, body :: Nil))))
              return true
            }
          case TheoryMacro("exists", v :: vs) =>
            val copies = blastEnumQuantifier(body, v).getOrElse(
              return false
            )
            val orRef = memoAddInstruction(TheoryMacro("or"))
            val new_body = memoAddInstruction(Application(orRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, Ref(new_body))
              return true
            } else {
              val newQuant = memoAddInstruction(TheoryMacro("exists", vs))
              memoUpdateInstruction(body, Ref(new_body))
              memoUpdateInstruction(app, Ref(memoAddInstruction(Application(newQuant, body :: Nil))))
              return true
            }
          case _ => return false
        }
      case _ => return false
    }
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
    val param = getStmt(v).asInstanceOf[FunctionParameter]
    val variants = getStmt(param.sort) match {
      case DataType(_, constructors) =>
        constructors.map { p =>
          val ctr = getStmt(p).asInstanceOf[Constructor]
          if (ctr.selectors.length > 0) {
            // not an enum
            return None
          }
          p
        }
      case _ => return None
    }
    // make n copies of the body
    val copies = variants.map { x =>
      // v is the bound variable we want to replace
      // x is the enum variant we want to plug in for v
      val replaceMap = HashMap((v, x))
      copyUpdateTerm(body, replaceMap.toMap)
    }
    Some(copies)
  }.ensuring(out => {
    out match {
      case Some(copies) => {
        //  get everything reachable from the updated position
        val marks = terms(copies)
        // the bound variable should not be reachable
        !marks(v) 
      }
      case None => true
    }
  })

  /** Updates the references in an application using the map
    *
    * @param pos starting location
    * @param map the changes we want to make: whenever we see x we will replace it with map(x)
    */
  protected def copyUpdateTerm(pos: Int, map: Map[Int, Int]): Int = {
    require(map.forall((a, b) => getStmt(a).isInstanceOf[Application] == getStmt(b).isInstanceOf[Application]))
    // try to rewrite the current position
    map.get(pos) match {
      case Some(value) => value
      case None => {
        getStmt(pos) match {
          case Application(caller, args) =>
            // pos points to an application, update the children
            val newArgs = args.map(a => copyUpdateTerm(a, map))
            val newCaller = copyUpdateTerm(caller, map)
            memoAddInstruction(Application(newCaller, newArgs))
          case Ref(loc) =>
            val newLoc = copyUpdateTerm(loc, map)
            memoAddInstruction(Ref(newLoc))
          case _ => pos
        }
      }
    }
  }
}
