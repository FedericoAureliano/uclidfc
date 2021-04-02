package com.uclid.termgraph

import com.uclid.context._

import scala.collection.mutable.{ArrayBuffer, HashMap, ListBuffer}

trait Rewritable() extends AbstractTermGraph {

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
    while changeHappened do {
      changeHappened = false
      (0 to getStmts().length - 1).foreach { p =>
        changeHappened = blastEnumQuantifier(p) || changeHappened
        repair()
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
            if vs.length == 0 then {
              memoUpdateInstruction(app, Ref(new_body))
              return true
            } else {
              val newQuant = memoAddInstruction(TheoryMacro("forall", vs))
              memoUpdateInstruction(body, Ref(new_body))
              memoUpdateInstruction(
                app,
                Ref(memoAddInstruction(Application(newQuant, body :: Nil)))
              )
              return true
            }
          case TheoryMacro("exists", v :: vs) =>
            val copies = blastEnumQuantifier(body, v).getOrElse(
              return false
            )
            val orRef = memoAddInstruction(TheoryMacro("or"))
            val new_body = memoAddInstruction(Application(orRef, copies))
            if vs.length == 0 then {
              memoUpdateInstruction(app, Ref(new_body))
              return true
            } else {
              val newQuant = memoAddInstruction(TheoryMacro("exists", vs))
              memoUpdateInstruction(body, Ref(new_body))
              memoUpdateInstruction(
                app,
                Ref(memoAddInstruction(Application(newQuant, body :: Nil)))
              )
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
    val param = getStmt(v).asInstanceOf[FunctionParameter]
    val variants = getStmt(param.sort) match {
      case DataType(_, constructors) =>
        constructors.map { p =>
          val ctr = getStmt(p).asInstanceOf[Constructor]
          if ctr.selectors.length > 0 then {
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
  }

  /** Updates the references in an application using the map
    *
    * Assumes that the term targets of the rewrite map do not contain the keys of the rewrite map.
    *
    * @param pos starting location
    * @param map the changes we want to make: whenever we see x we will replace it with map(x)
    */
  protected def copyUpdateTerm(pos: Int, map: Map[Int, Int]): Int = {
    require(
      map.forall((a, b) =>
        getStmt(a).isInstanceOf[Application] == getStmt(b)
          .isInstanceOf[Application]
      )
    )
    require(map.forall((a, b) => findTarget(a) == a && findTarget(b) == b))
    // try to rewrite the current position
    map.get(findTarget(pos)) match {
      case Some(newLoc) => newLoc
      case None =>
        getStmt(pos) match {
          case Application(function, args) =>
            // pos points to an application, update the children
            val newArgs = args.map(a => copyUpdateTerm(a, map))
            val newCaller = copyUpdateTerm(function, map)
            val newLoc = memoAddInstruction(Application(newCaller, newArgs))
            map.getOrElse(newLoc, newLoc)
          case Ref(loc) =>
            val newLoc = memoAddInstruction(Ref(copyUpdateTerm(loc, map)))
            map.getOrElse(newLoc, newLoc)
          case _ => pos
        }
    }
  }.ensuring { out =>
    val marks = terms(List(out))
    map.forall((key, value) => !marks(key))
  }
}
