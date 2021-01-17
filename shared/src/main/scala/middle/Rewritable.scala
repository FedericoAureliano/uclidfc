package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

class Rewritable(stmts: ArrayBuffer[Instruction]) extends Minimal(stmts) {

  def rewrite(blastEnumQuantifierFlag: Boolean): Unit =
    if (blastEnumQuantifierFlag) {
      var changeHappened = true
      while (changeHappened) {
        changeHappened = false
        val marks = mark(assertionRefs ++ axiomRefs)
        marks.zipWithIndex.foreach(p =>
          if (p._1) {
            changeHappened =
              blastEnumQuantifier(Ref(p._2, None)) || changeHappened
          }
        )
      }
    }

  /* rewrites
    "forall (x : <some enum>, ...) :: e" to "forall (...) :: e[x/x_1] and ... and e[x/x_n]" where x_i are the elements of <some enum>
    "exists (x : <some enum>, ...) :: e" to "exists (...) :: e[x/x_1] or ... or e[x/x_n]" where x_i are the elements of <some enum>
    if the resulting quantifier is empty, then it is eliminated.
    if the app is not of the correct form then nothing happens.

    TODO: handle case where enum quantifier is not in first position
   */
  def blastEnumQuantifier(app: Ref): Boolean =
    stmts(app.loc) match {
      case Application(quant, body :: Nil)
          if stmts(quant.loc).isInstanceOf[TheoryMacro] => {
        stmts(quant.loc).asInstanceOf[TheoryMacro] match {
          case TheoryMacro("forall", v :: vs) => {
            val copies = blastEnumQuantifierHelper(body, v).getOrElse(
              return false
            )
            val andRef = memoAddInstruction(TheoryMacro("and"))
            val new_body = memoAddInstruction(Application(andRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, stmts(new_body.loc))
              return true
            } else {
              memoUpdateInstruction(quant, TheoryMacro("forall", vs))
              memoUpdateInstruction(body, stmts(new_body.loc))
              return true
            }
          }
          case TheoryMacro("exists", v :: vs) => {
            val copies = blastEnumQuantifierHelper(body, v).getOrElse(
              return false
            )
            val orRef = memoAddInstruction(TheoryMacro("or"))
            val new_body = memoAddInstruction(Application(orRef, copies))
            if (vs.length == 0) {
              memoUpdateInstruction(app, stmts(new_body.loc))
              return true
            } else {
              memoUpdateInstruction(quant, TheoryMacro("exists", vs))
              memoUpdateInstruction(body, stmts(new_body.loc))
              return true
            }
          }
          case _ => return false
        }
      }
      case _ => return false
    }

  def blastEnumQuantifierHelper(body: Ref, v: Ref): Option[List[Ref]] = {
    val param = stmts(v.loc).asInstanceOf[FunctionParameter]
    val variants = stmts(param.sort.loc) match {
      case DataType(_, constructors) =>
        constructors.map { p =>
          val ctr = stmts(p.loc).asInstanceOf[Constructor]
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
      val replaceMap = HashMap((v, x))
      updateTerm(newBody, replaceMap)
      newBody
    }
    Some(copies)
  }
}
