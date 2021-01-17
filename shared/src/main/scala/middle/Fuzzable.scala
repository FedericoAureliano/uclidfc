package middle

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class Fuzzable(stmts: ArrayBuffer[Instruction]) extends Rewritable(stmts) {
  val random = new Random

  def fuzz(position: Ref): Ref =
    stmts(position.loc) match {
      case d: DataType      => fuzzDatatype(d)
      case t: TheorySort    => fuzzTheorysort(t)
      case u: UserSort      => fuzzUsersort(u)
      case m: middle.Module => fuzzModule(m)
      case o =>
        throw new FuzzingError(
          s"Cannot generate a random instance for the non-sort ${o}!"
        )
    }

  def fuzzModule(mod: Module): Ref = {
    // apply constructor to random instances of all its selectors
    val components = stmts(mod.ct.loc).asInstanceOf[Constructor].selectors.map {
      s =>
        val sel = stmts(s.loc).asInstanceOf[Selector]
        fuzz(sel.sort)
    }
    val body = memoAddInstruction(Application(mod.ct, components))
    memoAddInstruction(
      UserMacro(freshSymbolName(), memo(mod), body, List.empty)
    )
  }

  def fuzzDatatype(d: DataType): Ref = {
    // pick a random constructor
    val ctr = d.constructors(random.nextInt(d.constructors.length))
    // apply constructor to random instances of all its selectors
    val components = stmts(ctr.loc).asInstanceOf[Constructor].selectors.map {
      s =>
        val sel = stmts(s.loc).asInstanceOf[Selector]
        fuzz(sel.sort)
    }
    val body = memoAddInstruction(Application(ctr, components))
    memoAddInstruction(UserMacro(freshSymbolName(), memo(d), body, List.empty))
  }

  def fuzzTheorysort(t: TheorySort): Ref =
    t match {
      case TheorySort("Bool", _) =>
        memoAddInstruction(TheoryMacro(random.nextBoolean().toString()))
      case TheorySort("Int", _) =>
        memoAddInstruction(TheoryMacro(random.nextInt().toString()))
      case TheorySort("Array", params) => {
        val out = fuzz(params.last)
        val asConstAppRef = {
          val asConstRef = memoAddInstruction(TheoryMacro("as const"))
          memoAddInstruction(Application(asConstRef, List(memo(t))))
        }
        memoAddInstruction(
          Application(asConstAppRef, List(out))
        )
      }
      case _ =>
        throw new NotSupportedYet(
          s"Theory sort ${t} is not yet supported for fuzzing!"
        )
    }

  def fuzzUsersort(u: UserSort): Ref =
    memoAddInstruction(UserFunction(freshSymbolName(), memo(u)))
}
