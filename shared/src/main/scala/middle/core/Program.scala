package middle.core

import scala.collection.mutable.ArrayBuffer

import middle.core.rewrite.{incrementInstructionRefs}

// Essentially the whole AST
class Program(val stmts: Array[Instruction]) {
  override def toString(): String = stmts.mkString("\n")

  def apply(terms: Program*): Program = {
    val newStmts = new ArrayBuffer[Instruction]()

    var offset = stmts.length + 1
    val args = terms.map { t =>
      val r = Ref(offset)
      offset += t.stmts.length
      r
    }
    val call = Application(Ref(1), args.toList)

    // add initial application (call) and then all the existing stmts
    newStmts.addOne(call)
    (0 until stmts.length).foreach { i =>
      incrementInstructionRefs(stmts(i), 1) match {
        case Some(instruction) => newStmts.addOne(instruction)
        case None              => newStmts.addOne(stmts(i))
      }
    }

    // add all the argument terms with the correct address bumps
    offset = stmts.length + 1
    terms.foreach { t =>
      (0 until t.stmts.length).foreach { j =>
        incrementInstructionRefs(t.stmts(j), offset) match {
          case Some(instruction) => newStmts.addOne(instruction)
          case None              => newStmts.addOne(t.stmts(j))
        }
      }
      offset += t.stmts.length
    }

    new Program(newStmts.toArray)
  }
}
