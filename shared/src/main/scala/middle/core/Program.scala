package middle.core

import scala.collection.mutable.ArrayBuffer

import middle.core.rewrite.{incrementInstructionRefs}

// Essentially the whole AST. Head points to "first" element
class Program(val stmts: ArrayBuffer[Instruction], var head: Int) {
  override def toString(): String = stmts.mkString("\n")

  def prependAll(all: ArrayBuffer[Instruction]) = {
    val offset = all.length
    (0 until stmts.length).foreach { i =>
      stmts.update(i, incrementInstructionRefs(stmts(i), offset) match {
        case Some(instruction) => instruction
        case None              => stmts(i)
      })
    }
    head += offset
    stmts.prependAll(all)
  }

  def prependOne(one: Instruction) = {
    (0 until stmts.length).foreach { i =>
      stmts.update(i, incrementInstructionRefs(stmts(i), 1) match {
        case Some(instruction) => instruction
        case None              => stmts(i)
      })
    }
    head += 1
    stmts.prepend(one)
  }

  def appendIncAll(all: ArrayBuffer[Instruction]) = {
    val offset = stmts.length
    (0 until all.length).foreach { i =>
      all.update(i, incrementInstructionRefs(all(i), offset) match {
        case Some(instruction) => instruction
        case None              => all(i)
      })
    }
    stmts.appendAll(all)
  }

  def appendIncOne(one: Instruction) = {
    val offset = stmts.length
    stmts.append(incrementInstructionRefs(one, offset) match {
      case Some(instruction) => instruction
      case None              => one
    })
  }

  def apply(inArgs: Program*) = {
    var offset = stmts.length + 1
    val args = inArgs.map { t =>
      val r = Ref(offset)
      offset += t.stmts.length
      r
    }
    val call = Application(Ref(1), args.toList)

    // add initial application (call) before existing stmts and point head to it
    prependOne(call)
    head = 0

    // add all the argument terms with the correct address bumps
    inArgs.foreach(t => appendIncAll(t.stmts))
  }
}
