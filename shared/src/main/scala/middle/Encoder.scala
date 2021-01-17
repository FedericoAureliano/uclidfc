package middle

import front._
import scala.collection.mutable.ArrayBuffer

object Encoder {

  def run(
    model: List[OuterDecl],
    main: Option[String]
  ): Interfaceable = {
    val program = new Interfaceable(ArrayBuffer[Instruction]())
    // 1. Add every module to the
    // 2. When we find the main module, execute it
    model.foreach { m =>
      m match {
        case td: TypeDecl      => program.typeDeclToTerm(td)
        case dd: DefineDecl    => program.defineDeclToTerm(dd)
        case fd: FunctionDecl  => program.functionDeclToTerm(fd)
        case sy: SynthesisDecl => program.synthesisDeclToTerm(sy)
        case ax: OuterAxiom    => program.axiomToAssertion(ax)
        case mod: ModuleDecl => {
          program.moduleToTerm(mod, Some(mod.id.name) == main)
        }
      }
    }
    program
  }
}
