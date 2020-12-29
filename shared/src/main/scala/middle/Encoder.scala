package middle

import front._
import scala.collection.mutable.ArrayBuffer

object Encoder {

  def run(
    model: List[TopLevelDecl],
    main: Option[String]
  ): Interfaced = {
    val program = new Interfaced(ArrayBuffer[Instruction]())
    // 1. Add every module to the
    // 2. When we find the main module, execute it
    model.foreach { m =>
      m match {
        case td: TypeDecl     => program.typeDeclToTerm(td)
        case dd: DefineDecl   => program.defineDeclToTerm(dd)
        case fd: FunctionDecl => program.functionDeclToTerm(fd)
        case mod: ModuleDecl => {
          val params = program.moduleToTerm(mod)
          if (Some(mod.id.name) == main) {
            program.executeControl(mod.id, params._1, params._2, mod.cmds)
          }
        }
      }
    }
    program
  }
}
