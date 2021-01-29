// package com.uclid.uclidlanguage.compiler

// import com.uclid.uclidlanguage.parser._
// import com.uclid.termgraph

// import scala.collection.mutable.ArrayBuffer

// object Encoder {

//   def run(
//     model: List[OuterDecl],
//     main: Option[String]
//   ): Unit = {
//     // 1. Add every module to the
//     // 2. When we find the main module, execute it
//     model.foreach { m =>
//       m match {
//         case td: TypeDecl      => termgraph.typeDeclToTerm(td)
//         case dd: DefineDecl    => termgraph.defineDeclToTerm(dd)
//         case fd: FunctionDecl  => termgraph.functionDeclToTerm(fd)
//         case sy: SynthesisDecl => termgraph.synthesisDeclToTerm(sy)
//         case ax: OuterAxiom    => termgraph.axiomToAssertion(ax)
//         case mod: ModuleDecl => {
//           termgraph.moduleToTerm(mod, Some(mod.id.name) == main)
//         }
//       }
//     }
//     termgraph
//   }
// }

package com.uclid.uclidlanguage.compiler

import com.uclid.uclidlanguage.parser._
import com.uclid.termgraph
import com.uclid.uclidlanguage.lexer.UclidLexer
import com.uclid.uclidlanguage.parser.{UclidParser, Model}

object UclidCompiler {
  def parse(srcFiles: Seq[java.io.File]): Either[UclidCompilationError, Model] = {
    val code = srcFiles.map(f => scala.io.Source.fromFile(f)).mkString
    val model = for {
      tokens <- UclidLexer(code).right
      ast <- UclidParser(tokens).right
    } yield ast
    model 
  }

  def process(model: Model, main: Option[String]) : Unit = {
    model.outers.foreach { m =>
      m match {
        case td: TypeDecl      => termgraph.typeDeclToTerm(td)
        case dd: DefineDecl    => termgraph.defineDeclToTerm(dd)
        case fd: FunctionDecl  => termgraph.functionDeclToTerm(fd)
        case sy: SynthesisDecl => termgraph.synthesisDeclToTerm(sy)
        case ax: OuterAxiom    => termgraph.axiomToAssertion(ax)
        case mod: ModuleDecl => {
          termgraph.moduleToTerm(mod, Some(mod.id.name) == main)
        }
      }
    }
  }
}
