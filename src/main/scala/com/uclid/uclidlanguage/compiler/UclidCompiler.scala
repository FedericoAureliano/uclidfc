package com.uclid.uclidlanguage.compiler

import com.uclid.uclidlanguage.parser._
import com.uclid.termgraph.TermGraph
import com.uclid.uclidlanguage.lexer.UclidLexer
import com.uclid.uclidlanguage.parser.{UclidParser, Model}
import com.uclid.solverinterface.Context

object UclidCompiler {
  def parse(srcFiles: Seq[java.io.File]): Either[UclidCompilationError, Model] = {
    val code = srcFiles.foldLeft("")((acc, f) => acc ++ scala.io.Source.fromFile(f))
    val model = for {
      tokens <- UclidLexer(code)
      ast <- UclidParser(tokens)
    } yield ast
    model 
  }

  def process(model: Model, main: Option[String]) : Context = {
    val tg = new TermGraph()
    val ctx = new Context(tg)
    model.outers.foreach { m =>
      m match {
        case td: TypeDecl      => ctx.typeDeclToTerm(td)
        case dd: DefineDecl    => ctx.defineDeclToTerm(dd)
        case fd: FunctionDecl  => ctx.functionDeclToTerm(fd)
        case sy: SynthesisDecl => ctx.synthesisDeclToTerm(sy)
        case ax: OuterAxiom    => ctx.axiomToAssertion(ax)
        case mod: ModuleDecl => {
          ctx.moduleToTerm(mod, Some(mod.id.name) == main)
        }
      }
    }
    ctx
  }
}
