package uclid

import scala.util.parsing.combinator._
import scala.collection.immutable._
import front.{Identifier, ModuleDecl, _}

import scala.collection.mutable.ArrayBuffer

import middle.Encoder
import middle.Program
import middle.Printer

import back.Solver
import back.ProofResult

/** This is the main class for Uclid.
  *
  */
object UclidMain {

  def main(args: Array[String]): Unit =
    parseOptions(args) match {
      case None => sys.exit(2)
      case Some(config) => {
        val pResults = main(config)
        println(pResults)
        sys.exit(pResults.result match {
          case Some(false) => 0
          case Some(true)  => 1
          case None        => 2
        })
      }
    }

  /** Command-line configuration flags for uclid5.
    *
    * @param mainModuleName The name of the main module.
    * @param files List of files that should parsed and analyzed.
    */
  case class Config(
    mainModuleName: String = "main",
    solverPath: String = "z3",
    shouldPrint: Boolean = false,
    files: Seq[java.io.File] = Seq()
  )

  def parseOptions(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("uclid") {
      head("uclid", "1.0")

      opt[String]('m', "main")
        .valueName("<Module>")
        .action((x, c) => c.copy(mainModuleName = x))
        .text("Name of the main module.")

      opt[String]('s', "solver")
        .valueName("<Solver>")
        .action((x, c) => c.copy(solverPath = x))
        .text("Path to solver.")

      opt[Unit]('p', "print")
        .action((_, c) => c.copy(shouldPrint = true))
        .text("Print the query.")

      arg[java.io.File]("<file> ...")
        .unbounded()
        .required()
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("List of files to analyze.")

    }
    parser.parse(args, Config())
  }

  /** This version of 'main' does all the real work.
    */
  def main(config: Config): ProofResult = {
    val errorResult =
      new ProofResult()
    try {
      val mainModuleName = Identifier(config.mainModuleName)
      val modules = compile(config.files, mainModuleName)
      val obs = Encoder.run(modules, Some(config.mainModuleName))
      if (config.shouldPrint) {
        println(
          Printer
            .programToQuery(obs)
        )
      }
      Solver.solve(obs, config.solverPath)
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = List(e.toString())
        errorResult
      case e: SyntaxError =>
        errorResult.messages = List(e.toString())
        errorResult
    }
  }

  /** Parse modules, typecheck them, inline procedures, create LTL monitors, etc. */
  def compile(
    srcFiles: Seq[java.io.File],
    mainModuleName: Identifier,
    test: Boolean = false
  ): List[Decl] = {
    type NameCountMap = Map[Identifier, Int]
    var nameCnt: NameCountMap = Map().withDefaultValue(0)

    // Helper function to parse a single file.
    def parseFile(srcFile: String): List[Decl] = {
      val text = scala.io.Source.fromFile(srcFile).mkString
      UclidParser.parseModel(srcFile, text)
    }

    val parsedModules = srcFiles.foldLeft(List.empty[Decl]) { (acc, srcFile) =>
      acc ++ parseFile(srcFile.getPath())
    }

    parsedModules
  }
}
