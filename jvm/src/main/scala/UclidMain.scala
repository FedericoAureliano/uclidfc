package uclid

import scala.util.parsing.combinator._
import scala.collection.immutable._
import front.{Identifier, Module, _}
import front.Utils.ParserErrorList

import scala.collection.mutable.ArrayBuffer

import middle.Interpreter
import middle.ProofTask
import middle.Program

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
        var ctx: Option[Boolean] = Some(false)
        pResults.foreach { p =>
          println(p)
          ctx = p.result
        }

        sys.exit(ctx match {
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
    solverPath: String = "cvc4",
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
        .text("Path to SMT solver.")

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
  def main(config: Config): List[ProofResult] = {
    val errorResult =
      new ProofResult(new Program(new ArrayBuffer(), 0), "Front-end error")
    try {
      val mainModuleName = Identifier(config.mainModuleName)
      val modules = compile(config.files, mainModuleName)
      val obligations = Interpreter.run(modules, Some(config.mainModuleName))
      Solver.solve(obligations, config.solverPath)
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = List("Error: " + e.getMessage() + ".")
        List(errorResult)
      case (p: Utils.ParserError) =>
        errorResult.messages = List(
          "%s error %s: %s.\n%s"
            .format(p.errorName, p.positionStr, p.getMessage, p.fullStr)
        )
        List(errorResult)
    }
  }

  /** Parse modules, typecheck them, inline procedures, create LTL monitors, etc. */
  def compile(
    srcFiles: Seq[java.io.File],
    mainModuleName: Identifier,
    test: Boolean = false
  ): List[Module] = {
    type NameCountMap = Map[Identifier, Int]
    var nameCnt: NameCountMap = Map().withDefaultValue(0)

    // Helper function to parse a single file.
    def parseFile(srcFile: String): List[Module] = {
      val text = scala.io.Source.fromFile(srcFile).mkString
      UclidParser.parseModel(srcFile, text)
    }

    val parsedModules = srcFiles.foldLeft(List.empty[Module]) {
      (acc, srcFile) => acc ++ parseFile(srcFile.getPath())
    }

    parsedModules
  }
}
