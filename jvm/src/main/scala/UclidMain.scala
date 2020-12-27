package uclid

import scala.collection.immutable._
import front._

import middle.Encoder
import middle.SemanticError

import back.Solver
import back.ProofResult

object Solvers extends Enumeration {
  type Solvers = Value
  val z3, cvc4 = Value
}

/** This is the main class for Uclid.
  *
  */
object UclidMain {

  implicit val weekDaysRead: scopt.Read[Solvers.Value] =
    scopt.Read.reads(Solvers.withName(_))

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
    solver: Solvers.Value = Solvers.z3,
    printOnly: Boolean = false,
    files: Seq[java.io.File] = Seq()
  )

  def parseOptions(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("uclid") {
      head("uclid", "1.0")

      opt[String]('m', "main")
        .valueName("<Module>")
        .action((x, c) => c.copy(mainModuleName = x))
        .text("Name of the main module.")

      opt[Solvers.Value]('s', "solver")
        .action((x, c) => c.copy(solver = x))
        .text(s"Solver to use (${Solvers.values.mkString(", ")})")

      opt[Unit]('p', "print")
        .action((_, c) => c.copy(printOnly = true))
        .text("Print the query without solving.")

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
      val modules = compile(config.files)
      val obs = Encoder.run(modules, Some(config.mainModuleName))
      val solver = config.solver match {
        case Solvers.z3 => new Solver("z3", List())
        case Solvers.cvc4 =>
          new Solver("cvc4", List(("incremental", "true")))
      }
      solver.solve(obs, config.printOnly)
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = List(e.toString())
        errorResult
      case e: SyntaxError =>
        errorResult.messages = List(e.toString())
        errorResult
      case e: SemanticError =>
        errorResult.messages = List(e.toString())
        errorResult
    }
  }

  /** Parse modules, typecheck them, inline procedures, create LTL monitors, etc. */
  def compile(
    srcFiles: Seq[java.io.File]
  ): List[TopLevelDecl] = {
    // Helper function to parse a single file.
    def parseFile(srcFile: String): List[TopLevelDecl] = {
      val text = scala.io.Source.fromFile(srcFile).mkString
      UclidParser.parseModel(srcFile, text)
    }

    val parsedModules = srcFiles.foldLeft(List.empty[TopLevelDecl]) {
      (acc, srcFile) => acc ++ parseFile(srcFile.getPath())
    }

    parsedModules
  }
}
