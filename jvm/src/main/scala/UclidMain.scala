package uclid

import scala.collection.immutable._
import front._

import middle.Encoder

import middle.EncodingError
import back._

object Solvers extends Enumeration {
  type Solvers = Value
  val alt_ergo, cvc4, vampire, z3 = Value
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
        if (config.run) {
          println(pResults)
        } else {
          println(pResults.messages)
        }
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
    solver: Solvers.Value = Solvers.cvc4,
    run: Boolean = true,
    outFile: Option[String] = None,
    files: Seq[java.io.File] = Seq()
  )

  def parseOptions(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("uclidfc") {
      head("uclidfc", "1.0")

      opt[String]('m', "main")
        .valueName("<module>")
        .action((x, c) => c.copy(mainModuleName = x))
        .text("Name of the main module.")

      opt[Solvers.Value]('s', "solver")
        .valueName("<solver>")
        .action((x, c) => c.copy(solver = x))
        .text(
          s"Use one of ${Solvers.values.mkString(" or ")}. Solver must be in your path."
        )

      opt[String]('r', "run")
        .valueName("<boolean>")
        .action((x, c) => c.copy(run = x.toBoolean))
        .text("Run the solver?")

      opt[String]('o', "out")
        .valueName("<file>")
        .action((x, c) => c.copy(outFile = Some(x)))
        .text("Write query to <file>.")

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
        case Solvers.alt_ergo => new AltErgoSolver()
        case Solvers.cvc4     => new CVC4Solver()
        case Solvers.vampire  => new VampireSolver()
        case Solvers.z3       => new Z3Solver()
      }

      solver.solve(obs, config.run, config.outFile)
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = "\n" + e.toString()
        errorResult
      case e: SyntaxError =>
        errorResult.messages = "\n" + e.toString()
        errorResult
      case e: SemanticError =>
        errorResult.messages = "\n" + e.toString()
        errorResult
      case e: EncodingError =>
        errorResult.messages = "\n" + e.toString()
        errorResult
      case e: UclidJvmError =>
        errorResult.messages = "\n" + e.toString()
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
