package com.uclid.commandline

import com.uclid.termgraph
import com.uclid.context._
import com.uclid.solverinterface.solver._
import com.uclid.solverinterface.compiler._
import com.uclid.uclidinterface.compiler.parser._
import com.uclid.uclidinterface.compiler._

object Solvers extends Enumeration {
  type Solvers = Value
  val alt_ergo, cvc4, vampire, z3 = Value
}

/** This is the main class for Uclid.
  */
object UclidMain {

  implicit val solverRead: scopt.Read[Solvers.Value] =
    scopt.Read.reads(Solvers.withName(_))

  def main(args: Array[String]): Unit =
    parseOptions(args) match {
      case None => sys.exit(2)
      case Some(config) =>
        val answer = main(config)
        if (config.run) {
          println(answer.presult)
        } else {
          println(answer.presult.messages)
        }
        sys.exit(answer.presult.result match {
          case Some(false) => 0
          case Some(true)  => 1
          case None        => 2
        })
    }

  /** Command-line configuration flags for uclid5.
    *
    * @param mainModuleName The name of the main module.
    * @param files List of files that should parsed and analyzed.
    */
  case class Config(
    mainModuleName: String = "main",
    solvers: List[Solvers.Value] = List.empty,
    run: Boolean = true,
    features: Boolean = false,
    blastEnumQuantifierFlag: Boolean = false,
    outFile: Option[String] = None,
    files: Seq[java.io.File] = Seq()
  )

  def parseOptions(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("uclidfc") {
      head("uclidfc", "1.0")
      
      help("help").text("prints this usage text")

      note(sys.props("line.separator") + "Basic Usage")

      opt[String]('m', "main")
        .valueName("<module>")
        .action((x, c) => c.copy(mainModuleName = x))
        .text("Name of the main module.")

      opt[Solvers.Value]('s', "solver")
        .valueName("<solver>")
        .action((x, c) => c.copy(solvers = x :: c.solvers))
        .minOccurs(0)
        .maxOccurs(Solvers.values.size)
        .text(
          s"Solver to use (${Solvers.values.mkString(" or ")}). Solver must be in your path."
        )

      opt[String]('o', "out")
        .valueName("<file>")
        .action((x, c) => c.copy(outFile = Some(x)))
        .text("Write query to <file>.")

      opt[Unit]("skip-solver")
        .action((_, c) => c.copy(run = false))
        .text("Don't run the solver.")

      arg[java.io.File]("<file> ...")
        .unbounded()
        .required()
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("List of files to analyze.")

      note(sys.props("line.separator") + "Analysis")

      opt[Unit]("print-features")
        .action((_, c) => c.copy(features = true))
        .text("Print query features.")

      note(sys.props("line.separator") + "Rewrites")

      opt[Unit]("blast-enum-quantifiers")
        .action((_, c) => c.copy(blastEnumQuantifierFlag = true))
        .text(
          "Rewrite quantifiers over enums to finite disjunctions/conjunctions."
        )

    }
    parser.parse(args, Config())
  }

  /** This version of 'main' does all the real work.
    */
  def main(config: Config): UclidResult = {
    val errorResult =
      new ProofResult()

    val inputLanguage = if (config.files.forall(f => f.getName.endsWith(".ucl"))) {
      "UCLID"
    } else if (config.files.forall(f => f.getName.endsWith(".smt2"))) {
      "SMT"
    } else {
      errorResult.messages = "\nAll files must be Uclid5 queries (.ucl) or SMT2 queries (.smt2)!"
      return UclidResult(errorResult)
    }

    var parseDuration = 0.0
    var processDuration = 0.0
    var analysisDuration = 0.0

    try {
      val ctx = if (inputLanguage == "UCLID") {
        print("\nParsing input ... ")
        val startParse = System.nanoTime
        val modules = UclidCompiler.parse(config.files) match {
          case Right(m) => m
          case Left(e) =>
            errorResult.messages = "\n" + e.toString()
            return UclidResult(errorResult)
        }
        parseDuration = (System.nanoTime - startParse) / 1e9d
        println(s"Parsing completed in ${parseDuration} seconds.")

        print("Processing model ... ")
        val startProcess = System.nanoTime
        val ctx = UclidCompiler.process(modules, Some(config.mainModuleName))
        ctx.termgraph.rewrite(config.blastEnumQuantifierFlag)
        processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")

        ctx
      } else {
        // Must be SMT Language
        print("\nParsing input ... ")
        val startParse = System.nanoTime
        val ctx = SmtCompiler.compile(config.files.foldLeft("")((acc, f) => acc ++ scala.io.Source.fromFile(f)))
        parseDuration = (System.nanoTime - startParse) / 1e9d
        println(s"Parsing completed in ${parseDuration} seconds.")

        print("Processing model ... ")
        val startProcess = System.nanoTime
        ctx.termgraph.rewrite(config.blastEnumQuantifierFlag)
        processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")

        ctx
      }

      print("Analyzing model ... ")
      val startAnalysis = System.nanoTime
      val features = if (config.features) {
        ctx.termgraph.featuresList(ctx.entryPoints())
      } else {
        List.empty
      }
      analysisDuration = (System.nanoTime - startAnalysis) / 1e9d
      println(s"Analysis completed in ${analysisDuration} seconds.")
      if (config.features) {
        println(features.map(f => "-- " + f).mkString("\n"))
      }

      val solvers = config.solvers.map(solver => solver match {
        case Solvers.alt_ergo => new AltErgo(ctx)
        case Solvers.cvc4     => new CVC4(ctx)
        case Solvers.vampire  => new Vampire(ctx)
        case Solvers.z3       => new Z3(ctx)
      })

      val solver = if (solvers.length == 0) {
        Z3(ctx)
      } else if (solvers.length == 1) {
        solvers.head
      } else {
        Medley(ctx, solvers)
      }

      val res = solver.solve(config.run, config.outFile)
      if (ctx.ignoreResult()) {
        UclidResult(ProofResult(None, res._1.messages), parseDuration, processDuration, analysisDuration, res._2, res._3)
      } else {
        UclidResult(res._1, parseDuration, processDuration, analysisDuration, res._2, res._3)
      }

    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = "\n" + e.toString()
        UclidResult(errorResult)
      case e: SemanticError =>
        errorResult.messages = "\n" + e.toString()
        UclidResult(errorResult)
      case e: SolverMismatchError =>
        errorResult.messages = "\n" + e.toString()
        UclidResult(errorResult)
      case e: SmtParserError =>
        errorResult.messages = "\n" + e.toString()
        UclidResult(errorResult)
    }
  }
}

case class UclidResult(
  presult: ProofResult,
  parseTime: Double = 0,
  processTime: Double = 0,
  analysisTime: Double = 0,
  generationTime: Double = 0,
  solveTime: Double = 0
)
