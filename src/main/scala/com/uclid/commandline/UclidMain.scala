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
    solver: Solvers.Value = Solvers.z3,
    run: Boolean = true,
    features: Boolean = false,
    blastEnumQuantifierFlag: Boolean = false,
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

      opt[String]('o', "out")
        .valueName("<file>")
        .action((x, c) => c.copy(outFile = Some(x)))
        .text("Write query to <file>.")

      opt[Unit]("skip-solver")
        .action((_, c) => c.copy(run = false))
        .text("Don't run the solver.")

      opt[Unit]("print-features")
        .action((_, c) => c.copy(features = true))
        .text("Print query features.")

      opt[Unit]("blast-enum-quantifiers")
        .action((_, c) => c.copy(blastEnumQuantifierFlag = true))
        .text(
          "Rewrite quantifiers over enums to finite disjunctions/conjunctions."
        )

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
  def main(config: Config): UclidResult = {
    val errorResult =
      new ProofResult()

    if (config.files.forall(f => f.getName.endsWith(".ucl"))) {
      print("\nParsing input ... ")
      val startParse = System.nanoTime
      val modules = UclidCompiler.parse(config.files) match {
        case Right(m) => m
        case Left(e) =>
          errorResult.messages = "\n" + e.toString()
          return UclidResult(errorResult)
      }
      val parseDuration = (System.nanoTime - startParse) / 1e9d
      println(s"Parsing completed in ${parseDuration} seconds.")
  
      try {
        print("Processing model ... ")
        val startProcess = System.nanoTime
        val ctx = UclidCompiler.process(modules, Some(config.mainModuleName))
        ctx.termgraph.rewrite(config.blastEnumQuantifierFlag)
        val processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")
        if (config.features) {
          println(ctx.termgraph.featuresList().map(f => "-- " + f).mkString("\n"))
        }
  
        val solver = config.solver match {
          case Solvers.alt_ergo => new AltErgo(ctx)
          case Solvers.cvc4     => new CVC4(ctx)
          case Solvers.vampire  => new Vampire(ctx)
          case Solvers.z3       => new Z3(ctx)
        }
  
        val res = {
          val tmp = solver.solve(config.run, config.outFile)
          if (ctx.traceQuery) {
            (ProofResult(None, tmp._1.messages), tmp._2, tmp._3)
          } else {
            (tmp._1, tmp._2, tmp._3)
          }
        }
  
        UclidResult(res._1, parseDuration, processDuration, res._2, res._3)
  
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
      }
    } else if (config.files.forall(f => f.getName.endsWith(".smt2"))) {
      try {
        print("\nParsing input ... ")
        val startParse = System.nanoTime
        val ctx = SmtCompiler.compile(config.files.foldLeft("")((acc, f) => acc ++ scala.io.Source.fromFile(f)))
        val parseDuration = (System.nanoTime - startParse) / 1e9d
        println(s"Parsing completed in ${parseDuration} seconds.")

        print("Processing model ... ")
        val startProcess = System.nanoTime
        ctx.termgraph.rewrite(config.blastEnumQuantifierFlag)
        val processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")
        if (config.features) {
          println(ctx.termgraph.featuresList().map(f => "-- " + f).mkString("\n"))
        }
  
        val solver = config.solver match {
          case Solvers.alt_ergo => new AltErgo(ctx)
          case Solvers.cvc4     => new CVC4(ctx)
          case Solvers.vampire  => new Vampire(ctx)
          case Solvers.z3       => new Z3(ctx)
        }
  
        val res = solver.solve(config.run, config.outFile)
        UclidResult(res._1, parseDuration, processDuration, res._2, res._3)
  
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
    } else {
      errorResult.messages = "\nAll files must be Uclid5 queries (.ucl) or SMT2 queries (.smt2)!"
      UclidResult(errorResult)
    }
  }
}

case class UclidResult(
  presult: ProofResult,
  parseTime: Double = 0,
  processTime: Double = 0,
  generationTime: Double = 0,
  solveTime: Double = 0
)
