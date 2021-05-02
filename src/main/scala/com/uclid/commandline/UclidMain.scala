package com.uclid.commandline

import com.uclid.context.{Check, Command, _}
import com.uclid.smtcompiler._
import com.uclid.context.solver._
import com.uclid.termgraph
import com.uclid.uclidcompiler._
import com.uclid.uclidcompiler.parser._
import com.uclid.idiolect.WCFG

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.sys.process._

object Solvers extends Enumeration {
  type Solvers = Value
  val alt_ergo, cvc4, vampire, z3 = Value
}

/** This is the main class for Uclid.
  */
object UclidMain {

  implicit val solverRead: scopt.Read[Solvers.Value] =
    scopt.Read.reads(Solvers.withName(_))

  val optimizationLevels = List(0, 1)

  def main(args: Array[String]): Unit =
    parseOptions(args) match {
      case None         => sys.exit(2)
      case Some(config) => main(config)
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
    timeout: Int = Int.MaxValue,
    features: Boolean = false,
    blastEnumQuantifierFlag: Boolean = false,
    outFile: Option[String] = None,
    dataFolder: Option[String] = None,
    train: Boolean = false,
    prettyPrint: Boolean = false,
    debugPrint: Boolean = false,
    singleQuery: Boolean = false,
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

      opt[Int]('t', "timeout")
        .valueName("<timeout>")
        .action((x, c) => c.copy(timeout = x))
        .text(
          s"Timeout (in whole seconds) to give the solver per query."
        )

      opt[String]('w', "write")
        .valueName("<file>")
        .action((x, c) => c.copy(outFile = Some(x)))
        .text("Write query to <file>.")

      opt[Unit]("pretty-print")
        .action((_, c) => c.copy(prettyPrint = true))
        .text("Try to make output queries human readable.")

      opt[Unit]("debug-print")
        .action((_, c) => c.copy(debugPrint = true))
        .text("Add internal term graph information as SMT comments.")

      opt[Unit]("skip-solver")
        .action((_, c) => c.copy(run = false))
        .text("Don't run the solver.")

      opt[Unit]("single-thread")
        .action((_, c) => c.copy(singleQuery = true))
        .text("Don't run solvers in parallel.")

      arg[java.io.File]("<file> ...")
        .unbounded()
        .required()
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("List of input files.")

      note(sys.props("line.separator") + "Analysis")

      opt[Unit]("print-features")
        .action((_, c) => c.copy(features = true))
        .text("Print query features.")

      note(sys.props("line.separator") + "Algebraic Datatype Rewrites")

      opt[Unit]("blast-enum-quantifiers")
        .action((_, c) => c.copy(blastEnumQuantifierFlag = true))
        .text(
          "Rewrite quantifiers over enums to finite disjunctions/conjunctions."
        )

      note(sys.props("line.separator") + "Idiolect")

      opt[Unit]("train")
        .action((_, c) => c.copy(train = true))
        .text("Train an idiolect model per solver. Writes to data folder; requires at least two solvers.")

      opt[String]("data")
        .valueName("<folder>")
        .action((x, c) => c.copy(dataFolder = Some(x)))
        .text("Folder with idiolect models <folder>. Required for automated solver selection.")

      checkConfig(c => {
        if c.solvers.length > 1 && !c.dataFolder.isDefined then 
          failure("Must provide data folder for automated solver selection (you provided more than one solver but no data folder)!")
        else if c.train && !c.dataFolder.isDefined then 
          failure("Must provide data folder for automated solver selection training!")
        else if c.train && c.solvers.length <= 1 then 
          failure("Must specify at least two solvers for automated solver selection training!")
        else 
          success
      })

    }
    parser.parse(args, Config())
  }

  /** This version of 'main' does all the real work.
    */
  def main(config: Config): List[UclidResult] = {
    val errorResult =
      new ProofResult()

    val inputLanguage =
      if config.files.forall(f => f.getName.endsWith(".ucl")) then {
        "UCLID"
      } else if config.files.forall(f => f.getName.endsWith(".smt2")) then {
        "SMT"
      } else {
        errorResult.messages =
          "\nAll files must be Uclid5 queries (.ucl) or SMT2 queries (.smt2)!"
        println("\n" + errorResult)
        return List(UclidResult(errorResult))
      }

    val solvers = config.solvers.map(solver =>
      solver match {
        case Solvers.alt_ergo => new AltErgo()
        case Solvers.cvc4     => new CVC4()
        case Solvers.vampire  => new Vampire()
        case Solvers.z3       => new Z3()
      }
    )

    val solver = if solvers.length == 0 then {
      Z3()
    } else if solvers.length == 1 then {
      solvers.head
    } else {
      val idiolectMap = solvers.map(s => (WCFG.load(config.dataFolder.get + "/" + s.getName() + ".csv"), s))
      Lida(idiolectMap)
    }

    var parseDuration = 0.0
    var processDuration = 0.0
    var analysisDuration = 0.0

    val ret = if inputLanguage == "UCLID" then {
      List(runUclidMode(solver, config))
    } else {
      runSMTMode(solver, config)
    }

    if config.train then {
      val lida = solver.asInstanceOf[Lida]
      lida.save(config.dataFolder.get)
    }

    ret
  }

  def runUclidMode(solver: Solver, config: Config): UclidResult = {
    val errorResult =
      new ProofResult()

    val prettyPrintLevel = if config.debugPrint then {
      2
    } else if config.prettyPrint && !config.debugPrint then {
      1
    } else {
      0
    }

    try {
      print("\nParsing input ... ")
      val startParse = System.nanoTime
      val modules = UclidCompiler.parse(config.files) match {
        case Right(m) => m
        case Left(e) =>
          errorResult.messages = "\n" + e.toString()
          println("\n" + errorResult)
          return UclidResult(errorResult)
      }
      var parseDuration = (System.nanoTime - startParse) / 1e9d
      println(s"Parsing completed in ${parseDuration} seconds.")

      print("Processing model ... ")
      val startProcess = System.nanoTime
      val ctx = UclidCompiler.process(modules, Some(config.mainModuleName))

      if config.singleQuery then {
        ctx.singleQuery = true
      }
      if config.blastEnumQuantifierFlag then {
        ctx.termgraph.blastEnumQuantifier()
      }
      var processDuration = (System.nanoTime - startProcess) / 1e9d
      println(s"Processing completed in ${processDuration} seconds.")

      print("Analyzing model ... ")
      val startAnalysis = System.nanoTime
      val features = if config.features then {
        ctx.termgraph.featuresList(ctx.entryPoints())
      } else {
        List.empty
      }
      var analysisDuration = (System.nanoTime - startAnalysis) / 1e9d
      println(s"Analysis completed in ${analysisDuration} seconds.")
      if config.features then {
        println(features.map(f => "-- " + f).mkString("\n"))
      }

      val res = if config.train then {
        assert(solver.isInstanceOf[Lida], "Can only train Lida!")
        assert(config.run, "Must be set to run!")
        val lida = solver.asInstanceOf[Lida]
        lida.train(
          config.run,
          config.timeout,
          ctx,
          config.outFile,
          prettyPrintLevel
        )
      } else {
        solver.solve(
          config.run,
          config.timeout,
          ctx,
          config.outFile,
          prettyPrintLevel
        )
      }

      val ret = if ctx.ignoreResult() then {
        UclidResult(
          ProofResult(None, res._1.messages),
          parseDuration,
          processDuration,
          analysisDuration,
          res._2,
          res._3
        )
      } else {
        UclidResult(
          res._1,
          parseDuration,
          processDuration,
          analysisDuration,
          res._2,
          res._3
        )
      }

      if config.run then {
        println(ret.presult)
      } else {
        println(ret.presult.messages)
      }
      ret
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = "\n" + e.toString()
        println("\n" + errorResult)
        UclidResult(errorResult)
      case e: SemanticError =>
        errorResult.messages = "\n" + e.msg
        println("\n" + errorResult)
        UclidResult(errorResult)
      case e: SolverMismatchError =>
        errorResult.messages = "\n" + e.msg
        println("\n" + errorResult)
        UclidResult(errorResult)
      case e: SmtParserError =>
        errorResult.messages = "\n" + e.msg
        println("\n" + errorResult)
        UclidResult(errorResult)
      case e: TimeoutException =>
        errorResult.messages = "\n" + e.toString()
        println("\n" + errorResult)
        UclidResult(errorResult)
    }
  }

  def runSMTMode(solver: Solver, config: Config): List[UclidResult] = {

    val prettyPrintLevel = if config.debugPrint then {
      2
    } else if config.prettyPrint && !config.debugPrint then {
      1
    } else {
      0
    }

    // Must be SMT Language
    val results = config.files.map { f =>
      val errorResult = new ProofResult()
      try {
        print(s"\nParsing ${f.getName()} ... ")
        val startParse = System.nanoTime
        val ctx = SmtCompiler.compile(scala.io.Source.fromFile(f).mkString(""))
        var parseDuration = (System.nanoTime - startParse) / 1e9d
        println(s"Parsing completed in ${parseDuration} seconds.")

        print("Processing query ... ")
        var changed = false
        val startProcess = System.nanoTime
        if config.blastEnumQuantifierFlag then {
          changed = true
          ctx.termgraph.blastEnumQuantifier()
        }
        var processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")

        print("Analyzing query ... ")
        val startAnalysis = System.nanoTime
        val features = if config.features then {
          ctx.termgraph.featuresList(ctx.entryPoints())
        } else {
          List.empty
        }
        var analysisDuration = (System.nanoTime - startAnalysis) / 1e9d
        println(s"Analysis completed in ${analysisDuration} seconds.")
        if config.features then {
          println(features.map(f => "-- " + f).mkString("\n"))
        }

        val unmodifiedSMTFile = if changed || config.outFile.isDefined then {
          None
        } else {
          Some(f)
        }

        val res = if config.train then {
          assert(solver.isInstanceOf[Lida], "Can only train Lida!")
          assert(config.run, "Must be set to run!")
          val lida = solver.asInstanceOf[Lida]
          lida.train(
            config.run,
            config.timeout,
            ctx,
            config.outFile,
            prettyPrintLevel,
            unmodifiedSMTFile
          )
        } else {
          solver.solve(
            config.run,
            config.timeout,
            ctx,
            config.outFile,
            prettyPrintLevel,
            unmodifiedSMTFile
          )
        }

        val ret = if ctx.ignoreResult() then {
          UclidResult(
            ProofResult(None, res._1.messages),
            parseDuration,
            processDuration,
            analysisDuration,
            res._2,
            res._3
          )
        } else {
          UclidResult(
            ProofResult(res._1.result, res._1.messages, true),
            parseDuration,
            processDuration,
            analysisDuration,
            res._2,
            res._3
          )
        }

        if config.run then {
          println(ret.presult)
        } else {
          println(ret.presult.messages)
        }

        ret
      } catch {
        case (e: java.io.FileNotFoundException) =>
          errorResult.messages = "\n" + e.toString()
          println("\n" + errorResult)
          UclidResult(errorResult)
        case e: SemanticError =>
          errorResult.messages = "\n" + e.msg
          println("\n" + errorResult)
          UclidResult(errorResult)
        case e: SolverMismatchError =>
          errorResult.messages = "\n" + e.msg
          println("\n" + errorResult)
          UclidResult(errorResult)
        case e: SmtParserError =>
          errorResult.messages = "\n" + e.msg
          println("\n" + errorResult)
          UclidResult(errorResult)
        case e: TimeoutException =>
          errorResult.messages = "\n" + e.toString()
          println("\n" + errorResult)
          UclidResult(errorResult)
      }
    }
    results.toList
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
