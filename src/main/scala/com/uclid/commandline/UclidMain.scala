package com.uclid.commandline

import com.uclid.context.{Check, Command, _}
import com.uclid.solverinterface.compiler._
import com.uclid.solverinterface.solver._
import com.uclid.termgraph
import com.uclid.uclidinterface.compiler._
import com.uclid.uclidinterface.compiler.parser._

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

  def main(args: Array[String]): Unit =
    parseOptions(args) match {
      case None => sys.exit(2)
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
    plusMinusZero: Boolean = false,
    blastEnumQuantifierFlag: Boolean = false,
    assertionOverConjunction: Boolean = false,
    containsOverConcat: Boolean = false,
    containsOverReplace: Boolean = false,
    indexOfGTZGadgets: Boolean = false,
    outFile: Option[String] = None,
    prettyPrint: Boolean = false,
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
          s"Timeout (in seconds) to give the solver."
        )

      opt[String]('o', "out")
        .valueName("<file>")
        .action((x, c) => c.copy(outFile = Some(x)))
        .text("Write query to <file>.")

      opt[Unit]("pretty-print")
        .action((_, c) => c.copy(prettyPrint = true))
        .text("Try to make output queries human readable.")

      opt[Unit]("skip-solver")
        .action((_, c) => c.copy(run = false))
        .text("Don't run the solver.")

      arg[java.io.File]("<file> ...")
        .unbounded()
        .required()
        .action((x, c) => c.copy(files = c.files :+ x))
        .text("List of input files.")

      note(sys.props("line.separator") + "Analysis")

      opt[Unit]("print-features")
        .action((_, c) => c.copy(features = true))
        .text("Print query features.")

      note(sys.props("line.separator") + "Rewrites")

      opt[Unit]("plus-minus-zero")
        .action((_, c) => c.copy(plusMinusZero = true))
        .text(
          "Rewrite \"(+ a b ... 0 ... z)\" to \"(+ a b ... z)\" and \"(- 0 x)\" to \"-x\" and \"(- x 0)\" to \"x\"."
        )

      opt[Unit]("blast-enum-quantifiers")
        .action((_, c) => c.copy(blastEnumQuantifierFlag = true))
        .text(
          "Rewrite quantifiers over enums to finite disjunctions/conjunctions."
        )

      opt[Unit]("assertion-over-conjunction")
        .action((_, c) => c.copy(assertionOverConjunction = true))
        .text(
          "Rewrite asserted conjunctions to repeated assertion."
        )

      opt[Unit]("contains-over-concat")
        .action((_, c) => c.copy(containsOverConcat = true))
        .text(
          "Rewrite \"xy contains c\" as \"x contains c or y contains c,\" where c is a literal string of length 1."
        )

      opt[Unit]("contains-over-replace")
        .action((_, c) => c.copy(containsOverReplace = true))
        .text(
          "Rewrite \"(replace c1 with c2 in x) contains c3,\" where c1, c2, and c3 are literal strings of length 1."
        )

      opt[Unit]("indexof-gte-zero-gadgets")
        .action((_, c) => c.copy(indexOfGTZGadgets = true))
        .text(
          "Rewrite \"index of y in x >= 0\" to \"x contains y.\""
        )
    }
    parser.parse(args, Config())
  }

  /** This version of 'main' does all the real work.
    */
  def main(config: Config): List[UclidResult] = {
    val errorResult =
      new ProofResult()

    val inputLanguage = if (config.files.forall(f => f.getName.endsWith(".ucl"))) {
      "UCLID"
    } else if (config.files.forall(f => f.getName.endsWith(".smt2"))) {
      "SMT"
    } else {
      errorResult.messages = "\nAll files must be Uclid5 queries (.ucl) or SMT2 queries (.smt2)!"
      println("\n"+errorResult)
      return List(UclidResult(errorResult))
    }

    val solvers = config.solvers.map(solver => solver match {
      case Solvers.alt_ergo => new AltErgo()
      case Solvers.cvc4     => new CVC4()
      case Solvers.vampire  => new Vampire()
      case Solvers.z3       => new Z3()
    })

    val solver = if (solvers.length == 0) {
      Z3()
    } else if (solvers.length == 1) {
      solvers.head
    } else {
      Medley(solvers)
    }

    var parseDuration = 0.0
    var processDuration = 0.0
    var analysisDuration = 0.0
    if (inputLanguage == "UCLID") {
      List(runUclidMode(solver, config))
    } else {
      runSMTMode(solver, config)
    }
  }

  def runUclidMode(solver: Solver, config: Config) : UclidResult = {
    val errorResult =
      new ProofResult()

    try {
      print("\nParsing input ... ")
      val startParse = System.nanoTime
      val modules = UclidCompiler.parse(config.files) match {
        case Right(m) => m
        case Left(e) =>
          errorResult.messages = "\n" + e.toString()
          println("\n"+errorResult)
          return UclidResult(errorResult)
      }
      var parseDuration = (System.nanoTime - startParse) / 1e9d
      println(s"Parsing completed in ${parseDuration} seconds.")

      print("Processing model ... ")
      val startProcess = System.nanoTime
      val ctx = UclidCompiler.process(modules, Some(config.mainModuleName))
      if (config.plusMinusZero) {
        ctx.termgraph.plusMinusZero()
      }
      if (config.blastEnumQuantifierFlag) {
        ctx.termgraph.blastEnumQuantifier()
      }
      if (config.containsOverConcat) {
        ctx.termgraph.containsOverConcat()
      }
      if (config.containsOverReplace) {
        ctx.termgraph.containsOverReplace()
      }
      if (config.indexOfGTZGadgets) {
        ctx.termgraph.indexOfGTZGadgets()
      }
      if (config.assertionOverConjunction) {
        throw new SemanticError("Flatten Assertions Not Yet Supported In UCLID Mode")
      }
      var processDuration = (System.nanoTime - startProcess) / 1e9d
      println(s"Processing completed in ${processDuration} seconds.")

      print("Analyzing model ... ")
      val startAnalysis = System.nanoTime
      val features = if (config.features) {
        ctx.termgraph.featuresList(ctx.entryPoints())
      } else {
        List.empty
      }
      var analysisDuration = (System.nanoTime - startAnalysis) / 1e9d
      println(s"Analysis completed in ${analysisDuration} seconds.")
      if (config.features) {
        println(features.map(f => "-- " + f).mkString("\n"))
      }

      val res = solver.solve(config.run, config.timeout, ctx, config.outFile, config.prettyPrint)
      val ret = if (ctx.ignoreResult()) {
        UclidResult(ProofResult(None, res._1.messages), parseDuration, processDuration, analysisDuration, res._2, res._3)
      } else {
        UclidResult(res._1, parseDuration, processDuration, analysisDuration, res._2, res._3)
      }

      if (config.run) {
        println(ret.presult)
      } else {
        println(ret.presult.messages)
      }
      ret 
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = "\n" + e.toString()
        println("\n"+errorResult)
        UclidResult(errorResult)
      case e: SemanticError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        UclidResult(errorResult)
      case e: SolverMismatchError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        UclidResult(errorResult)
      case e: SmtParserError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        UclidResult(errorResult)
      case e: TimeoutException => 
        errorResult.messages = "\n" + e.toString()
        println("\n"+errorResult)
        UclidResult(errorResult)
    }
  }

  def runSMTMode(solver: Solver, config: Config) : List[UclidResult] = {
    // Must be SMT Language
    val results = config.files.map(f => {
      val errorResult = new ProofResult()
      try {
        print(s"\nParsing ${f.getName()} ... ")
        val startParse = System.nanoTime
        val ctx = SmtCompiler.compile(scala.io.Source.fromFile(f).mkString(""))
        var parseDuration = (System.nanoTime - startParse) / 1e9d
        println(s"Parsing completed in ${parseDuration} seconds.")

        print("Processing query ... ")
        val startProcess = System.nanoTime
        if (config.plusMinusZero) {
          ctx.termgraph.plusMinusZero()
        }
        if (config.blastEnumQuantifierFlag) {
          ctx.termgraph.blastEnumQuantifier()
        }
        if (config.containsOverConcat) {
          ctx.termgraph.containsOverConcat()
        }
        if (config.containsOverReplace) {
          ctx.termgraph.containsOverReplace()
        }
        if (config.indexOfGTZGadgets) {
          ctx.termgraph.indexOfGTZGadgets()
        }
        if (config.assertionOverConjunction) {
          ctx.script = ctx.script.foldLeft(List.empty)((acc, c) => {
            c match {
              case a : Assert => {
                acc ++ ctx.termgraph.assertionOverConjunction(a)
              }
              case _ => acc ++ List(c)
            }
          })
        }
        var processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")

        print("Analyzing query ... ")
        val startAnalysis = System.nanoTime
        val features = if (config.features) {
          ctx.termgraph.featuresList(ctx.entryPoints())
        } else {
          List.empty
        }
        var analysisDuration = (System.nanoTime - startAnalysis) / 1e9d
        println(s"Analysis completed in ${analysisDuration} seconds.")
        if (config.features) {
          println(features.map(f => "-- " + f).mkString("\n"))
        }
  
        val res = solver.solve(config.run, config.timeout, ctx, config.outFile, config.prettyPrint)
        val ret = if (ctx.ignoreResult()) {
          UclidResult(ProofResult(None, res._1.messages), parseDuration, processDuration, analysisDuration, res._2, res._3)
        } else {
          UclidResult(res._1, parseDuration, processDuration, analysisDuration, res._2, res._3)
        }

        if (config.run) {
          println(ret.presult)
        } else {
          println(ret.presult.messages)
        }

        ret
      } catch {
        case (e: java.io.FileNotFoundException) =>
          errorResult.messages = "\n" + e.toString()
          println("\n"+errorResult)
          UclidResult(errorResult)
        case e: SemanticError =>
          errorResult.messages = "\n" + e.msg
          println("\n"+errorResult)
          UclidResult(errorResult)
        case e: SolverMismatchError =>
          errorResult.messages = "\n" + e.msg
          println("\n"+errorResult)
          UclidResult(errorResult)
        case e: SmtParserError =>
          errorResult.messages = "\n" + e.msg
          println("\n"+errorResult)
          UclidResult(errorResult)
        case e: TimeoutException => 
          errorResult.messages = "\n" + e.toString()
          println("\n"+errorResult)
          UclidResult(errorResult)
      }
    })
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
