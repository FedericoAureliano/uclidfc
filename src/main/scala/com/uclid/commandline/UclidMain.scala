package com.uclid.commandline

import com.uclid.termgraph
import com.uclid.context._
import com.uclid.context.{Command, Check}
import com.uclid.solverinterface.solver._
import com.uclid.solverinterface.compiler._
import com.uclid.uclidinterface.compiler.parser._
import com.uclid.uclidinterface.compiler._

import scala.collection.mutable.ListBuffer

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
    features: Boolean = false,
    plusMinusZero: Boolean = false,
    blastEnumQuantifierFlag: Boolean = false,
    assertionOverConjunction: Boolean = false,
    containsOverConcat: Boolean = false,
    containsOverReplace: Boolean = false,
    lengthOverSubstring: Boolean = false,
    indexOfSubstringGadget: Boolean = false,
    indexOfGteZeroGadget: Boolean = false,
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
          "Rewrite \"(+ a b ... 0 ... z)\" to \"(+ a b ... z)\""
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
          "Rewrite \"xy contains c\" as \"x contains c or y contains c\"."
        )

      opt[Unit]("contains-over-replace")
        .action((_, c) => c.copy(containsOverReplace = true))
        .text(
          "Rewrite \"(replace c1 with c2 in x) contains c3\" as \"x contains c3\" if c1 = c3 and c2 = c3; as \"false\" if c1 = c3 and c2 != c3; as \"x contains c3\" if c1 != c3 and c2 != c3; and \"x contains c3 or x contains c1\" if c1 != c3 and c2 == c3."
        )

      opt[Unit]("length-over-substring")
        .action((_, c) => c.copy(lengthOverSubstring = true))
        .text(
          "Rewrite \"len(x[n:m])\" as \"ite(len(x) - n - (len(x) - m) < 0, 0, len(x) - n - (len(x) - m))\"."
        )

      opt[Unit]("indexof-substring-gadget")
        .action((_, c) => c.copy(indexOfSubstringGadget = true))
        .text(
          "Rewrite \"index of c in x[k:len(x)-k]\" to \"index of c in z\" where x = yz and len(y) = k."
        )

      opt[Unit]("indexof-gte-zero-gadget")
        .action((_, c) => c.copy(indexOfGteZeroGadget = true))
        .text(
          "Rewrite \"index of c in x >= 0\" to \"x contains c\""
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

    try {
      if (inputLanguage == "UCLID") {
        print("\nParsing input ... ")
        val startParse = System.nanoTime
        val modules = UclidCompiler.parse(config.files) match {
          case Right(m) => m
          case Left(e) =>
            errorResult.messages = "\n" + e.toString()
            return List(UclidResult(errorResult))
        }
        parseDuration = (System.nanoTime - startParse) / 1e9d
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
        if (config.lengthOverSubstring) {
          ctx.termgraph.lengthOverSubstring()
        }
        if (config.indexOfSubstringGadget) {
          ctx.script = ctx.termgraph.indexOfSubstringGadget() ++ ctx.script
        }
        if (config.indexOfGteZeroGadget) {
          ctx.termgraph.indexOfGteZeroGadget()
        }
        if (config.assertionOverConjunction) {
          throw new SemanticError("Flatten Assertions Not Yet Supported In UCLID Mode")
        }
        processDuration = (System.nanoTime - startProcess) / 1e9d
        println(s"Processing completed in ${processDuration} seconds.")

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
  
        val res = solver.solve(config.run, ctx, config.outFile, config.prettyPrint)
        val ret = if (ctx.ignoreResult()) {
          List(UclidResult(ProofResult(None, res._1.messages), parseDuration, processDuration, analysisDuration, res._2, res._3))
        } else {
          List(UclidResult(res._1, parseDuration, processDuration, analysisDuration, res._2, res._3))
        }

        if (config.run) {
          println(ret(0).presult)
        } else {
          println(ret(0).presult.messages)
        }

        ret 
      } else {
        // Must be SMT Language
        val results = config.files.map(f => {
          print(s"\nParsing ${f.getName()} ... ")
          val startParse = System.nanoTime
          val ctx = SmtCompiler.compile(scala.io.Source.fromFile(f).mkString(""))
          parseDuration = (System.nanoTime - startParse) / 1e9d
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
          if (config.lengthOverSubstring) {
            ctx.termgraph.lengthOverSubstring()
          }
          if (config.indexOfSubstringGadget) {
            ctx.script = ctx.termgraph.indexOfSubstringGadget() ++ ctx.script
          }
          if (config.indexOfGteZeroGadget) {
            ctx.termgraph.indexOfGteZeroGadget()
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
          processDuration = (System.nanoTime - startProcess) / 1e9d
          println(s"Processing completed in ${processDuration} seconds.")
  
          print("Analyzing query ... ")
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
    
          val res = solver.solve(config.run, ctx, config.outFile, config.prettyPrint)
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
        })
        results.toList
      }
    } catch {
      case (e: java.io.FileNotFoundException) =>
        errorResult.messages = "\n" + e.toString()
        println("\n"+errorResult)
        List(UclidResult(errorResult))
      case e: SemanticError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        List(UclidResult(errorResult))
      case e: SolverMismatchError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        List(UclidResult(errorResult))
      case e: SmtParserError =>
        errorResult.messages = "\n" + e.msg
        println("\n"+errorResult)
        List(UclidResult(errorResult))
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
