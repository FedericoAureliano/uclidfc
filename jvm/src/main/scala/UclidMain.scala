package uclid

import scala.util.parsing.combinator._
import scala.collection.immutable._
import front.{Identifier, Module, _}
import front.Utils.ParserErrorList

import interface.out.smt.getSmtCtxString
import interface.in.Translate

/** This is the main class for Uclid.
  *
  */
object UclidMain {

  def main(args: Array[String]): Unit =
    parseOptions(args) match {
      case None         =>
      case Some(config) => main(config)
    }

  /** Command-line configuration flags for uclid5.
    *
    * @param mainModuleName The name of the main module.
    * @param files List of files that should parsed and analyzed.
    */
  case class Config(
    mainModuleName: String = "main",
    files: Seq[java.io.File] = Seq()
  )

  def parseOptions(args: Array[String]): Option[Config] = {
    val parser = new scopt.OptionParser[Config]("uclid") {
      head("uclid", "1.0")

      opt[String]('m', "main")
        .valueName("<Module>")
        .action((x, c) => c.copy(mainModuleName = x))
        .text("Name of the main module.")

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
  def main(config: Config): Unit =
    try {
      val mainModuleName = Identifier(config.mainModuleName)
      val modules = compile(config.files, mainModuleName)
      val mainModule = instantiate(config, modules, mainModuleName)
      mainModule match {
        case Some(m) => {
          val term = Translate.modelToProgram(modules)
          println(getSmtCtxString(term))
        }
        case None =>
          throw new Utils.ParserError("Unable to find main module", None, None)
      }
      println(
        "Finished execution for module: %s.".format(mainModuleName.toString)
      )
    } catch {
      case (e: java.io.FileNotFoundException) =>
        println("Error: " + e.getMessage() + ".")
      case (p: Utils.ParserError) =>
        println(
          "%s error %s: %s.\n%s"
            .format(p.errorName, p.positionStr, p.getMessage, p.fullStr)
        )
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

  /** Instantiate modules.
    *
    * @param moduleList List of modules to be analyzed.
    * @param mainModuleName Name of main module.
    */
  def instantiate(
    config: Config,
    moduleList: List[Module],
    mainModuleName: Identifier
  ): Option[Module] = {
    if (moduleList.find(m => m.id == mainModuleName).isEmpty) {
      return None
    }

    // return main module.
    moduleList.find((m) => m.id == mainModuleName)
  }
}
