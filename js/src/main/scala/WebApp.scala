package web

import scala.reflect.ClassTag
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLPreElement, HTMLTextAreaElement}

import scala.collection.mutable.ArrayBuffer

import middle._

import front.UclidParser
import front.Utils

object WebApp {

  def main(args: Array[String]): Unit = {
    val area0 =
      document.querySelector("#demoq0").asInstanceOf[HTMLTextAreaElement]
    area0.oninput = (e: dom.Event) => updateSMTLIB(area0.value, "demoa0")

    val area1 =
      document.querySelector("#demoq1").asInstanceOf[HTMLTextAreaElement]
    area1.oninput = (e: dom.Event) => updateLetify(area1.value, "demoa1")

    val area2 =
      document.querySelector("#demoq2").asInstanceOf[HTMLTextAreaElement]
    area2.oninput = (e: dom.Event) => updateInline(area2.value, "demoa2")

    val area3 =
      document.querySelector("#demoq3").asInstanceOf[HTMLTextAreaElement]
    area3.oninput = (e: dom.Event) => updateCompact(area3.value, "demoa3")

    val areaFull =
      document.querySelector("#demofull").asInstanceOf[HTMLTextAreaElement]
    areaFull.oninput = (e: dom.Event) =>
      updateFull(areaFull.value, "demofullanswer")
  }

  def updateSMTLIB(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]

    val program = new Program(ArrayBuffer[Instruction](), 0)

    try {
      for (line <- text.split("\n")) {
        program.appendIncOne(parse(line))
      }
      result.textContent = Interface.programToQuery(program)
    } catch {
      case e: Throwable => result.textContent = e.toString()
    }
  }

  def updateLetify(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]
    val program = new Program(ArrayBuffer[Instruction](), 0)

    try {
      for (line <- text.split("\n")) {
        program.appendIncOne(parse(line))
      }
      val answer = Rewriter.letify(program, "tmp")

      result.textContent = answer.toString()
    } catch {
      case e: Throwable => result.textContent = e.toString()
    }
  }

  def updateInline(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]
    val program = new Program(ArrayBuffer[Instruction](), 0)

    try {
      for (line <- text.split("\n")) {
        program.appendIncOne(parse(line))
      }
      val answer = Rewriter.inlineApplication(program, 0)

      result.textContent = answer.toString()
    } catch {
      case e: Throwable => result.textContent = e.toString()
    }
  }

  def updateCompact(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]
    val program = new Program(ArrayBuffer[Instruction](), 0)

    try {
      for (line <- text.split("\n")) {
        program.appendIncOne(parse(line))
      }

      Rewriter.reduceDuplicates(program)
      Rewriter.reduceIndirection(program)

      val cleaned = Garbage.collectGarbage(program)

      result.textContent = cleaned.toString()
    } catch {
      case e: Throwable => result.textContent = e.toString()
    }
  }

  def updateFull(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]

    try {
      val parsed = UclidParser.parseModel("web", text)

      val pResult = Interpreter.run(parsed, Some("main"))

      pResult.program.head = pResult.obligations.last._2.loc

      result.textContent = Interface.programToQuery(pResult.program)
    } catch {
      case e: Utils.SyntaxError =>
        result.textContent = s"${e.pos.get}: ${e.msg}"
      case e: Throwable => result.textContent = e.toString()
    }
  }

  def parse(line: String): Instruction = {
    def parseRef(r: String): Ref = {
      assert(r.startsWith("#"), "must start with #")
      Ref(r.slice(1, r.length).toInt)
    }

    def parseNumeral(r: String): Numeral =
      Numeral(r.toInt)

    def parseName(n: String): String = {
      assert(!n.startsWith("#"), "must not start with #")
      n
    }

    val atoms = line.replaceAll("\t", " ").trim.split(" +")
    atoms(0) match {
      case "[tst]" =>
        TheorySort(
          parseName(atoms(1)),
          atoms.slice(2, atoms.length).map(a => parseRef(a)).toList
        )
      case "[smo]" => SortMacro(parseName(atoms(1)), parseRef(atoms(2)))
      case "[spr]" => SortParameter(parseName(atoms(1)))
      case "[ust]" => UserSort(parseName(atoms(1)), parseNumeral(atoms(2)))
      case "[fpr]" => FunctionParameter(parseName(atoms(1)), parseRef(atoms(2)))
      case "[tmo]" =>
        TheoryMacro(
          parseName(atoms(1)),
          atoms.slice(2, atoms.length).map(a => parseRef(a)).toList
        )
      case "[umo]" =>
        UserMacro(
          parseName(atoms(1)),
          parseRef(atoms(2)),
          parseRef(atoms(3)),
          atoms.slice(4, atoms.length).map(a => parseRef(a)).toList
        )
      case "[ufn]" =>
        UserFunction(
          parseName(atoms(1)),
          parseRef(atoms(2)),
          atoms.slice(3, atoms.length).map(a => parseRef(a)).toList
        )
      case "[ctr]" =>
        Constructor(
          parseName(atoms(1)),
          parseRef(atoms(2)),
          atoms.slice(3, atoms.length).map(a => parseRef(a)).toList
        )
      case "[slr]" => Selector(parseName(atoms(1)), parseRef(atoms(2)))
      case "[adt]" =>
        DataType(
          parseName(atoms(1)),
          atoms.slice(2, atoms.length).map(a => parseRef(a)).toList
        )
      case "[mod]" =>
        Module(
          parseName(atoms(1)),
          parseRef(atoms(2)),
          parseRef(atoms(3)),
          parseRef(atoms(4)),
          parseRef(atoms(5))
        )
      case "[app]" =>
        Application(
          parseRef(atoms(1)),
          atoms.slice(2, atoms.length).map(a => parseRef(a)).toList
        )
      case "[num]" => parseNumeral(atoms(1))
      case _ => {
        // it's a ref
        assert(atoms.length == 1, "there must be only one atom")
        parseRef(atoms(0))
      }
    }
  }
}
