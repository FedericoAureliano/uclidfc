package web

import scala.reflect.ClassTag
import org.scalajs.dom.document
import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLPreElement, HTMLTextAreaElement}

import scala.collection.mutable.ArrayBuffer

import middle._

import front.UclidParser

object WebApp {

  def main(args: Array[String]): Unit = {
    val areaFull =
      document.querySelector("#demofull").asInstanceOf[HTMLTextAreaElement]
    areaFull.oninput = (e: dom.Event) =>
      updateFull(areaFull.value, "demofullanswer")
  }

  def updateFull(text: String, id: String): Unit = {
    val result = document.querySelector(s"#$id").asInstanceOf[HTMLPreElement]

    try {
      val parsed = UclidParser.parseModel("web", text)

      val pResult = Encoder.run(parsed, Some("main"))

      result.textContent = Printer.programToQuery(pResult)
    } catch {
      case e: Throwable => result.textContent = e.toString()
    }
  }
}
