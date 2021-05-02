package com.uclid.idiolect

import com.uclid.context._
import com.uclid.termgraph._

import scala.collection.mutable.{HashMap}
import java.io._ 
import scala.io.Source

class WCFG (weights: Map[String, Int]) {

    def likelihood(ctx: Context) : Int = {
        var p = 1
        val marks = ctx.termgraph.mark(ctx.entryPoints())

        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => p = weights.getOrElse(name, 1) * p
                case None => 
            }
            case _ =>
        })

        p
    }

    override def toString() : String = {
        weights.toList.map((k, v) => s"$k: $v").mkString("\n")
    }

    def save(location: String) : Unit = {
        val file = new File(location)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write(weights.toList.map((k, v) => s"$k, $v").mkString("\n"))
        bw.close()
    }
}

object WCFG {

    def load(location: String) : WCFG = {
        val content=Source.fromFile(location).getLines.map(_.split(","))
        val header=content.next
        assert(header.length == 2, "WCFGs should have two columns!")
        assert(header(0) == "production rule", "First column of WCFG should be \"production rule\"")
        assert(header(1) == "weight", "Second column of WCFG should be \"weight\"")
        WCFG(content.map(line => (line(0), line(1).toInt)).toMap)
    }
}
