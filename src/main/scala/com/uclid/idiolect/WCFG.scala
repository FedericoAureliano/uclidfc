package com.uclid.idiolect

import com.uclid.context._
import com.uclid.termgraph._

import scala.collection.mutable.{HashMap}
import java.io._ 
import scala.io.Source

class WCFG (weights: HashMap[String, Int]) {

    def likelihood(ctx: Context) : Int = {
        var p = 1
        val marks = ctx.termgraph.mark(ctx.entryPoints())

        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => p = weights.getOrElse(name, 1) * p
                case None => 
            }
            case Synthesis(_, _, _) => p = weights.getOrElse("synthesis", 0) * p
            case _ =>
        })

        p
    }

    override def toString() : String = {
        weights.toList.prepended(("production rule", "weight")).map((k, v) => s"$k,$v").mkString("\n")
    }

    // this was the best solver, so reward it
    def update(ctx: Context) : Unit = {
        val marks = ctx.termgraph.mark(ctx.entryPoints())

        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => weights.update(name, weights.getOrElse(name, 0) + 1)
                case None => 
            }
            case Synthesis(_, _, _) => weights.update("synthesis", weights.getOrElse("synthesis", 0) + 1)
            case _ =>
        })
    }

    def save(location: String) : Unit = {
        weights.getOrElseUpdate("synthesis", 0)
        
        val file = new File(location)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write("production rule,weight\n")
        bw.write(weights.toList.map((k, v) => s"$k,$v").mkString("\n"))
        bw.close()
    }
}

object WCFG {

    def load(location: String) : WCFG = {
        val wcfg = new File(location);
        wcfg.createNewFile()
        val content = Source.fromFile(wcfg).getLines.map(_.split(","))
        val header = if content.isEmpty then Array("production rule", "weight") else content.next
        assert(header.length == 2, "WCFGs should have two columns!")
        assert(header(0) == "production rule", "First column of WCFG should be \"production rule\"")
        assert(header(1) == "weight", "Second column of WCFG should be \"weight\"")

        val hmap : HashMap[String, Int] = HashMap(content.map(line => line(0) -> line(1).toInt).toList: _*)

        WCFG(hmap)
    }
}
