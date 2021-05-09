package com.uclid.idiolect

import com.uclid.context._
import com.uclid.termgraph._

import scala.collection.mutable.{HashMap}
import java.io._ 
import scala.io.Source

class WCFG (weights: HashMap[String, Double]) {

    def likelihood(ctx: Context) : Double = {
        var p = 0.0
        val marks = ctx.termgraph.mark(ctx.entryPoints())

        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => p = weights.getOrElse(name, 0.0) + p
                case None => 
            }
            case _ =>
        })

        p
    }

    override def toString() : String = {
        weights.toList.prepended(("production rule", "weight")).map((k, v) => s"$k,$v").mkString("\n")
    }

    // this was not the best solver, so punish it
    def punish(ctx: Context, factor: Double) : Unit = {
        var count = 0.0
        val newWeights: HashMap[String, Double]= HashMap()
        val marks = ctx.termgraph.mark(ctx.entryPoints())
        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => newWeights.update(name, newWeights.getOrElse(name, 0.0) - 1.0); count += 1.0
                case None => 
            }
            case _ =>
        })
        newWeights.keys.foreach(k => {
            newWeights(k) = (newWeights(k)/count)*factor
            weights.update(k, weights.getOrElse(k, 0.0) + newWeights(k))
        })
    }

    def save(location: String) : Unit = {
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

        val hmap : HashMap[String, Double] = HashMap(content.map(line => line(0) -> line(1).toDouble).toList: _*)

        WCFG(hmap)
    }
}
