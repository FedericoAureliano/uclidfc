package com.uclid.idiolect

import com.uclid.context._
import com.uclid.termgraph._

import scala.collection.mutable.{HashMap}
import java.io._ 

class WCFG (weights: HashMap[String, Int] = new HashMap()) {

    def likelihood(ctx: SyMTContext) : Int = {
        var p = 1
        val marks = ctx.termgraph.mark(ctx.entryPoints())

        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => weights.getOrElse(name, 1) * p
                case None => 
            }
            case _ =>
        })

        p
    }

    def update(ctx: SyMTContext) : Unit = {
        val marks = ctx.termgraph.mark(ctx.entryPoints())
        marks.zipWithIndex.foreach((m, i) => if m then ctx.termgraph.getStmt(ctx.termgraph.findTarget(i)) match {
            case Application(parent, _) => ctx.termgraph.getName(parent) match {
                case Some(name) => {
                    weights.getOrElseUpdate(name, 0)
                    weights(name) = weights(name) + 1
                }
                case None => 
            }
            case _ =>
        })
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
        val weights : HashMap[String, Int] = HashMap()
        val bufferedSource = io.Source.fromFile(location)
        for (line, count) <- bufferedSource.getLines.zipWithIndex do {
            val cols = line.split(",").map(_.trim)
            weights(cols(0)) = cols(1).toInt
        }
        bufferedSource.close

        WCFG(weights)
    }
}
