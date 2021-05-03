package com.uclid.utility

import java.io._ 
import scala.io.Source
import scala.collection.mutable.{HashMap}

// map from solver to map from benchmark to solver result and time taken
class SimulationTable (table: Map[String, Map[String, (String, Double)]]) {

    def simulate(solver: String, benchmark: String) : (String, Double) = {
        if table.contains(solver) && table(solver).contains(benchmark) then table(solver)(benchmark)
        else ("Error", 0.0)
    }

}

object SimulationTable {
    def load(location: String) : SimulationTable = {
        val data = new File(location);
        data.createNewFile()
        val content = Source.fromFile(data).getLines.map(_.split(","))
        val header = if content.isEmpty then Array("benchmark","solver","answer","time") else content.next
        assert(header.length == 4, "Data should have four columns!")
        assert(header(0) == "benchmark", "First column of Data should be \"benchmark\"")
        assert(header(1) == "solver", "Second column of Data should be \"solver\"")
        assert(header(2) == "answer", "Third column of Data should be \"answer\"")
        assert(header(3) == "time", "Fourth column of Data should be \"time\"")

        val hmap : HashMap[String, HashMap[String, (String, Double)]] = new HashMap()

        content.foreach(line => {
            val tmp = (line(0), (line(2), line(3).toDouble))
             if hmap.contains(line(1)) then {
                 hmap(line(1)).addOne(tmp)
             } else {
                 hmap.addOne(line(1), HashMap(tmp))
             }
        })

        SimulationTable(hmap.map(f => (f._1, f._2.toMap)).toMap)
    }
}