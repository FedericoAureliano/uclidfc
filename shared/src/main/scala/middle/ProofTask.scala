package middle

import scala.collection.mutable.ListBuffer

class ProofTask(
  val program: Program,
  val obligations: ListBuffer[(String, Ref)] = new ListBuffer()
)
