package front

import scala.util.parsing.input.Position
import java.io.File
import java.io.PrintWriter

object Utils {

  def writeToFile(p: String, s: String): Unit = {
    val pw = new PrintWriter(new File(p.replace(" ", "_")))
    try pw.write(s)
    finally pw.close()
  }

  def assert(
    b: Boolean,
    err: => String /* error may be lazily computed. */
  ): Unit =
    if (!b) {
      throw new AssertionError(err)
    }

  def raiseParsingError(
    err: => String,
    pos: => Position,
    fileName: => Option[String]
  ): Unit =
    throw new ParserError(err, Some(pos), fileName)

  def checkParsingError(
    b: Boolean,
    err: => String,
    pos: => Position,
    fileName: => Option[String]
  ): Unit =
    if (!b) { raiseParsingError(err, pos, fileName) }

  class UnimplementedException(msg: String = null, cause: Throwable = null)
      extends java.lang.UnsupportedOperationException(msg, cause)

  class RuntimeError(msg: String = null, cause: Throwable = null)
      extends java.lang.RuntimeException(msg, cause)

  class EvaluationError(msg: String, cause: Throwable = null)
      extends RuntimeError(msg, cause)
  class SyGuSParserError(msg: String) extends RuntimeError(msg, null)

  class AssertionError(msg: String = null, cause: Throwable = null)
      extends java.lang.RuntimeException(msg, cause)

  class ParserError(
    val msg: String,
    val pos: Option[Position],
    val filename: Option[String]
  ) extends java.lang.RuntimeException(msg, null) {

    override def hashCode: Int =
      msg.hashCode() + pos.hashCode() + filename.hashCode()
    def errorName = "Parser"

    lazy val positionStr = (filename, pos) match {
      case (Some(f), Some(p)) => f.toString + ", line " + p.line.toString
      case (None, Some(p))    => "line " + p.line.toString
      case _                  => ""
    }

    lazy val fullStr = pos match {
      case Some(p) => p.longString
      case None    => ""
    }
  }

  class TypeError(msg: String, pos: Option[Position], filename: Option[String])
      extends ParserError(msg, pos, filename) {

    override def equals(that: Any): Boolean =
      that match {
        case that: TypeError =>
          (msg == that.msg) && (pos == that.pos) && filename == that.filename
        case _ =>
          false
      }
    override def errorName = "Type"
  }

  class SyntaxError(
    msg: String,
    pos: Option[Position],
    filename: Option[String]
  ) extends ParserError(msg, pos, filename) {

    override def equals(that: Any): Boolean =
      that match {
        case that: SyntaxError =>
          (msg == that.msg) && (pos == that.pos) && filename == that.filename
        case _ => false
      }
    override def errorName = "Syntax"
  }

  class TypeErrorList(val errors: List[TypeError])
      extends java.lang.RuntimeException("Type errors.", null)

  class ParserErrorList(val errors: List[(String, front.ASTPosition)])
      extends java.lang.RuntimeException("Parser Errors", null)

  class UnknownIdentifierException(val ident: front.Identifier)
      extends java.lang.RuntimeException(
        "Unknown identifier: " + ident.toString
      )

  def existsOnce(a: List[front.Identifier], b: front.Identifier): Boolean =
    existsNTimes(a, b, 1)

  def existsNone(a: List[front.Identifier], b: front.Identifier): Boolean =
    existsNTimes(a, b, 0)

  def existsNTimes(
    a: List[front.Identifier],
    b: front.Identifier,
    n: Int
  ): Boolean =
    a.count { x =>
      x.name == b.name
    } == n

  def allUnique(a: List[front.Identifier]): Boolean = a.distinct.size == a.size

  def join(things: Seq[String], sep: String) =
    things.size match {
      case 0 => ""
      case _ =>
        things.head + things.tail.foldLeft("")((acc, i) => acc + sep + i)
    }

  def topoSort[T](roots: List[T], graph: Map[T, Set[T]]): List[T] = {
    def visit(node: T, visitOrder: Map[T, Int]): Map[T, Int] =
      if (visitOrder.contains(node)) {
        visitOrder
      } else {
        val visitOrderP = visitOrder + (node -> visitOrder.size)
        graph.get(node) match {
          case Some(nodes) =>
            nodes.foldLeft(visitOrderP)((acc, m) => visit(m, acc))
          case None => visitOrderP
        }
      }
    // now walk through the dep graph
    val order: List[(T, Int)] =
      roots.foldLeft(Map.empty[T, Int])((acc, r) => visit(r, acc)).toList
    order.sortWith((x, y) => x._2 < y._2).map(p => p._1)
  }

  def schedule[T](roots: List[T], graph: Map[T, Set[T]]): List[T] = {
    def visit(node: T, visitOrder: Map[T, Int]): Map[T, Int] =
      if (visitOrder.contains(node)) {
        visitOrder
      } else {
        val visitOrderP = graph.get(node) match {
          case Some(nodes) =>
            nodes.foldLeft(visitOrder)((acc, m) => visit(m, acc))
          case None => visitOrder
        }
        val outputOrder = visitOrderP + (node -> visitOrderP.size)
        outputOrder
      }
    val order: List[(T, Int)] =
      roots.foldLeft(Map.empty[T, Int])((acc, r) => visit(r, acc)).toList
    order.sortWith((x, y) => x._2 < y._2).map(p => p._1)
  }

  def findCyclicDependencies[U, V](
    graph: Map[U, Set[U]],
    roots: Seq[U],
    errorFn: ((U, List[U]) => V)
  ): List[V] = {
    def visit(node: U, stack: List[U], errorsIn: List[V]): List[V] =
      if (stack contains node) {
        val cycleError = errorFn(node, stack)
        cycleError :: errorsIn
      } else {
        graph.get(node) match {
          case Some(nodes) =>
            nodes.foldLeft(errorsIn)((acc, n) => visit(n, node :: stack, acc))
          case None =>
            errorsIn
        }
      }
    roots.foldLeft(List.empty[V])((acc, r) => visit(r, List.empty[U], acc))
  }
}

class UniqueNamer(val prefix: String) {
  var count = 0

  def newName(): String = {
    val name: String = prefix + "_" + count.toString
    count = count + 1
    return name
  }
}

class Memo[I, O](f: I => O) {
  var memoHashMap = new scala.collection.mutable.HashMap[I, O]
  val fun = f

  def apply(key: I): O =
    memoHashMap.get(key) match {
      case Some(v) => v
      case None => {
        val item = fun(key)
        memoHashMap.put(key, item)
        item
      }
    }
}
