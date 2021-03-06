package com.uclid.termgraph

import scala.collection.mutable.ArrayBuffer

/*
Passing semantic checks must gurantee a well formed query
 */
trait WellFormed() extends AbstractTermGraph {

  def checkRefBounds(): Unit = {
    def checkRef(r: Int): Boolean = r < getStmts().length && r >= 0

    getStmts().zipWithIndex.foreach { case (s, i) =>
      s match {
        case r: Ref =>
          assert(checkRef(r.loc))
        case Numeral(_) =>
        case TheorySort(_, p) =>
          p.foreach(a => assert(checkRef(a)))
        case UserSort(_, _) =>
        case FunctionParameter(_, sr) =>
          assert(checkRef(sr))
        case TheoryMacro(_, p) =>
          p.foreach(a => assert(checkRef(a)))
        case UserMacro(_, sr, b, p) =>
          assert(checkRef(sr))
          assert(checkRef(b))
          p.foreach(a => assert(checkRef(a)))
        case UserFunction(_, sr, p) =>
          assert(checkRef(sr))
          p.foreach(a => assert(checkRef(a)))
        case Synthesis(_, sr, p) =>
          assert(checkRef(sr))
          p.foreach(a => assert(checkRef(a)))
        case Constructor(_, sr, p) =>
          assert(checkRef(sr))
          p.foreach(a => assert(checkRef(a)))
        case Selector(_, sr) =>
          assert(checkRef(sr))
        case DataType(_, p) =>
          p.foreach(a => assert(checkRef(a)))
        case Module(_, d, in, x, s) =>
          assert(checkRef(d))
          assert(checkRef(in))
          assert(checkRef(x))
          s.foreach(v => assert(checkRef(v)))
        case Application(function, p) =>
          assert(checkRef(function))
          p.foreach(a => assert(checkRef(a)))
      }
    }

    None
  }

  def inferTermType(
    app: Int
  ): Int =
    var sort = getStmt(app) match {
      case Application(function, args) =>
        getStmt(function) match {
          case TheoryMacro("ite", _) => inferTermType(args.head)
          case TheoryMacro("store", _) =>
            inferTermType(args.head)
          case TheoryMacro("select", _) =>
            val arrayRef = inferTermType(args.head)
            val arraySort =
              getStmt(arrayRef).asInstanceOf[TheorySort]
            arraySort.params.last
          case _ => inferTermType(function)
        }
      case Constructor(_, sort, _)    => sort
      case FunctionParameter(_, sort) => sort
      case Selector(_, sort)          => sort
      case UserMacro(_, sort, _, _)   => sort
      case UserFunction(_, sort, _)   => sort
      case Ref(loc)                => inferTermType(loc)
      case _ =>
        throw new IllegalArgumentException(
          s"type inference not yet supported: ${getStmt(app)}"
        )
    }

    findTarget(sort)
}
