package com.uclid.termgraph

import com.uclid.error._

import scala.collection.mutable.ArrayBuffer

/*
Passing semantic checks must gurantee a well formed query
 */

def checkRefBounds(): Unit = {
  def checkRef(r: Int): Boolean = r < stmts.length && r >= 0

  stmts.zipWithIndex.foreach {
    case (s, i) => {
      val err = s"${i}: ${s} >>> REFERENCE OUT OF BOUNDS!"
      s match {
        case r: Ref =>
          if (!checkRef(r.loc)) throw new RefOutOfBoundsError(err)
        case Numeral(_) =>
        case TheorySort(_, p) =>
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        case UserSort(_, _) =>
        case FunctionParameter(_, sr) =>
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
        case TheoryMacro(_, p) =>
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        case UserMacro(_, sr, b, p) => {
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          if (!checkRef(b)) throw new RefOutOfBoundsError(err)
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        }
        case UserFunction(_, sr, p) => {
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        }
        case Synthesis(_, sr, p) => {
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        }
        case Constructor(_, sr, p) => {
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        }
        case Selector(_, sr) =>
          if (!checkRef(sr)) throw new RefOutOfBoundsError(err)
        case DataType(_, p) =>
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        case Module(_, d, in, x, v) => {
          if (!checkRef(d)) throw new RefOutOfBoundsError(err)
          if (!checkRef(in)) throw new RefOutOfBoundsError(err)
          if (!checkRef(x)) throw new RefOutOfBoundsError(err)
          if (!checkRef(v)) throw new RefOutOfBoundsError(err)
        }
        case Application(caller, p) => {
          if (!checkRef(caller)) throw new RefOutOfBoundsError(err)
          p.foreach(a => if (!checkRef(a)) throw new RefOutOfBoundsError(err))
        }
      }
    }
  }

  None
}

protected def inferTermType(
  app: Int
): Int =
  stmts(app) match {
    case Application(caller, args) => {
      stmts(caller) match {
        case TheoryMacro("ite", _) => inferTermType(args.head)
        case TheoryMacro("store", _) =>
          inferTermType(args.head)
        case TheoryMacro("select", _) => {
          val arrayRef = inferTermType(args.head)
          val arraySort =
            stmts(arrayRef).asInstanceOf[TheorySort]
          arraySort.params.last
        }
        case _ => inferTermType(caller)
      }
    }
    case Constructor(_, sort, _)    => sort
    case FunctionParameter(_, sort) => sort
    case Selector(_, sort)          => sort
    case UserMacro(_, sort, _, _)   => sort
    case UserFunction(_, sort, _)   => sort
    case Ref(loc, _)                => inferTermType(loc)
    case _ =>
      throw new IllegalArgumentException(
        s"type inference not yet supported: ${stmts(app)}"
      )
  }
