package middle

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

object Rewriter {

  def incrementRefs(term: Program, increment: Int): Unit =
    (0 until term.stmts.length).foreach { i =>
      incrementInstructionRefs(term.stmts(i), increment) match {
        case Some(instruction) => term.stmts(i) = instruction
        case None              =>
      }
    }

  def incrementInstructionRefs(
    s: Instruction,
    increment: Int
  ): Option[Instruction] = {
    def updateParams(params: List[Ref]): List[Ref] =
      params.map(p => Ref(p.loc + increment))
    s match {
      case Ref(i)           => Some(Ref(i + increment))
      case Numeral(_)       => None
      case TheorySort(n, p) => Some(TheorySort(n, updateParams(p)))
      case SortMacro(n, b)  => Some(SortMacro(n, Ref(b.loc + increment)))
      case SortParameter(_) => None
      case UserSort(n, a)   => None
      case FunctionParameter(n, s) =>
        Some(FunctionParameter(n, Ref(s.loc + increment)))
      case TheoryMacro(n, p) => Some(TheoryMacro(n, updateParams(p)))
      case UserMacro(n, s, b, p) =>
        Some(
          UserMacro(
            n,
            Ref(s.loc + increment),
            Ref(b.loc + increment),
            updateParams(p)
          )
        )
      case UserFunction(n, s, p) =>
        Some(UserFunction(n, Ref(s.loc + increment), updateParams(p)))
      case Constructor(n, s, p) =>
        Some(Constructor(n, Ref(s.loc + increment), updateParams(p)))
      case Selector(n, s) => Some(Selector(n, Ref(s.loc + increment)))
      case DataType(n, p) => Some(DataType(n, updateParams(p)))
      case Module(n, d, i, x, v) =>
        Some(
          Module(
            n,
            Ref(d.loc + increment),
            Ref(i.loc + increment),
            Ref(x.loc + increment),
            Ref(v.loc + increment)
          )
        )
      case Application(c, args) =>
        Some(
          Application(
            Ref(c.loc + increment),
            args.map(r => Ref(r.loc + increment))
          )
        )
    }
  }

  def updateRefs(term: Program, newLocations: Int => Int): Unit =
    (0 until term.stmts.length).foreach { i =>
      updateInstructionRef(term.stmts(i), newLocations) match {
        case Some(instruction) => term.stmts(i) = instruction
        case None              =>
      }
    }

  def updateInstructionRef(
    instruction: Instruction,
    newLocations: Int => Int
  ): Option[Instruction] =
    instruction match {
      case Ref(i)     => Some(Ref(newLocations(i)))
      case Numeral(_) => None
      case TheorySort(n, p) =>
        Some(TheorySort(n, p.map(p => Ref(newLocations(p.loc)))))
      case SortMacro(n, b)  => Some(SortMacro(n, Ref(newLocations(b.loc))))
      case SortParameter(_) => None
      case UserSort(n, a)   => None
      case FunctionParameter(n, s) =>
        Some(FunctionParameter(n, Ref(newLocations(s.loc))))
      case TheoryMacro(n, p) =>
        Some(TheoryMacro(n, p.map(p => Ref(newLocations(p.loc)))))
      case UserMacro(n, s, b, p) =>
        Some(
          UserMacro(
            n,
            Ref(newLocations(s.loc)),
            Ref(newLocations(b.loc)),
            p.map(p => Ref(newLocations(p.loc)))
          )
        )
      case UserFunction(n, s, p) =>
        Some(
          UserFunction(
            n,
            Ref(newLocations(s.loc)),
            p.map(p => Ref(newLocations(p.loc)))
          )
        )
      case Constructor(n, s, p) =>
        Some(
          Constructor(
            n,
            Ref(newLocations(s.loc)),
            p.map(p => Ref(newLocations(p.loc)))
          )
        )
      case Selector(n, s) => Some(Selector(n, Ref(newLocations(s.loc))))
      case DataType(n, p) =>
        Some(DataType(n, p.map(p => Ref(newLocations(p.loc)))))
      case Module(n, d, i, x, v) =>
        Some(
          Module(
            n,
            Ref(newLocations(d.loc)),
            Ref(newLocations(i.loc)),
            Ref(newLocations(x.loc)),
            Ref(newLocations(v.loc))
          )
        )
      case Application(c, a) =>
        Some(
          Application(
            Ref(newLocations(c.loc)),
            a.map(s => Ref(newLocations(s.loc)))
          )
        )
    }

  def copySubTerm(term: Program, position: Int): Program = {
    val toKeep = Garbage.mark_i(term, position)
    Garbage.sweep(term, toKeep)
  }

  def inlineApplication(term: Program, position: Int): Program = {
    // get the application
    val ap = term.stmts(position) match {
      case Application(_, _) => term.stmts(position).asInstanceOf[Application]
      case _ =>
        throw new IllegalArgumentException(
          s"must have an application: ${position}\n${term}"
        )
    }

    // copy the macro
    val caller = copySubTerm(term, ap.caller.loc)

    // find the UserMacro in caller (must be first instruction)
    val defi = caller.stmts(0) match {
      case UserMacro(_, _, _, _) => caller.stmts(0).asInstanceOf[UserMacro]
      case _ =>
        throw new IllegalArgumentException(
          s"must have a user macro: ${0}\n${caller}"
        )
    }

    val callerBody = copySubTerm(caller, defi.body.loc)

    // copy the args and update their references
    var offset = callerBody.stmts.length
    val args = ap.args.map { r =>
      val cp = copySubTerm(term, r.loc)
      incrementRefs(
        cp,
        offset + term.stmts.length
      ) // add term length since we will combine it at the end
      offset += cp.stmts.length
      cp
    }

    // replace the parameters of callerBody with pointers to the args
    offset = callerBody.stmts.length
    defi.params.zipWithIndex.foreach {
      case (p, i) => {
        callerBody.stmts(p.loc - 1) = Ref(offset) // -1 since we removed the head
        offset += args(i).stmts.length
      }
    }

    incrementRefs(
      callerBody,
      term.stmts.length
    ) // couldn't do this before like we did with args because of params

    var finalBuffer = new ArrayBuffer[Instruction]()
    finalBuffer.addAll(term.stmts)

    // replace the application with a reference to the inlined stuff
    finalBuffer(position) = Ref(term.stmts.length)

    // add in the inlined stuff
    finalBuffer.addAll(callerBody.stmts)
    args.foreach(a => finalBuffer.addAll(a.stmts))

    new Program(finalBuffer, 0)
  }

  def letify(term: Program, prefix: String): Program = {

    // find every application
    var newMacros = new ArrayBuffer[Instruction]()
    var newLocations = new ListBuffer[(Int, Int)]()
    var count = term.stmts.length
    term.stmts.zipWithIndex.foreach {
      case (s, i) => {
        s match {
          case Application(c, a) => {
            // point anything that used to point to this app to a new user macro
            newLocations.addOne((i, count))
            count += 1
            val sortRef = Checker.inferSort(term, i) match {
              case Ref(r) => Ref(r)
              case TheorySort(n, args) => {
                // the application is a theory function so we add it's sort
                val index = term.stmts.indexOf(TheorySort(n, args), 0)
                if (index >= 0) {
                  Ref(index)
                } else {
                  newMacros.addOne(TheorySort(n, args))
                  val ret = Ref(count)
                  count += 1
                  ret
                }
              }
              case _ =>
                throw new IllegalArgumentException(
                  "must be a reference to a sort or a theory sort"
                )
            }
            newMacros.addOne(
              UserMacro(
                s"${prefix}_${i}_to_${count - 1}",
                sortRef,
                Ref(i),
                List()
              )
            )
          }
          case _ =>
        }
      }
    }

    val newLocationMap: Map[Int, Int] = newLocations.toMap

    val tmpTerm = new Program(term.stmts.clone(), 0)
    updateRefs(
      tmpTerm,
      (x: Int) => if (newLocationMap contains x) newLocationMap(x) else x
    )

    val newTerm = new Program(tmpTerm.stmts ++ newMacros, 0)

    newTerm
  }

  def reduceDuplicates(term: Program): Unit =
    // for every statement, if there is an equal statement later in the array, just point to it.
    (0 until term.stmts.length).foreach { i =>
      // don't do anything for standalone references since this will just increase indirection
      term.stmts(i) match {
        case Ref(_) =>
        case _ => {
          val index = term.stmts.indexOf(term.stmts(i), i + 1)
          if (index >= 0) {
            term.stmts(i) = Ref(index)
          }
        }
      }
    }

  def reduceIndirection(term: Program): Unit = {
    // if we have a pointer chain then just cut out the middle pointers
    def findTarget(candidate: Instruction, position: Int): Ref =
      candidate match {
        case Ref(j) => findTarget(term.stmts(j), j)
        case _      => Ref(position)
      }

    (0 until term.stmts.length).foreach { i =>
      term.stmts(i) match {
        case Ref(j)     => term.stmts(i) = findTarget(term.stmts(j), j)
        case Numeral(_) =>
        case TheorySort(n, p) =>
          term.stmts(i) =
            TheorySort(n, p.map(r => findTarget(term.stmts(r.loc), r.loc)))
        case SortMacro(n, b) =>
          SortMacro(n, findTarget(term.stmts(b.loc), b.loc))
        case SortParameter(_) =>
        case UserSort(n, a)   =>
        case FunctionParameter(n, s) =>
          term.stmts(i) =
            FunctionParameter(n, findTarget(term.stmts(s.loc), s.loc))
        case TheoryMacro(n, p) =>
          term.stmts(i) =
            TheoryMacro(n, p.map(r => findTarget(term.stmts(r.loc), r.loc)))
        case UserMacro(n, s, b, p) =>
          term.stmts(i) = UserMacro(
            n,
            findTarget(term.stmts(s.loc), s.loc),
            findTarget(term.stmts(b.loc), b.loc),
            p.map(r => findTarget(term.stmts(r.loc), r.loc))
          )
        case UserFunction(n, s, p) =>
          term.stmts(i) = UserFunction(
            n,
            findTarget(term.stmts(s.loc), s.loc),
            p.map(r => findTarget(term.stmts(r.loc), r.loc))
          )
        case Constructor(n, s, p) =>
          term.stmts(i) = Constructor(
            n,
            findTarget(term.stmts(s.loc), s.loc),
            p.map(r => findTarget(term.stmts(r.loc), r.loc))
          )
        case Selector(n, s) =>
          term.stmts(i) = Selector(n, findTarget(term.stmts(s.loc), s.loc))
        case DataType(n, p) =>
          term.stmts(i) =
            DataType(n, p.map(r => findTarget(term.stmts(r.loc), r.loc)))
        case Module(n, d, in, x, v) =>
          term.stmts(i) = Module(
            n,
            findTarget(term.stmts(d.loc), d.loc),
            findTarget(term.stmts(in.loc), in.loc),
            findTarget(term.stmts(x.loc), x.loc),
            findTarget(term.stmts(v.loc), v.loc)
          )
        case Application(c, p) =>
          term.stmts(i) = Application(
            findTarget(term.stmts(c.loc), c.loc),
            p.map(r => findTarget(term.stmts(r.loc), r.loc))
          )
      }
    }
  }
}
