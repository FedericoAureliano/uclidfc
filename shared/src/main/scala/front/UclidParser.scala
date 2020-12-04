package front

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

import scala.language.implicitConversions
import scala.collection.mutable

/** This is a re-implementation of the Scala libraries StdTokenParsers with StdToken replaced by UclidToken. */
trait UclidTokenParsers extends TokenParsers {
  type Tokens <: UclidTokens
  import lexical.{
    BitVectorLit,
    BitVectorTypeLit,
    Identifier,
    IntegerLit,
    Keyword,
    StringLit
  }

  protected val keywordCache = mutable.HashMap[String, Parser[String]]()

  /** A parser which matches a single keyword token.
    *
    * @param chars    The character string making up the matched keyword.
    * @return a `Parser` that matches the given string
    */
  implicit def keyword(chars: String): Parser[String] =
    keywordCache.getOrElseUpdate(chars, accept(Keyword(chars)) ^^ (_.chars))

  /** A parser which matches an integer literal */
  def integerLitParser: Parser[IntegerLit] =
    elem("integer", _.isInstanceOf[IntegerLit]) ^^ (_.asInstanceOf[IntegerLit])

  /** A parser which matches a bitvector type */
  def bitVectorTypeParser: Parser[BitVectorTypeLit] =
    elem("bitvector type", _.isInstanceOf[BitVectorTypeLit]) ^^ {
      _.asInstanceOf[BitVectorTypeLit]
    }

  /** A parser which matches a bitvector literal */
  def bitvectorLitParser: Parser[BitVectorLit] =
    elem("bitvector", _.isInstanceOf[BitVectorLit]) ^^ (_.asInstanceOf[
      BitVectorLit
    ])

  /** A parser which matches a string literal */
  def stringLitParser: Parser[String] =
    elem("string literal", _.isInstanceOf[StringLit]) ^^ (_.chars)

  /** A parser which matches an identifier */
  def identParser: Parser[String] =
    elem("identifier", _.isInstanceOf[Identifier]) ^^ (_.chars)
}

object UclidParser extends UclidTokenParsers with PackratParsers {
  type Tokens = UclidTokens
  val lexical = new UclidLexical

  // an implicit keyword function that gives a warning when a given word is not in the reserved/delimiters list
  override implicit def keyword(chars: String): Parser[String] =
    if (lexical.reserved.contains(chars) || lexical.delimiters.contains(chars))
      super.keyword(chars)
    else
      failure(
        "You are trying to parse \"" + chars + "\", but it is neither contained in the delimiters list, nor in the reserved keyword list of your lexical object"
      )

  sealed class PositionedString(val str: String) extends Positional

  lazy val OpAnd = "&&"
  lazy val OpOr = "||"
  lazy val OpBvAnd = "&"
  lazy val OpBvOr = "|"
  lazy val OpBvXor = "^"
  lazy val OpBvNot = "~"
  lazy val OpAdd = "+"
  lazy val OpSub = "-"
  lazy val OpMul = "*"
  lazy val OpUMul = "*_u"
  lazy val OpBvSrem = "%"
  lazy val OpBvUrem = "%_u"
  lazy val OpBiImpl = "<==>"
  lazy val OpImpl = "==>"
  lazy val OpLT = "<"
  lazy val OpULT = "<_u"
  lazy val OpGT = ">"
  lazy val OpUGT = ">_u"
  lazy val OpLE = "<="
  lazy val OpULE = "<=_u"
  lazy val OpGE = ">="
  lazy val OpUGE = ">=_u"
  lazy val OpEQ = "=="
  lazy val OpNE = "!="
  lazy val OpConcat = "++"
  lazy val OpNeg = "-"
  lazy val OpNot = "!"
  lazy val OpMinus = "-"
  lazy val OpPrime = "'"
  lazy val KwBoolean = "boolean"
  lazy val KwInteger = "integer"
  lazy val KwString = "string"
  lazy val KwEnum = "enum"
  lazy val KwRecord = "record"
  lazy val KwAssume = "assume"
  lazy val KwAssert = "assert"
  lazy val KwHavoc = "havoc"
  lazy val KwVar = "var"
  lazy val KwSharedVar = "sharedvar"
  lazy val KwConst = "const"
  lazy val KwIf = "if"
  lazy val KwThen = "then"
  lazy val KwElse = "else"
  lazy val KwWhile = "while"
  lazy val KwType = "type"
  lazy val KwInput = "input"
  lazy val KwOutput = "output"
  lazy val KwInit = "init"
  lazy val KwNext = "next"
  lazy val KwModule = "module"
  lazy val KwFunction = "function"
  lazy val KwControl = "control"
  lazy val KwForall = "forall"
  lazy val KwExists = "exists"
  lazy val KwSynthesis = "synthesis"
  lazy val KwDefine = "define"
  lazy val KwInvariant = "invariant"
  lazy val KwProperty = "property"
  lazy val KwDefineAxiom = "axiom"
  lazy val KwEnsures = "ensures"

  lexical.delimiters ++= List(
    "(",
    ")",
    ",",
    "[",
    "]",
    "bv",
    "{",
    "}",
    ";",
    "=",
    ":",
    "::",
    ".",
    "*",
    OpAnd,
    OpOr,
    OpBvAnd,
    OpBvOr,
    OpBvXor,
    OpBvNot,
    OpAdd,
    OpSub,
    OpMul,
    OpBiImpl,
    OpImpl,
    OpLT,
    OpGT,
    OpLE,
    OpGE,
    OpULT,
    OpUGT,
    OpULE,
    OpUGE,
    OpEQ,
    OpNE,
    OpConcat,
    OpNot,
    OpMinus,
    OpPrime,
    OpBvUrem,
    OpBvSrem
  )
  lexical.reserved ++= List(
    OpAnd,
    OpOr,
    OpAdd,
    OpSub,
    OpMul,
    OpBiImpl,
    OpImpl,
    OpLT,
    OpGT,
    OpLE,
    OpGE,
    OpULT,
    OpUGT,
    OpULE,
    OpUGE,
    OpEQ,
    OpNE,
    OpBvAnd,
    OpBvOr,
    OpBvXor,
    OpBvUrem,
    OpBvSrem,
    OpBvNot,
    OpConcat,
    OpNot,
    OpMinus,
    OpPrime,
    "false",
    "true",
    "bv",
    KwBoolean,
    KwInteger,
    KwString,
    KwAssume,
    KwAssert,
    KwSharedVar,
    KwVar,
    KwHavoc,
    KwIf,
    KwThen,
    KwElse,
    KwWhile,
    KwInput,
    KwOutput,
    KwConst,
    KwModule,
    KwType,
    KwEnum,
    KwRecord,
    KwDefine,
    KwFunction,
    KwControl,
    KwInit,
    KwNext,
    KwProperty,
    KwDefineAxiom,
    KwForall,
    KwExists,
    KwSynthesis,
    KwInvariant,
    KwEnsures
  )

  lazy val ast_binary: Expr ~ String ~ Expr => Expr = {
    case x ~ OpBiImpl ~ y => OperatorApplication(IffOp(), List(x, y))
    case x ~ OpImpl ~ y   => OperatorApplication(ImplicationOp(), List(x, y))
    case x ~ OpAnd ~ y    => OperatorApplication(ConjunctionOp(), List(x, y))
    case x ~ OpOr ~ y     => OperatorApplication(DisjunctionOp(), List(x, y))
    case x ~ OpBvAnd ~ y  => OperatorApplication(BVAndOp(0), List(x, y))
    case x ~ OpBvOr ~ y   => OperatorApplication(BVOrOp(0), List(x, y))
    case x ~ OpBvXor ~ y  => OperatorApplication(BVXorOp(0), List(x, y))
    case x ~ OpBvUrem ~ y => OperatorApplication(BVUremOp(0), List(x, y))
    case x ~ OpBvSrem ~ y => OperatorApplication(BVSremOp(0), List(x, y))
    case x ~ OpLT ~ y     => OperatorApplication(LTOp(), List(x, y))
    case x ~ OpGT ~ y     => OperatorApplication(GTOp(), List(x, y))
    case x ~ OpLE ~ y     => OperatorApplication(LEOp(), List(x, y))
    case x ~ OpGE ~ y     => OperatorApplication(GEOp(), List(x, y))
    case x ~ OpULT ~ y    => OperatorApplication(BVLTUOp(0), List(x, y))
    case x ~ OpUGT ~ y    => OperatorApplication(BVGTUOp(0), List(x, y))
    case x ~ OpULE ~ y    => OperatorApplication(BVLEUOp(0), List(x, y))
    case x ~ OpUGE ~ y    => OperatorApplication(BVGEUOp(0), List(x, y))
    case x ~ OpEQ ~ y     => OperatorApplication(EqualityOp(), List(x, y))
    case x ~ OpNE ~ y     => OperatorApplication(InequalityOp(), List(x, y))
    case x ~ OpConcat ~ y => OperatorApplication(ConcatOp(), List(x, y))
    case x ~ OpAdd ~ y    => OperatorApplication(AddOp(), List(x, y))
    case x ~ OpSub ~ y    => OperatorApplication(SubOp(), List(x, y))
    case x ~ OpMul ~ y    => OperatorApplication(MulOp(), List(x, y))
  }

  lazy val LLOpParser: Parser[Operator] = positioned {
    OpLT ^^ { case _    => LTOp() } |
      OpULT ^^ { case _ => BVLTUOp(0) } |
      OpLE ^^ { case _  => LEOp() } |
      OpULE ^^ { case _ => BVLEUOp(0) }
  }

  lazy val RelOpParser: Parser[String] =
    OpGT | OpUGT | OpLT | OpULT | OpEQ | OpNE | OpGE | OpUGE | OpLE | OpULE
  lazy val UnOpParser: Parser[String] = OpNot | OpMinus

  lazy val RecordSelectOpParser: Parser[PolymorphicSelect] = positioned {
    ("." ~> IdParser) ^^ { case id => PolymorphicSelect(id) }
  }

  lazy val ArraySelectOpParser: Parser[ArraySelect] =
    ("[" ~> ExprParser ~ rep("," ~> ExprParser) <~ "]") ^^ {
      case e ~ es =>
        ArraySelect(e :: es)
    }

  lazy val ArrayStoreOpParser: Parser[ArrayUpdate] =
    ("[" ~> (ExprParser ~ rep("," ~> ExprParser) ~ ("->" ~> ExprParser)) <~ "]") ^^ {
      case e ~ es ~ r => ArrayUpdate(e :: es, r)
    }

  lazy val ConstBitVectorSliceParser: Parser[ConstBitVectorSlice] =
    positioned {
      ("[" ~> IntegerParser ~ ":" ~ IntegerParser <~ "]") ^^ {
        case x ~ ":" ~ y =>
          ConstBitVectorSlice(x.value.toInt, y.value.toInt)
      }
    }

  lazy val VarBitVectorSliceParser: Parser[VarBitVectorSlice] =
    positioned {
      ("[" ~> ExprParser ~ ":" ~ ExprParser <~ "]") ^^ {
        case x ~ ":" ~ y =>
          VarBitVectorSlice(x, y)
      }
    }

  lazy val ConstExtractOpParser: Parser[ConstExtractOp] =
    ("[" ~> IntegerParser ~ ":" ~ IntegerParser <~ "]") ^^ {
      case x ~ ":" ~ y =>
        ConstExtractOp(
          ConstBitVectorSlice(x.value.toInt, y.value.toInt)
        )
    }

  lazy val VarExtractOpParser: Parser[VarExtractOp] =
    positioned {
      ("[" ~> ExprParser ~ ":" ~ ExprParser <~ "]") ^^ {
        case x ~ ":" ~ y =>
          VarExtractOp(VarBitVectorSlice(x, y))
      }
    }

  lazy val ExtractOpParser: Parser[ExtractOp] = positioned {
    ConstExtractOpParser | VarExtractOpParser
  }

  lazy val IdParser: PackratParser[Identifier] = positioned {
    identParser ^^ { case i => Identifier(i) }
  }

  /* BEGIN Literals. */
  lazy val BoolParser: PackratParser[BoolLit] =
    positioned {
      "false" ^^ { _ =>
        BoolLit(false)
      } | "true" ^^ { _ => BoolLit(true) }
    }

  lazy val IntegerParser: PackratParser[IntLit] =
    positioned {
      integerLitParser ^^ {
        case intLit => IntLit(BigInt(intLit.chars, intLit.base))
      }
    }

  lazy val BitVectorParser: PackratParser[BitVectorLit] =
    positioned {
      bitvectorLitParser ^^ {
        case bvLit =>
          BitVectorLit(bvLit.intValue, bvLit.width)
      }
    }

  lazy val NumberParser: PackratParser[NumericLit] = positioned(
    IntegerParser | BitVectorParser
  )

  lazy val StringParser: PackratParser[StringLit] = positioned {
    stringLitParser ^^ { case stringLit => StringLit(stringLit) }
  }

  lazy val LiteralParser: PackratParser[Literal] = positioned(
    BoolParser | NumberParser | StringParser
  )

  /* END of Literals. */
  // Match quantifier patterns; but we don't want to make pattern a keyword.
  lazy val CommaSeparatedExprListParser: PackratParser[List[Expr]] =
    E1Parser ~ rep("," ~> E1Parser) ^^ { case e ~ es => e :: es }

  lazy val PatternListParser: PackratParser[List[List[Expr]]] =
    CommaSeparatedExprListParser ~ rep(";" ~> CommaSeparatedExprListParser) ^^ {
      case l ~ ls => l :: ls
    }

  lazy val PatternParser: PackratParser[(Identifier, List[List[Expr]])] =
    IdParser ~ ("[" ~> PatternListParser <~ "]") ^^ {
      case id ~ pats => (id, pats)
    }

  lazy val E1Parser: PackratParser[Expr] =
    KwForall ~> IdTypeListParser ~ PatternParser.? ~ ("::" ~> E1Parser) ^^ {
      case ids ~ pat ~ expr => {
        pat match {
          case None =>
            OperatorApplication(ForallOp(ids, List.empty), List(expr))
          case Some(p) =>
            if (p._1.name != "pattern") {
              throw new Utils.ParserError(
                "Unknown decorator: " + p._1.toString(),
                Some(p._1.pos),
                None
              )
            } else {
              OperatorApplication(ForallOp(ids, p._2), List(expr))
            }
        }
      }
    } |
      KwExists ~> IdTypeListParser ~ PatternParser.? ~ ("::" ~> E1Parser) ^^ {
        case ids ~ pat ~ expr => {
          pat match {
            case None =>
              OperatorApplication(ExistsOp(ids, List.empty), List(expr))
            case Some(p) =>
              if (p._1.name != "pattern") {
                throw new Utils.ParserError(
                  "Unknown decorator: " + p._1.toString(),
                  Some(p._1.pos),
                  None
                )
              } else {
                OperatorApplication(ExistsOp(ids, p._2), List(expr))
              }
          }
        }
      } |
      E3Parser

  /** E3Parser = E4Parser OpEquiv E3Parser | E4Parser  * */
  lazy val E3Parser: PackratParser[Expr] = positioned {
    E4Parser ~ OpBiImpl ~ E3Parser ^^ ast_binary | E4Parser
  }

  /** E4Parser = E5Parser OpImpl E4Parser | E5Parser  * */
  lazy val E4Parser: PackratParser[Expr] = positioned {
    E5Parser ~ OpImpl ~ E4Parser ^^ ast_binary | E5Parser
  }

  /** E5Parser = E6Parser <Bool_Or_Bv_Op> E5Parser | E6Parser * */
  lazy val E5Parser: PackratParser[Expr] = positioned {
    E6Parser ~ OpAnd ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpOr ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpBvAnd ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpBvOr ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpBvXor ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpBvUrem ~ E5Parser ^^ ast_binary |
      E6Parser ~ OpBvSrem ~ E5Parser ^^ ast_binary |
      E6Parser
  }

  /** E6Parser = E7Parser OpRel E7Parser | E7Parser  * */
  lazy val E6Parser: PackratParser[Expr] = positioned {
    E7Parser ~ LLOpParser ~ E7Parser ~ LLOpParser ~ E7Parser ^^ {
      case e1 ~ o1 ~ e2 ~ o2 ~ e3 => {
        OperatorApplication(
          ConjunctionOp(),
          List(
            OperatorApplication(o1, List(e1, e2)),
            OperatorApplication(o2, List(e2, e3))
          )
        )
      }
    } |
      E7Parser ~ RelOpParser ~ E7Parser ^^ ast_binary |
      E7Parser
  }

  /** E7Parser = E8Parser OpConcat E7Parser | E8Parser * */
  lazy val E7Parser: PackratParser[Expr] = positioned(
    E8Parser ~ OpConcat ~ E7Parser ^^ ast_binary | E8Parser
  )

  /** E8Parser = E9Parser OpAdd E8Parser | E9Parser * */
  lazy val E8Parser: PackratParser[Expr] = positioned(
    E9Parser ~ OpAdd ~ E8Parser ^^ ast_binary | E9Parser
  )

  /** E9Parser = E9Parser OpSub E10Parser | E10Parser * */
  lazy val E9Parser: PackratParser[Expr] = positioned(
    E10Parser ~ OpSub ~ E10Parser ^^ ast_binary | E10Parser
  )

  /** E10Parser = E11Parser OpMul E11Parser | E11Parser * */
  lazy val E10Parser: PackratParser[Expr] =
    E11Parser ~ OpMul ~ E11Parser ^^ ast_binary | E11Parser

  /** E11Parser = UnOp E12Parser | E12Parser * */
  lazy val E11Parser: PackratParser[Expr] = positioned {
    OpNeg ~> E12Parser ^^ {
      case e => OperatorApplication(UnaryMinusOp(), List(e))
    } |
      OpNot ~> E12Parser ^^ {
        case e => OperatorApplication(NegationOp(), List(e))
      } |
      OpBvNot ~> E12Parser ^^ {
        case e => OperatorApplication(BVNotOp(0), List(e))
      } |
      E12Parser
  }

  /** ExpressionSuffixes. */
  lazy val ExprSuffixParser: PackratParser[Operator] = positioned {
    ArraySelectOpParser | ArrayStoreOpParser | ExtractOpParser | RecordSelectOpParser
  }

  /** E12Parser = E12Parser (ExprList) | E12Parser ExprSuffix | E15Parser */
  lazy val E12Parser: PackratParser[Expr] = positioned {
    E12Parser ~ ExprSuffixParser ^^ {
      case e ~ es => OperatorApplication(es, List(e))
    } |
      E12Parser ~ ExprListParser ^^ { case e ~ f => FuncApplication(e, f) } |
      E15Parser
  }

  lazy val ConstArrayParser: PackratParser[ConstArray] = positioned {
    KwConst ~ "(" ~> ExprParser ~ ("," ~> TypeParser) <~ ")" ^^ {
      case (exp ~ typ) =>
        ConstArray(exp, typ)
    }
  }

  /** E15Parser = false | true | Number | ConstArray | Id FuncApplication | (Expr) * */
  lazy val E15Parser: PackratParser[Expr] = positioned {
    LiteralParser |
      "{" ~> ExprParser ~ rep("," ~> ExprParser) <~ "}" ^^ {
        case e ~ es =>
          Tuple(e :: es)
      } |
      KwIf ~> ("(" ~> ExprParser <~ ")") ~ (KwThen ~> ExprParser) ~ (KwElse ~> ExprParser) ^^ {
        case expr ~ thenExpr ~ elseExpr =>
          OperatorApplication(ITEOp(), List(expr, thenExpr, elseExpr))
      } |
      ConstArrayParser |
      "(" ~> ExprParser <~ ")" |
      IdParser <~ OpPrime ^^ {
        case id =>
          OperatorApplication(GetNextValueOp(), List(id))
      } |
      IdParser
  }

  /** Expr = E1Parser (Used to be TemporalExpr0) * */
  lazy val ExprParser: PackratParser[Expr] = positioned(
    E1Parser
  ) // Used to be TemporalExpr0
  lazy val ExprListParser: Parser[List[Expr]] =
    ("(" ~> ExprParser ~ rep("," ~> ExprParser) <~ ")") ^^ {
      case e ~ es => e :: es
    } |
      "(" ~> ")" ^^ { case _ => List.empty[Expr] }

  /** Examples of allowed types are bool | int | [int,int,bool] int * */
  lazy val PrimitiveTypeParser: PackratParser[Type] = positioned {
    KwBoolean ^^ { case _                  => BooleanType() } |
      KwInteger ^^ { case _                => IntegerType() } |
      KwString ^^ { case _                 => StringType() } |
      bitVectorTypeParser ^^ { case bvType => BitVectorType(bvType.width) }
  }

  lazy val EnumTypeParser: PackratParser[EnumType] = positioned {
    KwEnum ~> ("{" ~> IdParser) ~ rep("," ~> IdParser) <~ "}" ^^ {
      case id ~ ids =>
        EnumType(id :: ids)
    }
  }

  lazy val TupleTypeParser: PackratParser[TupleType] = positioned {
    ("{" ~> TypeParser ~ rep("," ~> TypeParser) <~ "}") ^^ {
      case t ~ ts =>
        TupleType(t :: ts)
    }
  }

  lazy val RecordTypeParser: PackratParser[RecordType] = positioned {
    KwRecord ~> ("{" ~> IdTypeParser) ~ rep("," ~> IdTypeParser) <~ "}" ^^ {
      case id ~ ids => RecordType(id :: ids)
    }
  }

  lazy val MapTypeParser: PackratParser[MapType] = positioned {
    PrimitiveTypeParser ~ rep("*" ~> PrimitiveTypeParser) ~ ("->" ~> TypeParser) ^^ {
      case t ~ ts ~ rt => MapType(t :: ts, rt)
    }
  }

  lazy val ArrayTypeParser: PackratParser[ArrayType] = positioned {
    ("[") ~> TypeParser ~ (rep("," ~> TypeParser) <~ "]") ~ TypeParser ^^ {
      case t ~ ts ~ rt =>
        ArrayType(t :: ts, rt)
    }
  }

  // also handles module instance types
  lazy val SynonymTypeParser: PackratParser[SynonymType] = positioned(
    IdParser ^^ {
      case id => SynonymType(id)
    }
  )

  lazy val ExternalTypeParser: PackratParser[ExternalType] = positioned {
    IdParser ~ ("." ~> IdParser) ^^ {
      case moduleId ~ typeId =>
        ExternalType(moduleId, typeId)
    }
  }

  lazy val TypeParser: PackratParser[Type] = positioned {
    MapTypeParser | ArrayTypeParser | EnumTypeParser | TupleTypeParser | RecordTypeParser | ExternalTypeParser | SynonymTypeParser | PrimitiveTypeParser
  }

  lazy val IdTypeParser: PackratParser[(Identifier, Type)] =
    IdParser ~ (":" ~> TypeParser) ^^ { case id ~ typ => (id, typ) }

  lazy val IdsTypeParser: PackratParser[List[(Identifier, Type)]] =
    IdListParser ~ (":" ~> TypeParser) ^^ {
      case ids ~ typ => (ids.map((_, typ)))
    }

  lazy val IdTypeListParser: PackratParser[List[(Identifier, Type)]] =
    "(" ~> IdsTypeParser ~ (rep("," ~> IdsTypeParser) <~ ")") ^^ {
      case t ~ ts =>
        t ++ ts.flatMap(v => v)
    } |
      "(" ~ ")" ^^ { case _ ~ _ => List.empty[(Identifier, Type)] }

  lazy val LhsParser: PackratParser[Lhs] = positioned {
    IdParser ~ VarBitVectorSliceParser ^^ {
      case id ~ slice =>
        LhsVarSliceSelect(id, slice)
    } |
      IdParser ~ ArraySelectOpParser ^^ {
        case id ~ mapOp =>
          LhsArraySelect(id, mapOp.indices)
      } |
      IdParser ~ RecordSelectOpParser ~ rep(RecordSelectOpParser) ^^ {
        case id ~ rOp ~ rOps =>
          LhsRecordSelect(id, (rOp.id) :: (rOps.map(_.id)))
      } |
      IdParser <~ OpPrime ^^ { case id => LhsId(id) }
  }

  lazy val LhsListParser: PackratParser[List[Lhs]] =
    ("(" ~> LhsParser ~ rep("," ~> LhsParser) <~ ")") ^^ {
      case l ~ ls => l :: ls
    } |
      "(" ~> ")" ^^ { case _ => List.empty[Lhs] }

  lazy val IdListParser: PackratParser[List[Identifier]] =
    IdParser ~ rep("," ~> IdParser) ^^ { case id ~ ids => id :: ids }

  lazy val BlockVarsDeclParser: PackratParser[BlockVarsDecl] = positioned {
    KwVar ~> IdListParser ~ (":" ~> TypeParser) <~ ";" ^^ {
      case ids ~ typ =>
        BlockVarsDecl(ids, typ)
    }
  }

  lazy val InvariantParser: PackratParser[Expr] = positioned {
    KwInvariant ~> ExprParser <~ ";"
  }

  lazy val StatementParser: PackratParser[Statement] = positioned {
    KwAssert ~> ExprParser <~ ";" ^^ { case e   => AssertStmt(e, None) } |
      KwAssume ~> ExprParser <~ ";" ^^ { case e => AssumeStmt(e, None) } |
      KwHavoc ~> IdParser <~ ";" ^^ { case id   => HavocStmt(id) } |
      LhsParser ~ rep("," ~> LhsParser) ~ "=" ~ ExprParser ~ rep(
        "," ~> ExprParser
      ) <~ ";" ^^ {
        case l ~ ls ~ "=" ~ r ~ rs => AssignStmt(l :: ls, r :: rs)
      } |
      KwNext ~ "(" ~> IdParser <~ ")" ~ ";" ^^ {
        case id =>
          ModuleCallStmt(id)
      } |
      KwIf ~ "(" ~ "*" ~ ")" ~> (BlkStmtParser <~ KwElse) ~ BlkStmtParser ^^ {
        case tblk ~ fblk =>
          IfElseStmt(FreshLit(BooleanType()), tblk, fblk)
      } |
      KwIf ~ "(" ~ "*" ~ ")" ~> BlkStmtParser ^^ {
        case blk =>
          IfElseStmt(
            FreshLit(BooleanType()),
            blk,
            BlockStmt(List.empty, List.empty)
          )
      } |
      KwIf ~ "(" ~> (ExprParser <~ ")") ~ BlkStmtParser ~ (KwElse ~> BlkStmtParser) ^^ {
        case e ~ f ~ g => IfElseStmt(e, f, g)
      } |
      KwIf ~> (ExprParser ~ BlkStmtParser) ^^ {
        case e ~ f =>
          IfElseStmt(e, f, BlockStmt(List.empty, List.empty))
      } |
      BlkStmtParser |
      ";" ^^ { case _ => SkipStmt() }
  }

  lazy val BlkStmtParser: PackratParser[BlockStmt] =
    "{" ~> rep(BlockVarsDeclParser) ~ rep(StatementParser) <~ "}" ^^ {
      case vars ~ stmts =>
        BlockStmt(vars, stmts)
    }

  lazy val OptionalExprParser: PackratParser[Option[Expr]] =
    "(" ~ ")" ^^ { case _                     => None } |
      "(" ~> ExprParser <~ ")" ^^ { case expr => Some(expr) }

  lazy val ArgMapParser: PackratParser[(Identifier, Option[Expr])] =
    IdParser ~ ":" ~ OptionalExprParser ^^ {
      case id ~ ":" ~ optExpr => (id, optExpr)
    }

  lazy val ArgMapListParser: PackratParser[List[(Identifier, Option[Expr])]] =
    "(" ~ ")" ^^ { case _ => List.empty } |
      "(" ~> ArgMapParser ~ rep("," ~> ArgMapParser) <~ ")" ^^ {
        case arg ~ args =>
          arg :: args
      }

  lazy val TypeDeclParser: PackratParser[TypeDecl] = positioned {
    KwType ~> IdParser ~ ("=" ~> TypeParser) <~ ";" ^^ {
      case id ~ t =>
        TypeDecl(id, t)
    } |
      KwType ~> IdParser <~ ";" ^^ {
        case id =>
          TypeDecl(id, UninterpretedType(id))
      }
  }

  lazy val ModuleTypesImportDeclParser: PackratParser[ModuleTypesImportDecl] =
    positioned {
      KwType ~ "*" ~ "=" ~> IdParser <~ "." ~ "*" ~ ";" ^^ {
        case id =>
          ModuleTypesImportDecl(id)
      }
    }

  lazy val VarsDeclParser: PackratParser[StateVarsDecl] = positioned {
    KwVar ~> IdListParser ~ ":" ~ TypeParser <~ ";" ^^ {
      case ids ~ ":" ~ typ =>
        StateVarsDecl(ids, typ)
    }
  }

  lazy val InputsDeclParser: PackratParser[InputVarsDecl] = positioned {
    KwInput ~> IdListParser ~ ":" ~ TypeParser <~ ";" ^^ {
      case ids ~ ":" ~ typ =>
        InputVarsDecl(ids, typ)
    }
  }

  lazy val OutputsDeclParser: PackratParser[OutputVarsDecl] = positioned {
    KwOutput ~> IdListParser ~ ":" ~ TypeParser <~ ";" ^^ {
      case ids ~ ":" ~ typ =>
        OutputVarsDecl(ids, typ)
    }
  }

  lazy val SharedVarsDeclParser: PackratParser[SharedVarsDecl] = positioned {
    KwSharedVar ~> IdListParser ~ ":" ~ TypeParser <~ ";" ^^ {
      case ids ~ ":" ~ typ =>
        SharedVarsDecl(ids, typ)
    }
  }

  lazy val ConstLitDeclParser: PackratParser[ConstantLitDecl] = positioned {
    KwConst ~> IdParser ~ (":" ~ TypeParser ~ "=" ~> NumberParser) <~ ";" ^^ {
      case id ~ lit =>
        ConstantLitDecl(id, lit)
    } |
      KwConst ~> IdParser ~ (":" ~ TypeParser ~ "=" ~ OpNeg ~> NumberParser) <~ ";" ^^ {
        case id ~ lit => ConstantLitDecl(id, lit.negate)
      }
  }

  lazy val ConstDeclParser: PackratParser[ConstantsDecl] = positioned {
    KwConst ~> IdListParser ~ ":" ~ TypeParser <~ ";" ^^ {
      case ids ~ ":" ~ typ =>
        ConstantsDecl(ids, typ)
    }
  }

  lazy val ModuleConstsImportDeclParser
    : PackratParser[ModuleConstantsImportDecl] = positioned {
    KwConst ~ "*" ~ "=" ~> IdParser <~ "." ~ "*" ~ ";" ^^ {
      case id =>
        ModuleConstantsImportDecl(id)
    }
  }

  lazy val FuncDeclParser: PackratParser[FunctionDecl] = positioned {
    KwFunction ~> IdParser ~ IdTypeListParser ~ (":" ~> TypeParser) <~ ";" ^^ {
      case id ~ idtyps ~ rt =>
        FunctionDecl(id, FunctionSig(idtyps, rt))
    }
  }

  lazy val ModuleFuncsImportDeclParser
    : PackratParser[ModuleFunctionsImportDecl] = positioned {
    KwFunction ~ "*" ~ "=" ~> IdParser <~ "." ~ "*" ~ ";" ^^ {
      case id =>
        ModuleFunctionsImportDecl(id)
    }
  }

  lazy val SynthFuncDeclParser: PackratParser[SynthesisFunctionDecl] =
    positioned {
      KwSynthesis ~ KwFunction ~> IdParser ~ IdTypeListParser ~ (":" ~> TypeParser) ~
        rep(KwEnsures ~> ExprParser) <~ ";" ^^ {
        case id ~ idtyps ~ rt ~ ensures =>
          SynthesisFunctionDecl(
            id,
            FunctionSig(idtyps, rt),
            None,
            List.empty,
            ensures
          )
      }
    }

  lazy val DefineDeclParser: PackratParser[DefineDecl] = positioned {
    KwDefine ~> IdParser ~ IdTypeListParser ~ (":" ~> TypeParser) ~ ("=" ~> ExprParser) <~ ";" ^^ {
      case id ~ idTypeList ~ retType ~ expr => {
        DefineDecl(id, FunctionSig(idTypeList, retType), expr)
      }
    }
  }

  lazy val ModuleDefsImportDeclParser: PackratParser[ModuleDefinesImportDecl] =
    positioned {
      KwDefine ~ "*" ~ "=" ~> IdParser <~ "." ~ "*" ~ ";" ^^ {
        case id =>
          ModuleDefinesImportDecl(id)
      }
    }

  lazy val InitDeclParser: PackratParser[InitDecl] = positioned {
    KwInit ~> BlkStmtParser ^^ { case b => InitDecl(b) }
  }

  lazy val NextDeclParser: PackratParser[NextDecl] = positioned {
    KwNext ~> BlkStmtParser ^^ { case b => NextDecl(b) }
  }

  lazy val SpecDeclParser: PackratParser[SpecDecl] = positioned {
    (KwInvariant | KwProperty) ~> ("[" ~> rep(
      ExprParser
    ) <~ "]").? ~ IdParser ~ (":" ~> ExprParser) <~ ";" ^^ {
      case decOption ~ id ~ expr =>
        decOption match {
          case None => SpecDecl(id, expr)
          case Some(dec) =>
            SpecDecl(id, expr)
        }
    }
  }

  lazy val AxiomDeclParser: PackratParser[AxiomDecl] = positioned {
    (KwAssume | KwDefineAxiom) ~> IdParser ~ (":" ~> ExprParser) <~ ";" ^^ {
      case id ~ expr => AxiomDecl(Some(id), expr)
    } |
      (KwAssume | KwDefineAxiom) ~> ExprParser <~ ";" ^^ {
        case expr =>
          AxiomDecl(None, expr)
      }
  }

  lazy val DeclParser: PackratParser[Decl] =
    positioned(
      TypeDeclParser | ConstDeclParser | FuncDeclParser |
        ModuleTypesImportDeclParser | ModuleFuncsImportDeclParser | ModuleConstsImportDeclParser |
        SynthFuncDeclParser | DefineDeclParser | ModuleDefsImportDeclParser |
        VarsDeclParser | InputsDeclParser | OutputsDeclParser | SharedVarsDeclParser |
        ConstLitDeclParser | ConstDeclParser |
        InitDeclParser | NextDeclParser | SpecDeclParser | AxiomDeclParser
    )

  // control commands.
  lazy val CmdParamParser: PackratParser[CommandParams] =
    // TODO: Current fix to allow for logic to be specified for synthesize invariant
    (IdParser <~ "=") ~ ("[" ~> ExprParser ~ rep("," ~> ExprParser) <~ "]") ^^ {
      case id ~ (e ~ es) => CommandParams(id, e :: es)
    } |
      (IdParser) ^^ { case id => CommandParams(id, List.empty) }

  lazy val CmdParamListParser: PackratParser[List[CommandParams]] =
    "[" ~ "]" ^^ { case _ => List.empty } |
      "[" ~> CmdParamParser ~ rep(";" ~> CmdParamParser) <~ "]" ^^ {
        case p ~ ps =>
          p :: ps
      }

  lazy val CmdParser: PackratParser[GenericProofCommand] = positioned {
    (IdParser <~ "=").? ~ (IdParser <~ ".").? ~ IdParser <~ ";" ^^ {
      case rId ~ oId ~ id =>
        GenericProofCommand(id, List.empty, List.empty, rId, oId)
    } |
      (IdParser <~ "=").? ~ (IdParser <~ ".").? ~ IdParser ~ CmdParamListParser <~ ";" ^^ {
        case rId ~ oId ~ id ~ cmdParams =>
          GenericProofCommand(id, cmdParams, List.empty, rId, oId)
      } |
      (IdParser <~ "=").? ~ (IdParser <~ ".").? ~ IdParser ~ ExprListParser <~ ";" ^^ {
        case rId ~ oId ~ id ~ es =>
          GenericProofCommand(
            id,
            List.empty,
            es.map(e => (e, e.toString())),
            rId,
            oId
          )
      } |
      (IdParser <~ "=").? ~ (IdParser <~ ".").? ~ IdParser ~ CmdParamListParser ~ ExprListParser <~ ";" ^^ {
        case rId ~ oId ~ id ~ cmdParams ~ es =>
          GenericProofCommand(
            id,
            cmdParams,
            es.map(e => (e, e.toString())),
            rId,
            oId
          )
      }
  }

  lazy val CmdBlockParser: PackratParser[List[GenericProofCommand]] =
    KwControl ~ "{" ~> rep(CmdParser) <~ "}"

  lazy val ModuleParser: PackratParser[Module] = positioned {
    KwModule ~> IdParser ~ ("{" ~> rep(DeclParser) ~ (CmdBlockParser.?) <~ "}") ^^ {
      case id ~ (decls ~ Some(cs)) =>
        Module(id, decls, cs)
      case id ~ (decls ~ None) =>
        Module(id, decls, List.empty)
    }
  }

  lazy val ModelParser: PackratParser[List[Module]] = rep(ModuleParser)

  def parseModel(filename: String, text: String): List[Module] = {
    val tokens = new PackratReader(new lexical.Scanner(text))
    phrase(ModelParser)(tokens) match {
      case Success(modules, _) => modules.map((m) => m.withFilename(filename))
      case NoSuccess(msg, next) =>
        throw new Utils.SyntaxError(msg, Some(next.pos), Some(filename))
    }
  }
}
