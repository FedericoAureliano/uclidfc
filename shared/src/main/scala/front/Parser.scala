package front

import scala.util.parsing.input.Positional
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

import scala.language.implicitConversions
import scala.collection.mutable

/** This is a re-implementation of the Scala libraries StdTokenParsers with StdToken replaced by UclidToken. */
trait UclidTokenParsers extends TokenParsers {
  type Tokens <: UclidTokens
  import lexical.{Identifier, IntegerLit, Keyword}

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

  val OpAnd = "&&"
  val OpOr = "||"
  val OpAdd = "+"
  val OpSub = "-"
  val OpMul = "*"
  val OpBiImpl = "<==>"
  val OpImpl = "==>"
  val OpLT = "<"
  val OpGT = ">"
  val OpLE = "<="
  val OpGE = ">="
  val OpEQ = "=="
  val OpNE = "!="
  val OpNeg = "-"
  val OpNot = "!"
  val OpMinus = "-"
  val OpPrime = "'"
  val KwBoolean = "boolean"
  val KwInteger = "integer"
  val KwAssume = "assume"
  val KwAssert = "assert"
  val KwHavoc = "havoc"
  val KwVar = "var"
  val KwSharedVar = "sharedvar"
  val KwConst = "const"
  val KwIf = "if"
  val KwThen = "then"
  val KwElse = "else"
  val KwType = "type"
  val KwInput = "input"
  val KwOutput = "output"
  val KwInit = "init"
  val KwNext = "next"
  val KwModule = "module"
  val KwControl = "control"
  val KwInvariant = "invariant"
  val KwCase = "case"
  val KwEsac = "esac"
  val KwDefault = "default"

  lexical.delimiters ++= List(
    "(",
    ")",
    ",",
    "[",
    "]",
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
    OpAdd,
    OpSub,
    OpMul,
    OpBiImpl,
    OpImpl,
    OpLT,
    OpGT,
    OpLE,
    OpGE,
    OpEQ,
    OpNE,
    OpNot,
    OpMinus,
    OpPrime
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
    OpEQ,
    OpNE,
    OpNot,
    OpMinus,
    OpPrime,
    "false",
    "true",
    KwBoolean,
    KwInteger,
    KwAssume,
    KwAssert,
    KwSharedVar,
    KwVar,
    KwHavoc,
    KwIf,
    KwThen,
    KwElse,
    KwInput,
    KwOutput,
    KwConst,
    KwModule,
    KwType,
    KwControl,
    KwInit,
    KwNext,
    KwInvariant,
    KwCase,
    KwEsac,
    KwDefault
  )

  lazy val ast_binary: Expr ~ String ~ Expr => Expr = {
    case x ~ OpBiImpl ~ y => OperatorApplication(IffOp(), List(x, y))
    case x ~ OpImpl ~ y   => OperatorApplication(ImplicationOp(), List(x, y))
    case x ~ OpAnd ~ y    => OperatorApplication(ConjunctionOp(), List(x, y))
    case x ~ OpOr ~ y     => OperatorApplication(DisjunctionOp(), List(x, y))
    case x ~ OpLT ~ y     => OperatorApplication(LTOp(), List(x, y))
    case x ~ OpGT ~ y     => OperatorApplication(GTOp(), List(x, y))
    case x ~ OpLE ~ y     => OperatorApplication(LEOp(), List(x, y))
    case x ~ OpGE ~ y     => OperatorApplication(GEOp(), List(x, y))
    case x ~ OpEQ ~ y     => OperatorApplication(EqualityOp(), List(x, y))
    case x ~ OpNE ~ y     => OperatorApplication(InequalityOp(), List(x, y))
    case x ~ OpAdd ~ y    => OperatorApplication(AddOp(), List(x, y))
    case x ~ OpSub ~ y    => OperatorApplication(SubOp(), List(x, y))
    case x ~ OpMul ~ y    => OperatorApplication(MulOp(), List(x, y))
  }

  lazy val LLOpParser: Parser[Operator] = positioned {
    OpLT ^^ { case _   => LTOp() } |
      OpLE ^^ { case _ => LEOp() }
  }

  lazy val RelOpParser: Parser[String] =
    OpGT | OpLT | OpEQ | OpNE | OpGE | OpLE
  lazy val UnOpParser: Parser[String] = OpNot | OpMinus

  lazy val PolymorphicSelectOpParser: Parser[PolymorphicSelect] = positioned {
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

  lazy val NumberParser: PackratParser[NumericLit] = positioned(
    IntegerParser
  )

  lazy val LiteralParser: PackratParser[Literal] = positioned(
    BoolParser | NumberParser
  )

  /* END of Literals. */
  // Match quantifier patterns; but we don't want to make pattern a keyword.
  lazy val CommaSeparatedExprListParser: PackratParser[List[Expr]] =
    E1Parser ~ rep("," ~> E1Parser) ^^ { case e ~ es => e :: es }

  lazy val E1Parser: PackratParser[Expr] =
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
    E8Parser
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
      E12Parser
  }

  /** ExpressionSuffixes. */
  lazy val ExprSuffixParser: PackratParser[Operator] = positioned {
    ArraySelectOpParser | ArrayStoreOpParser | PolymorphicSelectOpParser
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
    KwBoolean ^^ { case _   => BooleanType() } |
      KwInteger ^^ { case _ => IntegerType() }
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

  lazy val TypeParser: PackratParser[Type] = positioned {
    ArrayTypeParser | SynonymTypeParser | PrimitiveTypeParser
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
    ExprParser ^^ { case expr => Lhs(expr) }
  }

  lazy val LhsListParser: PackratParser[List[Lhs]] =
    ("(" ~> LhsParser ~ rep("," ~> LhsParser) <~ ")") ^^ {
      case l ~ ls => l :: ls
    } |
      "(" ~> ")" ^^ { case _ => List.empty[Lhs] }

  lazy val IdListParser: PackratParser[List[Identifier]] =
    IdParser ~ rep("," ~> IdParser) ^^ { case id ~ ids => id :: ids }

  lazy val StatementParser: PackratParser[Statement] = positioned {
    KwAssert ~> ExprParser <~ ";" ^^ { case e   => AssertStmt(e, None) } |
      KwAssume ~> ExprParser <~ ";" ^^ { case e => AssumeStmt(e, None) } |
      KwHavoc ~> IdParser <~ ";" ^^ { case id   => HavocStmt(id) } |
      LhsParser ~ rep("," ~> LhsParser) ~ "=" ~ ExprParser ~ rep(
        "," ~> ExprParser
      ) <~ ";" ^^ {
        case l ~ ls ~ "=" ~ r ~ rs => AssignStmt(l :: ls, r :: rs)
      } |
      KwNext ~ "(" ~> ExprParser <~ ")" ~ ";" ^^ {
        case expr =>
          ModuleNextCallStmt(expr)
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
            BlockStmt(List.empty)
          )
      } |
      KwIf ~ "(" ~> (ExprParser <~ ")") ~ BlkStmtParser ~ (KwElse ~> BlkStmtParser) ^^ {
        case e ~ f ~ g => IfElseStmt(e, f, g)
      } |
      KwIf ~> (ExprParser ~ BlkStmtParser) ^^ {
        case e ~ f =>
          IfElseStmt(e, f, BlockStmt(List.empty))
      } |
      KwCase ~> rep(CaseBlockStmtParser) <~ KwEsac ^^ { case i => CaseStmt(i) } |
      BlkStmtParser |
      ";" ^^ { case _ => SkipStmt() }
  }

  lazy val CaseBlockStmtParser: PackratParser[(Expr, Statement)] =
    (ExprParser ~ ":" ~ BlkStmtParser) ^^ { case e ~ ":" ~ ss => (e, ss) } |
      (KwDefault ~ ":" ~> BlkStmtParser) ^^ { case ss         => (BoolLit(true), ss) }

  lazy val BlkStmtParser: PackratParser[BlockStmt] =
    "{" ~> rep(StatementParser) <~ "}" ^^ {
      case stmts =>
        BlockStmt(stmts)
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

  lazy val InitDeclParser: PackratParser[InitDecl] = positioned {
    KwInit ~> BlkStmtParser ^^ { case b => InitDecl(b) }
  }

  lazy val NextDeclParser: PackratParser[NextDecl] = positioned {
    KwNext ~> BlkStmtParser ^^ { case b => NextDecl(b) }
  }

  lazy val SpecDeclParser: PackratParser[SpecDecl] = positioned {
    (KwInvariant) ~> ("[" ~> rep(
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

  lazy val DeclParser: PackratParser[Decl] =
    positioned(
      TypeDeclParser | ConstDeclParser |
        VarsDeclParser | InputsDeclParser | OutputsDeclParser | SharedVarsDeclParser |
        ConstLitDeclParser | ConstDeclParser |
        InitDeclParser | NextDeclParser | SpecDeclParser
    )

  // control commands.
  lazy val CmdParser: PackratParser[ProofCommand] = positioned {
    IdParser ~ ("(" ~> IntegerParser <~ ")").? <~ ";" ^^ {
      case id ~ k =>
        ProofCommand(id, k)
    }
  }

  lazy val CmdBlockParser: PackratParser[List[ProofCommand]] =
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
