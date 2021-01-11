package front

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.PackratParsers

import scala.language.implicitConversions
import scala.collection.mutable

/** This is a re-implementation of the Scala libraries StdTokenParsers with StdToken replaced by UclidToken. */
trait UclidTokenParsers extends TokenParsers {
  type Tokens <: UclidTokens
  import lexical.{Identifier, IntegerLit, Keyword, StringLit}

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

  /** A parser which matches a string literal */
  def stringLit: Parser[String] =
    elem("string literal", _.isInstanceOf[StringLit]) ^^ (_.chars)
}

object UclidParser extends UclidTokenParsers with PackratParsers {
  type Tokens = UclidTokens
  val lexical = new UclidLexical

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
  val KwVar = "var"
  val KwSharedVar = "sharedvar"
  val KwConst = "const"
  val KwFunc = "function"
  val KwDef = "define"
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
  val KwEnum = "enum"
  val KwRecord = "record"
  val KwForall = "forall"
  val KwExists = "exists"
  val KwHavoc = "havoc"
  val KwAssume = "assume"
  val KwAssert = "assert"
  val KwOption = "set_solver_option"
  val KwSynthesis = "synthesis"
  val KwGetValue = "print_cex"
  val KwCheck = "check"

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
    ".",
    "*",
    "::",
    "->",
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
    KwSharedVar,
    KwVar,
    KwIf,
    KwThen,
    KwElse,
    KwInput,
    KwOutput,
    KwConst,
    KwFunc,
    KwDef,
    KwModule,
    KwType,
    KwControl,
    KwInit,
    KwNext,
    KwInvariant,
    KwCase,
    KwEsac,
    KwDefault,
    KwEnum,
    KwRecord,
    KwForall,
    KwExists,
    KwHavoc,
    KwAssume,
    KwAssert,
    KwOption,
    KwSynthesis,
    KwGetValue,
    KwCheck
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
    case x ~ OpNE ~ y =>
      OperatorApplication(
        NegationOp(),
        List(OperatorApplication(EqualityOp(), List(x, y)))
      )
    case x ~ OpAdd ~ y => OperatorApplication(AddOp(), List(x, y))
    case x ~ OpSub ~ y => OperatorApplication(SubOp(), List(x, y))
    case x ~ OpMul ~ y => OperatorApplication(MulOp(), List(x, y))
  }

  lazy val LLOpParser: Parser[Operator] = positioned {
    OpLT ^^ { case _   => LTOp() } |
      OpLE ^^ { case _ => LEOp() }
  }

  lazy val RelOpParser: Parser[String] =
    OpGT | OpLT | OpEQ | OpNE | OpGE | OpLE

  lazy val PolymorphicSelectOpParser: Parser[OperatorApplication] = positioned {
    ("." ~> IdParser) ^^ {
      case id => OperatorApplication(PolymorphicSelect(id), List.empty)
    }
  }

  lazy val ArraySelectOpParser: Parser[OperatorApplication] = positioned {
    ("[" ~> ExprParser <~ "]") ^^ {
      case e =>
        OperatorApplication(ArraySelect(), List(e))
    }
  }

  lazy val ArrayStoreOpParser: Parser[OperatorApplication] = positioned {
    ("[" ~> (ExprParser ~ ("->" ~> ExprParser)) <~ "]") ^^ {
      case e ~ r => OperatorApplication(ArrayUpdate(), List(e, r))
    }
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

  lazy val LiteralParser: PackratParser[Literal] = positioned {
    BoolParser | IntegerParser
  }

  /* END of Literals. */

  lazy val E1Parser: PackratParser[Expr] =
    KwForall ~> IdTypeListParser ~ ("::" ~> E1Parser) ^^ {
      case ids ~ expr => {
        OperatorApplication(ForallOp(ids), List(expr))
      }
    } |
      KwExists ~> IdTypeListParser ~ ("::" ~> E1Parser) ^^ {
        case ids ~ expr => {
          OperatorApplication(ExistsOp(ids), List(expr))
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
  lazy val E7Parser: PackratParser[Expr] = positioned {
    E8Parser
  }

  /** E8Parser = E9Parser OpAdd E8Parser | E9Parser * */
  lazy val E8Parser: PackratParser[Expr] = positioned {
    E9Parser ~ OpAdd ~ E8Parser ^^ ast_binary | E9Parser
  }

  /** E9Parser = E9Parser OpSub E10Parser | E10Parser * */
  lazy val E9Parser: PackratParser[Expr] = positioned {
    E10Parser ~ OpSub ~ E10Parser ^^ ast_binary | E10Parser
  }

  /** E10Parser = E11Parser OpMul E11Parser | E11Parser * */
  lazy val E10Parser: PackratParser[Expr] = positioned {
    E11Parser ~ OpMul ~ E11Parser ^^ ast_binary | E11Parser
  }

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
  lazy val ExprSuffixParser: PackratParser[OperatorApplication] = positioned {
    ArraySelectOpParser | ArrayStoreOpParser | PolymorphicSelectOpParser
  }

  /** E12Parser = E12Parser (ExprList) | E12Parser ExprSuffix | E15Parser */
  lazy val E12Parser: PackratParser[Expr] = positioned {
    E12Parser ~ ExprSuffixParser ^^ {
      case e ~ es => OperatorApplication(es.op, List(e) ++ es.operands)
    } |
      E12Parser ~ ExprListParser ^^ { case e ~ f => FunctionApplication(e, f) } |
      E15Parser
  }

  lazy val ConstArrayParser: PackratParser[OperatorApplication] = positioned {
    KwConst ~ "(" ~> ExprParser ~ ("," ~> InlineTypeParser) <~ ")" ^^ {
      case (exp ~ typ) =>
        OperatorApplication(ConstArray(typ), List(exp))
    }
  }

  /** E15Parser = false | true | Number | ConstArray | Id FunctionApplication | (Expr) * */
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

  lazy val ExprParser: PackratParser[Expr] = positioned {
    E1Parser |
      "(".? ~> ExprParser <~ ")".? <~ OpPrime ^^ {
        case e =>
          throw new PrimeExpressionError(e)
      }
  }

  lazy val ExprListParser: Parser[List[Expr]] =
    ("(" ~> ExprParser ~ rep("," ~> ExprParser) <~ ")") ^^ {
      case e ~ es => e :: es
    } |
      "(" ~> ")" ^^ { case _ => List.empty[Expr] }

  lazy val PrimitiveTypeParser: PackratParser[InlineType] = positioned {
    KwBoolean ^^ { case _   => BooleanType() } |
      KwInteger ^^ { case _ => IntegerType() }
  }

  lazy val ArrayTypeParser: PackratParser[ArrayType] = positioned {
    ("[" ~> InlineTypeParser <~ "]") ~ InlineTypeParser ^^ {
      case t ~ rt =>
        ArrayType(t, rt)
    }
  }

  // also handles module instance types
  lazy val NamedTypeParser: PackratParser[NamedType] = positioned {
    IdParser ^^ {
      case id => NamedType(id)
    }
  }

  lazy val InlineTypeParser: PackratParser[InlineType] = positioned {
    ArrayTypeParser | NamedTypeParser | PrimitiveTypeParser |
      (KwRecord ~> ("{" ~> IdsTypeParser)) ~ (rep(
        "," ~> IdsTypeParser
      ) <~ "}") ^^ {
        case id ~ _ => throw new TypeMustBeNamed(id.head._1)
      } |
      (KwEnum ~> ("{" ~> IdParser)) ~ (rep(
        "," ~> IdParser
      ) <~ "}") ^^ {
        case v1 ~ _ => throw new TypeMustBeNamed(v1)
      }
  }

  lazy val IdsTypeParser: PackratParser[List[(Identifier, InlineType)]] =
    IdListParser ~ (":" ~> InlineTypeParser) ^^ {
      case ids ~ typ => (ids.map((_, typ)))
    }

  lazy val IdTypeListParser: PackratParser[List[(Identifier, InlineType)]] =
    "(" ~> IdsTypeParser ~ (rep("," ~> IdsTypeParser) <~ ")") ^^ {
      case t ~ ts =>
        t ++ ts.flatMap(v => v)
    } |
      "(" ~ ")" ^^ { case _ ~ _ => List.empty[(Identifier, InlineType)] }

  lazy val TypeListParser: PackratParser[List[InlineType]] =
    "(" ~> InlineTypeParser ~ (rep("," ~> InlineTypeParser) <~ ")") ^^ {
      case t ~ ts =>
        List(t) ++ ts.flatMap(v => List(v))
    } |
      "(" ~ ")" ^^ { case _ ~ _ => List.empty[InlineType] }

  lazy val LhsParser: PackratParser[Expr] = positioned {
    ExprParser ^^ {
      case expr =>
        expr match {
          case _: Identifier => expr
          case OperatorApplication(PolymorphicSelect(_), _) =>
            expr // for polymorphic selects
          case OperatorApplication(ArraySelect(), _)    => expr
          case OperatorApplication(GetNextValueOp(), _) => expr
          case _ => {
            throw new BadLeftHandSideError(expr)
          }
        }
    }
  }

  lazy val IdListParser: PackratParser[List[Identifier]] =
    IdParser ~ rep("," ~> IdParser) ^^ { case id ~ ids => id :: ids }

  lazy val StatementParser: PackratParser[Statement] = positioned {
    StatementEndInBracket |
      StatementEndInSemicolon <~ ";" |
      StatementEndInSemicolon ^^ {
        case s =>
          throw new MissingSemicolon(s)
      }
  }

  lazy val StatementEndInSemicolon: PackratParser[Statement] = positioned {
    LhsParser ~ rep("," ~> LhsParser) ~ "=" ~ ExprParser ~ rep(
      "," ~> ExprParser
    ) ^^ {
      case l ~ ls ~ "=" ~ r ~ rs => {
        if (ls.length == rs.length) {
          val assigns = ls
            .zip(rs)
            .foldLeft(List(AssignStmt(l, r)))((acc, p) =>
              acc ++ List(AssignStmt(p._1, p._2))
            )
          BlockStmt(assigns)
        } else {
          throw new MismatchingAssign(l, ls.length, rs.length)
        }
      }
    } |
      KwNext ~ "(" ~> ExprParser <~ ")" ^^ {
        case expr =>
          ModuleNextCallStmt(expr)
      } |
      KwHavoc ~> ExprParser ^^ { case e => HavocStmt(e) } |
      KwAssume ~> "(" ~> ExprParser <~ ")" ^^ {
        case e => throw new NotSupportedYet(e)
      } |
      KwAssert ~> "(" ~> ExprParser <~ ")" ^^ {
        case e => throw new NotSupportedYet(e)
      }
  }

  lazy val StatementEndInBracket: PackratParser[Statement] = positioned {
    KwIf ~ "(" ~> (ExprParser <~ ")") ~ BlkStmtParser ~ (KwElse ~> BlkStmtParser) ^^ {
      case e ~ f ~ g => IfElseStmt(e, f, g)
    } |
      KwIf ~> (ExprParser ~ BlkStmtParser) ^^ {
        case e ~ f =>
          IfElseStmt(e, f, BlockStmt(List.empty))
      } |
      KwCase ~> rep(CaseBlockStmtParser) <~ KwEsac ^^ { case i => CaseStmt(i) } |
      BlkStmtParser
  }

  lazy val CaseBlockStmtParser: PackratParser[(Expr, Statement)] =
    (ExprParser ~ ":" ~ BlkStmtParser) ^^ { case e ~ ":" ~ ss => (e, ss) } |
      (KwDefault ~ ":" ~> BlkStmtParser) ^^ { case ss         => (BoolLit(true), ss) }

  lazy val BlkStmtParser: PackratParser[BlockStmt] = positioned {
    "{" ~> rep(StatementParser) <~ "}" ^^ {
      case stmts =>
        BlockStmt(stmts)
    }
  }

  lazy val RecordDeclParser: PackratParser[TypeDecl] = positioned {
    KwType ~> IdParser ~ ("=" ~> (KwRecord ~> ("{" ~> IdsTypeParser))) ~ (rep(
      "," ~> IdsTypeParser
    ) <~ "}") ^^ {
      case id ~ v1 ~ vs => {
        val elements: List[(Identifier, InlineType)] = v1 ++ vs.flatten
        TypeDecl(id, Some(RecordType(elements)))
      }
    } |
      KwType ~> IdParser ~ ("=" ~> (KwRecord ~> ("{" ~> IdsTypeParser))) ~ (rep(
        "," ~> IdsTypeParser
      )) ^^ {
        case id ~ _ ~ _ => {
          throw new MissingCloseBracket(id)
        }
      }
  }

  lazy val EnumDeclParser: PackratParser[TypeDecl] = positioned {
    KwType ~> IdParser ~ ("=" ~> (KwEnum ~> ("{" ~> IdParser))) ~ (rep(
      "," ~> IdParser
    ) <~ "}") ^^ {
      case id ~ v1 ~ vs => TypeDecl(id, Some(EnumType(List(v1) ++ vs)))
    } |
      KwType ~> IdParser ~ ("=" ~> (KwEnum ~> ("{" ~> IdParser))) ~ (rep(
        "," ~> IdParser
      )) ^^ {
        case id ~ _ ~ _ =>
          throw new MissingCloseBracket(id)
      }
  }

  lazy val TypeDeclParserWithoutSemicolon: PackratParser[TypeDecl] =
    positioned {
      EnumDeclParser |
        RecordDeclParser |
        KwType ~> IdParser ~ ("=" ~> InlineTypeParser) ^^ {
          case id ~ t =>
            TypeDecl(id, Some(t))
        } |
        KwType ~> IdParser ^^ {
          case id =>
            TypeDecl(id, None)
        }
    }

  lazy val VarDeclParserWithoutSemicolon: PackratParser[StateVarDecl] =
    positioned {
      KwVar ~> IdListParser ~ ":" ~ InlineTypeParser ^^ {
        case ids ~ ":" ~ typ =>
          StateVarDecl(ids, typ)
      }
    }

  lazy val ConstDeclParserWithoutSemicolon: PackratParser[StateConstDecl] =
    positioned {
      KwConst ~> IdListParser ~ ":" ~ InlineTypeParser ^^ {
        case ids ~ ":" ~ typ =>
          StateConstDecl(ids, typ)
      }
    }

  lazy val InputsDeclParserWithoutSemicolon: PackratParser[InputVarsDecl] =
    positioned {
      KwInput ~> IdListParser ~ ":" ~ InlineTypeParser ^^ {
        case ids ~ ":" ~ typ =>
          InputVarsDecl(ids, typ)
      }
    }

  lazy val OutputsDeclParserWithoutSemicolon: PackratParser[OutputVarsDecl] =
    positioned {
      KwOutput ~> IdListParser ~ ":" ~ InlineTypeParser ^^ {
        case ids ~ ":" ~ typ =>
          OutputVarsDecl(ids, typ)
      }
    }

  lazy val SharedVarDeclParserWithoutSemicolon: PackratParser[SharedVarsDecl] =
    positioned {
      KwSharedVar ~> IdListParser ~ ":" ~ InlineTypeParser ^^ {
        case ids ~ ":" ~ typ =>
          SharedVarsDecl(ids, typ)
      }
    }

  lazy val DefineDeclParserWithoutSemicolon: PackratParser[DefineDecl] =
    positioned {
      KwConst ~> IdParser ~ (":" ~> InlineTypeParser) ~ ("=" ~> OpNeg) ~ ExprParser ^^ {
        case id ~ typ ~ OpNeg ~ lit =>
          lit match {
            case l: Literal => DefineDecl(id, List.empty, typ, l.negate())
            case _          => throw new ConstantMustBeLiteral(lit)
          }
      } |
        KwConst ~> IdParser ~ (":" ~> InlineTypeParser) ~ ("=" ~> ExprParser) ^^ {
          case id ~ typ ~ lit =>
            lit match {
              case l: Literal => DefineDecl(id, List.empty, typ, l)
              case _          => throw new ConstantMustBeLiteral(lit)
            }
        } |
        KwDef ~> IdParser ~ IdTypeListParser ~ (":" ~> InlineTypeParser) ~ ("=" ~> ExprParser) ^^ {
          case id ~ args ~ typ ~ expr =>
            DefineDecl(id, args, typ, expr)
        }
    }

  lazy val FunctionDeclParserWithoutSemicolon: PackratParser[FunctionDecl] =
    positioned {
      KwConst ~> IdParser ~ (":" ~> InlineTypeParser) ^^ {
        case id ~ typ =>
          FunctionDecl(id, List.empty, typ)
      } |
        KwFunc ~> IdParser ~ TypeListParser ~ (":" ~> InlineTypeParser) ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args, typ)
        } |
        KwFunc ~> IdParser ~ IdTypeListParser ~ (":" ~> InlineTypeParser) ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args.map(p => p._2), typ)
        }
    }

  lazy val SynthesisDeclParserWithoutSemicolon: PackratParser[SynthesisDecl] =
    positioned {
      KwSynthesis ~> KwFunc ~> IdParser ~ IdTypeListParser ~ (":" ~> InlineTypeParser) ^^ {
        case id ~ args ~ typ =>
          SynthesisDecl(id, args, typ)
      }
    }

  lazy val InitDeclParser: PackratParser[InitDecl] = positioned {
    KwInit ~> BlkStmtParser ^^ { case b => InitDecl(b) }
  }

  lazy val NextDeclParser: PackratParser[NextDecl] = positioned {
    KwNext ~> BlkStmtParser ^^ { case b => NextDecl(b) }
  }

  lazy val SpecDeclParserWithoutSemicolon: PackratParser[SpecDecl] =
    positioned {
      (KwInvariant) ~> IdParser ~ (":" ~> ExprParser) ^^ {
        case id ~ expr => SpecDecl(id, expr)
      } |
        (KwInvariant) ~> ":".? ~> ExprParser ^^ {
          case expr =>
            throw new InvariantMissingNameError(
              expr
            )
        }
    }

  lazy val DeclParserWithoutSemicolon: PackratParser[Decl] =
    positioned {
      VarDeclParserWithoutSemicolon |
        ConstDeclParserWithoutSemicolon |
        InputsDeclParserWithoutSemicolon |
        OutputsDeclParserWithoutSemicolon |
        SharedVarDeclParserWithoutSemicolon |
        SpecDeclParserWithoutSemicolon
    }

  lazy val DeclParser: PackratParser[Decl] =
    positioned {
      InitDeclParser |
        NextDeclParser |
        DeclParserWithoutSemicolon <~ ";" |
        DeclParserWithoutSemicolon ^^ {
          case d =>
            throw new MissingSemicolon(
              d
            )
        }
    }

  // control commands.
  lazy val CmdParserWithoutSemicolon: PackratParser[Command] = positioned {
    KwOption ~> ("(" ~> stringLit) ~ ("," ~> stringLit) <~ ")" ^^ {
      case name ~ set =>
        SolverOption(name, set)
    } |
      IdParser ~ ("(" ~> IntegerParser <~ ")").? ^^ {
        case id ~ k =>
          ProofCommand(id, k)
      } |
      KwGetValue ~> ExprListParser.? ^^ {
        case ts => GetValue(ts.getOrElse(List.empty))
      } |
      KwCheck ^^ {
        case _ => Check()
      }
  }

  lazy val CmdParser: PackratParser[Command] = positioned {
    CmdParserWithoutSemicolon <~ ";" |
      CmdParserWithoutSemicolon ^^ {
        case c =>
          throw new MissingSemicolon(
            c
          )
      }
  }

  lazy val CmdBlockParser: PackratParser[List[Command]] =
    KwControl ~ "{" ~> rep(CmdParser) <~ "}" |
      KwControl ~ "{" ~> rep(CmdParser) ^^ {
        case cmds =>
          throw new MissingCloseBracket(
            // TODO: how to correctly handle "or else"
            cmds.headOption.getOrElse(ProofCommand(Identifier("failed"), None))
          )
      }

  lazy val ModuleParser: PackratParser[ModuleDecl] = positioned {
    KwModule ~> IdParser ~ ("{" ~> rep(DeclParser) ~ (CmdBlockParser.?) <~ "}") ^^ {
      case id ~ (decls ~ Some(cs)) =>
        ModuleDecl(id, decls, cs)
      case id ~ (decls ~ None) =>
        ModuleDecl(id, decls, List.empty)
    } |
      KwModule ~> IdParser ~ ("{" ~> rep(DeclParser) ~ (CmdBlockParser.?)) ^^ {
        case id ~ (_ ~ Some(_)) =>
          throw new MissingCloseBracket(
            id
          )
        case id ~ (_ ~ None) =>
          throw new MissingCloseBracket(
            id
          )
      }
  }

  lazy val TopLevelParserWithoutSemicolon: PackratParser[TopLevelDecl] =
    positioned {
      TypeDeclParserWithoutSemicolon |
        DefineDeclParserWithoutSemicolon | // define has to come before function because of const
        FunctionDeclParserWithoutSemicolon |
        SynthesisDeclParserWithoutSemicolon
    }

  lazy val TopLevelParser: PackratParser[TopLevelDecl] = positioned {
    ModuleParser |
      TopLevelParserWithoutSemicolon <~ ";" |
      TopLevelParserWithoutSemicolon ^^ {
        case d =>
          throw new MissingSemicolon(
            d
          )
      } |
      DeclParser ^^ {
        case d =>
          throw new WrongTopeLevelDeclError(
            d
          )
      }
  }

  lazy val ModelParser: PackratParser[List[TopLevelDecl]] = rep(TopLevelParser)

  def parseModel(fname: String, text: String): List[TopLevelDecl] = {
    filename = fname
    val tokens = new PackratReader(new lexical.Scanner(text))
    phrase(ModelParser)(tokens) match {
      case Success(decls, _) => decls
      case NoSuccess(_, next) =>
        throw new ParseFailedError(next.pos, Some(filename))
    }
  }

  var filename = "Unknown File"
}
