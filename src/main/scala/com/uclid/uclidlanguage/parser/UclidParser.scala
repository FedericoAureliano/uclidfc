package com.uclid.uclidlanguage.parser

import com.uclid.error._
import com.uclid.uclidlanguage.compiler.{Location, UclidParserError}
import com.uclid.uclidlanguage.lexer._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object UclidParser extends PackratParsers {
  override type Elem = UclidToken

  class UclidTokenReader(tokens: Seq[UclidToken]) extends Reader[UclidToken] {
    override def first: UclidToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[UclidToken] = new UclidTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[UclidToken]): Either[UclidParserError, Model] = {
    val reader = new UclidTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(UclidParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[Model] = positioned {
    phrase(modelParser)
  }

  private def identifier: Parser[IDENTIFIER] = positioned {
    accept("identifier", { case id @ IDENTIFIER(name) => id })
  }

  private def strliteral: Parser[STRLITERAL] = positioned {
    accept("string literal", { case lit @ STRLITERAL(name) => lit })
  }
  private def intliteral: Parser[INTLITERAL] = positioned {
    accept("integer literal", { case lit @ INTLITERAL(name) => lit })
  }

  def ast_binary: Expr ~ Elem ~ Expr => Expr = {
    case x ~ OPBIIMPL() ~ y => OperatorApplication(IffOp(), List(x, y))
    case x ~ OPIMPL() ~ y   => OperatorApplication(ImplicationOp(), List(x, y))
    case x ~ OPAND() ~ y    => OperatorApplication(ConjunctionOp(), List(x, y))
    case x ~ OPOR() ~ y     => OperatorApplication(DisjunctionOp(), List(x, y))
    case x ~ OPLT() ~ y     => OperatorApplication(LTOp(), List(x, y))
    case x ~ OPGT() ~ y     => OperatorApplication(GTOp(), List(x, y))
    case x ~ OPLE() ~ y     => OperatorApplication(LEOp(), List(x, y))
    case x ~ OPGE() ~ y     => OperatorApplication(GEOp(), List(x, y))
    case x ~ OPEQ() ~ y     => OperatorApplication(EqualityOp(), List(x, y))
    case x ~ OPNE() ~ y =>
      OperatorApplication(
        NegationOp(),
        List(OperatorApplication(EqualityOp(), List(x, y)))
      )
    case x ~ OPADD() ~ y => OperatorApplication(AddOp(), List(x, y))
    case x ~ OPSUB() ~ y => OperatorApplication(SubOp(), List(x, y))
    case x ~ OPMUL() ~ y => OperatorApplication(MulOp(), List(x, y))
  }

  def lLOpParser: Parser[Operator] = positioned {
    OPLT() ^^ { case _   => LTOp() } |
      OPLE() ^^ { case _ => LEOp() }
  }

  def relOpParser: Parser[Elem] =
    OPGT() | OPLT() | OPEQ() | OPNE() | OPGE() | OPLE()

  def polymorphicSelectOpParser: Parser[OperatorApplication] = positioned {
    (PERIOD() ~> idParser) ^^ {
      case id => OperatorApplication(PolymorphicSelect(id), List.empty)
    }
  }

  def arraySelectOpParser: Parser[OperatorApplication] = positioned {
    (LBRACKET() ~> exprParser <~ RBRACKET()) ^^ {
      case e =>
        OperatorApplication(ArraySelect(), List(e))
    }
  }

  def arrayStoreOpParser: Parser[OperatorApplication] = positioned {
    (LBRACKET() ~> (exprParser ~ (ARROW() ~> exprParser)) <~ RBRACKET()) ^^ {
      case e ~ r => OperatorApplication(ArrayUpdate(), List(e, r))
    }
  }

  def idParser: PackratParser[Identifier] = positioned {
    identifier ^^ { case i => Identifier(i.str) }
  }

  /* BEGIN Literals. */
  def boolParser: PackratParser[BoolLit] =
    positioned {
      FALSE() ^^ { _ =>
        BoolLit(false)
      } | TRUE() ^^ { _ => BoolLit(true) }
    }

  def integerParser: PackratParser[IntLit] =
    positioned {
      intliteral ^^ {
        case intLit => IntLit(BigInt(intLit.str, 10))
      }
    }

  def literalParser: PackratParser[Literal] = positioned {
    boolParser | integerParser
  }

  /* END of Literals. */

  def e1Parser: PackratParser[Expr] =
    KWFORALL() ~> idTypeListParser ~ (COLONCOLON() ~> e1Parser) ^^ {
      case ids ~ expr => {
        OperatorApplication(ForallOp(ids), List(expr))
      }
    } |
      KWEXISTS() ~> idTypeListParser ~ (COLONCOLON() ~> e1Parser) ^^ {
        case ids ~ expr => {
          OperatorApplication(ExistsOp(ids), List(expr))
        }
      } |
      e3Parser

  /** e3Parser = e4Parser OpEquiv e3Parser | e4Parser  * */
  def e3Parser: PackratParser[Expr] = positioned {
    e4Parser ~ OPBIIMPL() ~ e3Parser ^^ ast_binary | e4Parser
  }

  /** e4Parser = e5Parser OpImpl e4Parser | e5Parser  * */
  def e4Parser: PackratParser[Expr] = positioned {
    e5Parser ~ OPIMPL() ~ e4Parser ^^ ast_binary | e5Parser
  }

  /** e5Parser = e6Parser <Bool_Or_Bv_Op> e5Parser | e6Parser * */
  def e5Parser: PackratParser[Expr] = positioned {
    e6Parser ~ OPAND() ~ e5Parser ^^ ast_binary |
      e6Parser ~ OPOR() ~ e5Parser ^^ ast_binary |
      e6Parser
  }

  /** e6Parser = e7Parser OpRel e7Parser | e7Parser  * */
  def e6Parser: PackratParser[Expr] = positioned {
    e7Parser ~ lLOpParser ~ e7Parser ~ lLOpParser ~ e7Parser ^^ {
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
      e7Parser ~ relOpParser ~ e7Parser ^^ ast_binary |
      e7Parser
  }

  /** e7Parser = e8Parser OpConcat e7Parser | e8Parser * */
  def e7Parser: PackratParser[Expr] = positioned {
    e8Parser
  }

  /** e8Parser = e9Parser OpAdd e8Parser | e9Parser * */
  def e8Parser: PackratParser[Expr] = positioned {
    e9Parser ~ OPADD() ~ e8Parser ^^ ast_binary | e9Parser
  }

  /** e9Parser = e9Parser OpSub e10Parser | e10Parser * */
  def e9Parser: PackratParser[Expr] = positioned {
    e10Parser ~ OPSUB() ~ e10Parser ^^ ast_binary | e10Parser
  }

  /** e10Parser = e11Parser OpMul e11Parser | e11Parser * */
  def e10Parser: PackratParser[Expr] = positioned {
    e11Parser ~ OPMUL() ~ e11Parser ^^ ast_binary | e11Parser
  }

  /** e11Parser = UnOp e12Parser | e12Parser * */
  def e11Parser: PackratParser[Expr] = positioned {
    OPSUB() ~> e12Parser ^^ {
      case e => OperatorApplication(UnaryMinusOp(), List(e))
    } |
      OPNOT() ~> e12Parser ^^ {
        case e => OperatorApplication(NegationOp(), List(e))
      } |
      e12Parser
  }

  /** ExpressionSuffixes. */
  def exprSuffixParser: PackratParser[OperatorApplication] = positioned {
    arraySelectOpParser | arrayStoreOpParser | polymorphicSelectOpParser
  }

  /** e12Parser = e12Parser (ExprList) | e12Parser ExprSuffix | e15Parser */
  def e12Parser: PackratParser[Expr] = positioned {
    e12Parser ~ exprSuffixParser ^^ {
      case e ~ es => OperatorApplication(es.op, List(e) ++ es.operands)
    } |
      e12Parser ~ exprListParser ^^ { case e ~ f => FunctionApplication(e, f) } |
      e15Parser
  }

  def constArrayParser: PackratParser[OperatorApplication] = positioned {
    KWCONST() ~ LPARENTHESIS() ~> exprParser ~ (COMMA() ~> inlineTypeParser) <~ RPARENTHESIS() ^^ {
      case (exp ~ typ) =>
        OperatorApplication(ConstArray(typ), List(exp))
    }
  }

  /** e15Parser = false | true | Number | ConstArray | Id FunctionApplication | (Expr) * */
  def e15Parser: PackratParser[Expr] = positioned {
    literalParser |
      KWIF() ~> (LPARENTHESIS() ~> exprParser <~ RPARENTHESIS()) ~ (KWTHEN() ~> exprParser) ~ (KWELSE() ~> exprParser) ^^ {
        case expr ~ thenExpr ~ elseExpr =>
          OperatorApplication(ITEOp(), List(expr, thenExpr, elseExpr))
      } |
      constArrayParser |
      LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() |
      idParser <~ OPPRIME() ^^ {
        case id =>
          OperatorApplication(GetNextValueOp(), List(id))
      } |
      idParser
  }

  def exprParser: PackratParser[Expr] = positioned {
    e1Parser |
      LPARENTHESIS().? ~> exprParser <~ RPARENTHESIS().? <~ OPPRIME() ^^ {
        case e =>
          throw new PrimeExpressionError(e)
      }
  }

  def exprListParser: Parser[List[Expr]] =
    (LPARENTHESIS() ~> exprParser ~ rep(COMMA() ~> exprParser) <~ RPARENTHESIS()) ^^ {
      case e ~ es => e :: es
    } |
      LPARENTHESIS() ~> RPARENTHESIS() ^^ { case _ => List.empty[Expr] }

  def primitiveTypeParser: PackratParser[InlineType] = positioned {
    KWBOOLEAN() ^^ { case _   => BooleanType() } |
      KWINTEGER() ^^ { case _ => IntegerType() }
  }

  def arrayTypeParser: PackratParser[ArrayType] = positioned {
    (LBRACKET() ~> inlineTypeParser <~ RBRACKET()) ~ inlineTypeParser ^^ {
      case t ~ rt =>
        ArrayType(t, rt)
    }
  }

  // also handles module instance types
  def namedTypeParser: PackratParser[NamedType] = positioned {
    idParser ^^ {
      case id => NamedType(id)
    }
  }

  def inlineTypeParser: PackratParser[InlineType] = positioned {
    arrayTypeParser | namedTypeParser | primitiveTypeParser |
      (KWRECORD() ~> (LBRACE() ~> idsTypeParser)) ~ (rep(
        COMMA() ~> idsTypeParser
      ) <~ RBRACE()) ^^ {
        case id ~ _ => throw new TypeMustBeNamed(id.head._1)
      } |
      (KWENUM() ~> (LBRACE() ~> idParser)) ~ (rep(
        COMMA() ~> idParser
      ) <~ RBRACE()) ^^ {
        case v1 ~ _ => throw new TypeMustBeNamed(v1)
      }
  }

  def idsTypeParser: PackratParser[List[(Identifier, InlineType)]] =
    idListParser ~ (COLON() ~> inlineTypeParser) ^^ {
      case ids ~ typ => (ids.map((_, typ)))
    }

  def idTypeListParser: PackratParser[List[(Identifier, InlineType)]] =
    LPARENTHESIS() ~> idsTypeParser ~ (rep(COMMA() ~> idsTypeParser) <~ RPARENTHESIS()) ^^ {
      case t ~ ts =>
        t ++ ts.flatMap(v => v)
    } |
      LPARENTHESIS() ~ RPARENTHESIS() ^^ { case _ ~ _ => List.empty[(Identifier, InlineType)] }

  def typeListParser: PackratParser[List[InlineType]] =
    LPARENTHESIS() ~> inlineTypeParser ~ (rep(COMMA() ~> inlineTypeParser) <~ RPARENTHESIS()) ^^ {
      case t ~ ts =>
        List(t) ++ ts.flatMap(v => List(v))
    } |
      LPARENTHESIS() ~ RPARENTHESIS() ^^ { case _ ~ _ => List.empty[InlineType] }

  def lhsParser: PackratParser[Expr] = positioned {
    exprParser ^^ {
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

  def idListParser: PackratParser[List[Identifier]] =
    idParser ~ rep(COMMA() ~> idParser) ^^ { case id ~ ids => id :: ids }

  def statementParser: PackratParser[Statement] = positioned {
    statementEndInBracket |
      statementEndInSemicolon <~ SEMICOLON() |
      statementEndInSemicolon ^^ {
        case s =>
          throw new MissingSemicolon(s)
      }
  }

  def statementEndInSemicolon: PackratParser[Statement] = positioned {
    lhsParser ~ rep(COMMA() ~> lhsParser) ~ EQ() ~ exprParser ~ rep(
      COMMA() ~> exprParser
    ) ^^ {
      case l ~ ls ~ EQ() ~ r ~ rs => {
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
      KWNEXT() ~ LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() ^^ {
        case expr =>
          ModuleNextCallStmt(expr)
      } |
      KWHAVOC() ~> exprParser ^^ { case e => HavocStmt(e) } |
      KWASSUME() ~> LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() ^^ {
        case e => throw new InternalNotSupportedYet(e)
      } |
      KWASSERT() ~> LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() ^^ {
        case e => throw new InternalNotSupportedYet(e)
      } |
      KWLET() ~> idParser ~ (EQ() ~> exprParser) ^^ {
        case id ~ e => LetStatement(id, e)
      }
  }

  def statementEndInBracket: PackratParser[Statement] = positioned {
    KWIF() ~ LPARENTHESIS() ~> (exprParser <~ RPARENTHESIS()) ~ blkStmtParser ~ (KWELSE() ~> blkStmtParser) ^^ {
      case e ~ f ~ g => IfElseStmt(e, f, g)
    } |
      KWIF() ~> (exprParser ~ blkStmtParser) ^^ {
        case e ~ f =>
          IfElseStmt(e, f, BlockStmt(List.empty))
      } |
      KWCASE() ~> rep(caseBlockStmtParser) <~ KWESAC() ^^ { case i => CaseStmt(i) } |
      blkStmtParser
  }

  def caseBlockStmtParser: PackratParser[(Expr, Statement)] =
    (exprParser ~ COLON() ~ blkStmtParser) ^^ { case e ~ COLON() ~ ss => (e, ss) } |
      (KWDEFAULT() ~ COLON() ~> blkStmtParser) ^^ { case ss         => (BoolLit(true), ss) }

  def blkStmtParser: PackratParser[BlockStmt] = positioned {
    LBRACE() ~> rep(statementParser) <~ RBRACE() ^^ {
      case stmts =>
        BlockStmt(stmts)
    }
  }

  def recordDeclParser: PackratParser[TypeDecl] = positioned {
    KWTYPE() ~> idParser ~ (EQ() ~> (KWRECORD() ~> (LBRACE() ~> idsTypeParser))) ~ (rep(
      COMMA() ~> idsTypeParser
    ) <~ RBRACE()) ^^ {
      case id ~ v1 ~ vs => {
        val elements: List[(Identifier, InlineType)] = v1 ++ vs.flatten
        TypeDecl(id, Some(RecordType(elements)))
      }
    } |
      KWTYPE() ~> idParser ~ (EQ() ~> (KWRECORD() ~> (LBRACE() ~> idsTypeParser))) ~ (rep(
        COMMA() ~> idsTypeParser
      )) ^^ {
        case id ~ _ ~ _ => {
          throw new MissingCloseBracket(id)
        }
      }
  }

  def enumDeclParser: PackratParser[TypeDecl] = positioned {
    KWTYPE() ~> idParser ~ (EQ() ~> (KWENUM() ~> (LBRACE() ~> idParser))) ~ (rep(
      COMMA() ~> idParser
    ) <~ RBRACE()) ^^ {
      case id ~ v1 ~ vs => TypeDecl(id, Some(EnumType(List(v1) ++ vs)))
    } |
      KWTYPE() ~> idParser ~ (EQ() ~> (KWENUM() ~> (LBRACE() ~> idParser))) ~ (rep(
        COMMA() ~> idParser
      )) ^^ {
        case id ~ _ ~ _ =>
          throw new MissingCloseBracket(id)
      }
  }

  def typeDeclParserWithoutSemicolon: PackratParser[TypeDecl] =
    positioned {
      enumDeclParser |
        recordDeclParser |
        KWTYPE() ~> idParser ~ (EQ() ~> inlineTypeParser) ^^ {
          case id ~ t =>
            TypeDecl(id, Some(t))
        } |
        KWTYPE() ~> idParser ^^ {
          case id =>
            TypeDecl(id, None)
        }
    }

  def varDeclParserWithoutSemicolon: PackratParser[StateVarDecl] =
    positioned {
      KWVAR() ~> idListParser ~ COLON() ~ inlineTypeParser ^^ {
        case ids ~ COLON() ~ typ =>
          StateVarDecl(ids, typ)
      }
    }

  def constDeclParserWithoutSemicolon: PackratParser[StateConstDecl] =
    positioned {
      KWCONST() ~> idListParser ~ COLON() ~ inlineTypeParser ^^ {
        case ids ~ COLON() ~ typ =>
          StateConstDecl(ids, typ)
      }
    }

  def inputsDeclParserWithoutSemicolon: PackratParser[InputVarsDecl] =
    positioned {
      KWINPUT() ~> idListParser ~ COLON() ~ inlineTypeParser ^^ {
        case ids ~ COLON() ~ typ =>
          InputVarsDecl(ids, typ)
      }
    }

  def outputsDeclParserWithoutSemicolon: PackratParser[OutputVarsDecl] =
    positioned {
      KWOUTPUT() ~> idListParser ~ COLON() ~ inlineTypeParser ^^ {
        case ids ~ COLON() ~ typ =>
          OutputVarsDecl(ids, typ)
      }
    }

  def sharedVarDeclParserWithoutSemicolon: PackratParser[SharedVarsDecl] =
    positioned {
      KWSHAREDVAR() ~> idListParser ~ COLON() ~ inlineTypeParser ^^ {
        case ids ~ COLON() ~ typ =>
          SharedVarsDecl(ids, typ)
      }
    }

  def outerAxiomParserWithoutSemicolon: PackratParser[OuterAxiom] =
    positioned {
      KWAXIOM() ~> exprParser ^^ {
        case e => OuterAxiom(e)
      }
    }

  def defineDeclParserWithoutSemicolon: PackratParser[DefineDecl] =
    positioned {
      KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (EQ() ~> OPSUB()) ~ exprParser ^^ {
        case id ~ typ ~ OPSUB() ~ lit =>
          lit match {
            case l: Literal => DefineDecl(id, List.empty, typ, l.negate())
            case i: Identifier =>
              DefineDecl(
                id,
                List.empty,
                typ,
                OperatorApplication(NegationOp(), List(i))
              )
            case _ => throw new ConstantMustBeLiteral(lit)
          }
      } |
        KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (EQ() ~> exprParser) ^^ {
          case id ~ typ ~ lit =>
            lit match {
              case l: Literal    => DefineDecl(id, List.empty, typ, l)
              case i: Identifier => DefineDecl(id, List.empty, typ, i)
              case _             => throw new ConstantMustBeLiteral(lit)
            }
        } |
        KWDEF() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) ~ (EQ() ~> exprParser) ^^ {
          case id ~ args ~ typ ~ expr =>
            DefineDecl(id, args, typ, expr)
        }
    }

  def functionDeclParserWithoutSemicolon: PackratParser[FunctionDecl] =
    positioned {
      KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ^^ {
        case id ~ typ =>
          FunctionDecl(id, List.empty, typ)
      } |
        KWFUNC() ~> idParser ~ typeListParser ~ (COLON() ~> inlineTypeParser) ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args, typ)
        } |
        KWFUNC() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args.map(p => p._2), typ)
        }
    }

  def synthesisDeclParserWithoutSemicolon: PackratParser[SynthesisDecl] =
    positioned {
      KWSYNTHESIS() ~> KWFUNC() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) ^^ {
        case id ~ args ~ typ =>
          SynthesisDecl(id, args, typ)
      }
    }

  def initDeclParser: PackratParser[InitDecl] = positioned {
    KWINIT() ~> blkStmtParser ^^ { case b => InitDecl(b) }
  }

  def nextDeclParser: PackratParser[NextDecl] = positioned {
    KWNEXT() ~> blkStmtParser ^^ { case b => NextDecl(b) }
  }

  def specDeclParserWithoutSemicolon: PackratParser[SpecDecl] =
    positioned {
      (KWINVARIANT()) ~> idParser ~ (COLON() ~> exprParser) ^^ {
        case id ~ expr => SpecDecl(id, expr)
      } |
        (KWINVARIANT()) ~> COLON().? ~> exprParser ^^ {
          case expr =>
            throw new InvariantMissingNameError(
              expr
            )
        }
    }

  def innerAxiomParserWithoutSemicolon: PackratParser[InnerAxiom] =
    positioned {
      KWINVARIANT() ~> KWAXIOM() ~> idParser ~ (COLON() ~> exprParser) ^^ {
        case id ~ expr => InnerAxiom(id, expr, true)
      } |
        KWINVARIANT() ~> KWAXIOM() ~> COLON().? ~> exprParser ^^ {
          case expr =>
            throw new InvariantMissingNameError(
              expr
            )
        } |
        KWAXIOM() ~> idParser ~ (COLON() ~> exprParser) ^^ {
          case id ~ expr => InnerAxiom(id, expr, false)
        } |
        KWAXIOM() ~> COLON().? ~> exprParser ^^ {
          case expr =>
            throw new InvariantMissingNameError(
              expr
            )
        }
    }

  def innerDeclParserWithoutSemicolon: PackratParser[InnerDecl] =
    positioned {
      varDeclParserWithoutSemicolon |
        constDeclParserWithoutSemicolon |
        inputsDeclParserWithoutSemicolon |
        outputsDeclParserWithoutSemicolon |
        sharedVarDeclParserWithoutSemicolon |
        specDeclParserWithoutSemicolon |
        innerAxiomParserWithoutSemicolon
    }

  def innerDeclParser: PackratParser[InnerDecl] =
    positioned {
      initDeclParser |
        nextDeclParser |
        innerDeclParserWithoutSemicolon <~ SEMICOLON() |
        innerDeclParserWithoutSemicolon ^^ {
          case d =>
            throw new MissingSemicolon(
              d
            )
        }
    }

  // control commands.
  def cmdParserWithoutSemicolon: PackratParser[Command] = positioned {
    KWOPTION() ~> (LPARENTHESIS() ~> strliteral) ~ (COMMA() ~> strliteral) <~ RPARENTHESIS() ^^ {
      case name ~ set =>
        SolverOption(name.str, set.str)
    } |
      idParser ~ (LPARENTHESIS() ~> integerParser <~ RPARENTHESIS()).? ^^ {
        case id ~ k =>
          ProofCommand(id, k)
      } |
      KWGETVALUE() ~> exprListParser.? ^^ {
        case ts => GetValue(ts.getOrElse(List.empty))
      } |
      KWCHECK() ^^ {
        case _ => Check()
      } |
      KWTRACE() ~> LPARENTHESIS() ~> integerParser ~ (COMMA() ~> boolParser).? ~ (COMMA() ~> blkStmtParser).? <~ RPARENTHESIS() ^^ {
        case k ~ b ~ e =>
          Trace(
            k,
            b.getOrElse(BoolLit(true)),
            e.getOrElse(BlockStmt(List.empty))
          )
      }
  }

  def cmdParser: PackratParser[Command] = positioned {
    cmdParserWithoutSemicolon <~ SEMICOLON() |
      cmdParserWithoutSemicolon ^^ {
        case c =>
          throw new MissingSemicolon(
            c
          )
      }
  }

  def cmdBlockParser: PackratParser[List[Command]] =
    KWCONTROL() ~ LBRACE() ~> rep(cmdParser) <~ RBRACE() |
      KWCONTROL() ~ LBRACE() ~> rep(cmdParser) ^^ {
        case cmds =>
          throw new MissingCloseBracket(
            // TODO: how to correctly handle "or else"
            cmds.headOption.getOrElse(ProofCommand(Identifier("failed"), None))
          )
      }

  def moduleParser: PackratParser[ModuleDecl] = positioned {
    KWMODULE() ~> idParser ~ (LBRACE() ~> rep(innerDeclParser) ~ (cmdBlockParser.?) <~ RBRACE()) ^^ {
      case id ~ (decls ~ Some(cs)) =>
        ModuleDecl(id, decls, cs)
      case id ~ (decls ~ None) =>
        ModuleDecl(id, decls, List.empty)
    } |
      KWMODULE() ~> idParser ~ (LBRACE() ~> rep(innerDeclParser) ~ (cmdBlockParser.?)) ^^ {
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

  def outerDeclParserWithoutSemicolon: PackratParser[OuterDecl] =
    positioned {
      typeDeclParserWithoutSemicolon |
        defineDeclParserWithoutSemicolon | // define has to come before function because of const
        functionDeclParserWithoutSemicolon |
        synthesisDeclParserWithoutSemicolon |
        outerAxiomParserWithoutSemicolon
    }

  def outerDeclParser: PackratParser[OuterDecl] = positioned {
    moduleParser |
      outerDeclParserWithoutSemicolon <~ SEMICOLON() |
      outerDeclParserWithoutSemicolon ^^ {
        case d =>
          throw new MissingSemicolon(
            d
          )
      } |
      innerDeclParser ^^ {
        case d =>
          throw new WrongTopeLevelDeclError(
            d
          )
      }
  }

  def modelParser: PackratParser[Model] = rep(
    outerDeclParser
  ) ^^ {case declList => Model(declList)}
}
