package com.uclid.uclidlanguage.parser

import com.uclid.uclidlanguage.compiler.{Location, UclidParserError}
import com.uclid.uclidlanguage.lexer._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object UclidParser extends PackratParsers {
  override type Elem = UclidToken

  class UclidTokenReader(tokens: Seq[UclidToken]) extends Reader[UclidToken] {
    override def first: UclidToken = tokens.head
    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position =
      tokens.headOption.map(_.pos).getOrElse(NoPosition)
    override def rest: Reader[UclidToken] = new UclidTokenReader(tokens.tail)
  }

  def apply(tokens: Seq[UclidToken]): Either[UclidParserError, Model] = {
    val reader = new UclidTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) =>
        Left(UclidParserError(Location(next.pos.line, next.pos.column), msg))
      case Failure(msg, next) =>
        Left(UclidParserError(Location(next.pos.line, next.pos.column), msg))
      case Error(msg, next) =>
        Left(UclidParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[Model] =
    positioned {
      phrase(modelParser)
    }

  private def identifier: Parser[IDENTIFIER] =
    positioned {
      accept("identifier", { case id @ IDENTIFIER(name) => id })
    }

  private def strliteral: Parser[STRLITERAL] =
    positioned {
      accept("string literal", { case lit @ STRLITERAL(name) => lit })
    }

  private def intliteral: Parser[INTLITERAL] =
    positioned {
      accept("integer literal", { case lit @ INTLITERAL(name) => lit })
    }

  private def boolliteral: Parser[BOOLLITERAL] =
    positioned {
      accept("boolean literal", { case lit @ BOOLLITERAL(name) => lit })
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

  lazy val relOpParser: PackratParser[Elem] =
    OPGT() | OPLT() | OPEQ() | OPNE() | OPGE() | OPLE()

  lazy val polymorphicSelectOpParser: PackratParser[OperatorApplication] =
    positioned {
      (PERIOD() ~> idParser) ^^ { case id =>
        OperatorApplication(PolymorphicSelect(id), List.empty)
      }
    }

  lazy val arraySelectOpParser: PackratParser[OperatorApplication] =
    positioned {
      (LBRACKET() ~> exprParser <~ RBRACKET()) ^^ { case e =>
        OperatorApplication(ArraySelect(), List(e))
      }
    }

  lazy val arrayStoreOpParser: PackratParser[OperatorApplication] =
    positioned {
      (LBRACKET() ~> (exprParser ~ (ARROW() ~> exprParser)) <~ RBRACKET()) ^^ {
        case e ~ r => OperatorApplication(ArrayUpdate(), List(e, r))
      }
    }

  lazy val idParser: PackratParser[Identifier] =
    positioned {
      identifier ^^ { case i => Identifier(i.str) }
    }

  /* BEGIN Literals. */
  lazy val boolParser: PackratParser[BoolLit] =
    positioned {
      boolliteral ^^ { case boolLit =>
        BoolLit(boolLit.str.toBoolean)
      }
    }

  lazy val integerParser: PackratParser[IntLit] =
    positioned {
      intliteral ^^ { case intLit =>
        IntLit(BigInt(intLit.str, 10))
      }
    }

  lazy val literalParser: PackratParser[Literal] =
    positioned {
      boolParser | integerParser
    }

  /* END of Literals. */

  lazy val e1Parser: PackratParser[Expr] =
    KWFORALL() ~> idTypeListParser ~ (COLONCOLON() ~> e1Parser) ^^ {
      case ids ~ expr =>
        OperatorApplication(ForallOp(ids), List(expr))
    } |
      KWEXISTS() ~> idTypeListParser ~ (COLONCOLON() ~> e1Parser) ^^ {
        case ids ~ expr =>
          OperatorApplication(ExistsOp(ids), List(expr))
      } |
      e3Parser

  /** e3Parser = e4Parser OpEquiv e3Parser | e4Parser  * */
  lazy val e3Parser: PackratParser[Expr] =
    positioned {
      e4Parser ~ OPBIIMPL() ~ e3Parser ^^ ast_binary | e4Parser
    }

  /** e4Parser = e5Parser OpImpl e4Parser | e5Parser  * */
  lazy val e4Parser: PackratParser[Expr] =
    positioned {
      e5Parser ~ OPIMPL() ~ e4Parser ^^ ast_binary | e5Parser
    }

  /** e5Parser = e6Parser <Bool_Or_Bv_Op> e5Parser | e6Parser * */
  lazy val e5Parser: PackratParser[Expr] =
    positioned {
      e6Parser ~ OPAND() ~ e5Parser ^^ ast_binary |
        e6Parser ~ OPOR() ~ e5Parser ^^ ast_binary |
        e6Parser
    }

  /** e6Parser = e7Parser OpRel e7Parser | e7Parser  * */
  lazy val e6Parser: PackratParser[Expr] =
    positioned {
      e7Parser ~ relOpParser ~ e7Parser ^^ ast_binary |
        e7Parser
    }

  /** e7Parser = e8Parser OpConcat e7Parser | e8Parser * */
  lazy val e7Parser: PackratParser[Expr] =
    positioned {
      e8Parser
    }

  /** e8Parser = e9Parser OpAdd e8Parser | e9Parser * */
  lazy val e8Parser: PackratParser[Expr] =
    positioned {
      e9Parser ~ OPADD() ~ e8Parser ^^ ast_binary | e9Parser
    }

  /** e9Parser = e9Parser OpSub e10Parser | e10Parser * */
  lazy val e9Parser: PackratParser[Expr] =
    positioned {
      e10Parser ~ OPSUB() ~ e10Parser ^^ ast_binary | e10Parser
    }

  /** e10Parser = e11Parser OpMul e11Parser | e11Parser * */
  lazy val e10Parser: PackratParser[Expr] =
    positioned {
      e11Parser ~ OPMUL() ~ e11Parser ^^ ast_binary | e11Parser
    }

  /** e11Parser = UnOp e12Parser | e12Parser * */
  lazy val e11Parser: PackratParser[Expr] =
    positioned {
      OPSUB() ~> e12Parser ^^ { case e =>
        OperatorApplication(UnaryMinusOp(), List(e))
      } |
        OPNOT() ~> e12Parser ^^ { case e =>
          OperatorApplication(NegationOp(), List(e))
        } |
        e12Parser
    }

  /** ExpressionSuffixes. */
  lazy val exprSuffixParser: PackratParser[OperatorApplication] =
    positioned {
      arraySelectOpParser | arrayStoreOpParser | polymorphicSelectOpParser
    }

  /** e12Parser = e12Parser (ExprList) | e12Parser ExprSuffix | e15Parser */
  lazy val e12Parser: PackratParser[Expr] =
    positioned {
      e12Parser ~ exprSuffixParser ^^ { case e ~ es =>
        OperatorApplication(es.op, List(e) ++ es.operands)
      } |
        e12Parser ~ exprListParser ^^ { case e ~ f =>
          FunctionApplication(e, f)
        } |
        e15Parser
    }

  lazy val constArrayParser: PackratParser[OperatorApplication] =
    positioned {
      KWCONST() ~ LPARENTHESIS() ~> exprParser ~ (COMMA() ~> inlineTypeParser) <~ RPARENTHESIS() ^^ {
        case (exp ~ typ) =>
          OperatorApplication(ConstArray(typ), List(exp))
      }
    }

  /** e15Parser = false | true | Number | ConstArray | Id FunctionApplication | (Expr) * */
  lazy val e15Parser: PackratParser[Expr] =
    positioned {
      KWIF() ~> (LPARENTHESIS() ~> exprParser <~ RPARENTHESIS()) ~ (KWTHEN() ~> exprParser) ~ (KWELSE() ~> exprParser) ^^ {
        case expr ~ thenExpr ~ elseExpr =>
          OperatorApplication(ITEOp(), List(expr, thenExpr, elseExpr))
      } |
        constArrayParser |
        LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() |
        literalParser |
        idParser <~ OPPRIME() ^^ { case id =>
          OperatorApplication(GetNextValueOp(), List(id))
        } |
        idParser
    }

  lazy val exprParser: PackratParser[Expr] =
    positioned {
      e1Parser
    }

  lazy val exprListParser: PackratParser[List[Expr]] =
    (LPARENTHESIS() ~> exprParser ~ rep(
      COMMA() ~> exprParser
    ) <~ RPARENTHESIS()) ^^ { case e ~ es =>
      e :: es
    } |
      LPARENTHESIS() ~> RPARENTHESIS() ^^ { case _ => List.empty[Expr] }

  lazy val primitiveTypeParser: PackratParser[InlineType] =
    positioned {
      KWBOOLEAN() ^^ { case _ => BooleanType() } |
        KWINTEGER() ^^ { case _ => IntegerType() }
    }

  lazy val arrayTypeParser: PackratParser[ArrayType] =
    positioned {
      (LBRACKET() ~> inlineTypeParser <~ RBRACKET()) ~ inlineTypeParser ^^ {
        case t ~ rt =>
          ArrayType(t, rt)
      }
    }

  // also handles module instance types
  lazy val namedTypeParser: PackratParser[NamedType] =
    positioned {
      idParser ^^ { case id =>
        NamedType(id)
      }
    }

  lazy val inlineTypeParser: PackratParser[InlineType] =
    positioned {
      arrayTypeParser | namedTypeParser | primitiveTypeParser
    }

  lazy val idsTypeParser: PackratParser[List[(Identifier, InlineType)]] =
    idListParser ~ (COLON() ~> inlineTypeParser) ^^ {
      case ids ~ typ => (ids.map((_, typ)))
    }

  lazy val idTypeListParser: PackratParser[List[(Identifier, InlineType)]] =
    LPARENTHESIS() ~> idsTypeParser ~ (rep(
      COMMA() ~> idsTypeParser
    ) <~ RPARENTHESIS()) ^^ { case t ~ ts =>
      t ++ ts.flatMap(v => v)
    } |
      LPARENTHESIS() ~ RPARENTHESIS() ^^ { case _ ~ _ =>
        List.empty[(Identifier, InlineType)]
      }

  lazy val typeListParser: PackratParser[List[InlineType]] =
    LPARENTHESIS() ~> inlineTypeParser ~ (rep(
      COMMA() ~> inlineTypeParser
    ) <~ RPARENTHESIS()) ^^ { case t ~ ts =>
      List(t) ++ ts.flatMap(v => List(v))
    } |
      LPARENTHESIS() ~ RPARENTHESIS() ^^ { case _ ~ _ =>
        List.empty[InlineType]
      }

  lazy val lhsParser: PackratParser[Expr] =
    exprParser

  lazy val idListParser: PackratParser[List[Identifier]] =
    idParser ~ rep(COMMA() ~> idParser) ^^ { case id ~ ids => id :: ids }

  lazy val statementParser: PackratParser[Statement] =
    positioned {
      val kwnext =
        KWNEXT() ~ LPARENTHESIS() ~> exprParser <~ RPARENTHESIS() <~ SEMICOLON() ^^ {
          case expr =>
            ModuleNextCallStmt(expr)
        }
      val kwhavoc = KWHAVOC() ~> exprParser <~ SEMICOLON() ^^ { case e =>
        HavocStmt(e)
      }
      val kwlet =
        KWLET() ~> idParser ~ (ASSIGN() ~> exprParser) <~ SEMICOLON() ^^ {
          case id ~ e => LetStatement(id, e)
        }
      val kwif =
        KWIF() ~ LPARENTHESIS() ~> (exprParser <~ RPARENTHESIS()) ~ blkStmtParser ~ (KWELSE() ~> blkStmtParser).? ^^ {
          case e ~ f ~ Some(g) => IfElseStmt(e, f, g)
          case e ~ f ~ None =>
            IfElseStmt(e, f, BlockStmt(List.empty))
        }
      val kwcase = KWCASE() ~> rep(caseBlockStmtParser) <~ KWESAC() ^^ {
        case i => CaseStmt(i)
      }
      val lhsparser =
        lhsParser ~ rep(COMMA() ~> lhsParser) ~ (ASSIGN() ~> exprParser) ~ rep(
          COMMA() ~> exprParser
        ) <~ SEMICOLON() ^^ { case l ~ ls ~ r ~ rs =>
          val assigns = ls
            .zip(rs)
            .foldLeft(List(AssignStmt(l, r)))((acc, p) =>
              acc ++ List(AssignStmt(p._1, p._2))
            )
          BlockStmt(assigns)
        }

      kwnext | kwhavoc | kwlet | kwif | kwcase | lhsparser
    }

  lazy val caseBlockStmtParser: PackratParser[(Expr, Statement)] =
    (exprParser ~ COLON() ~ blkStmtParser) ^^ { case e ~ COLON() ~ ss =>
      (e, ss)
    } |
      (KWDEFAULT() ~ COLON() ~> blkStmtParser) ^^ { case ss =>
        (BoolLit(true), ss)
      }

  lazy val blkStmtParser: PackratParser[BlockStmt] =
    positioned {
      LBRACE() ~> rep(statementParser) <~ RBRACE() ^^ { case stmts =>
        BlockStmt(stmts)
      }
    }

  lazy val recordDeclParser: PackratParser[TypeDecl] =
    positioned {
      KWTYPE() ~> idParser ~ (ASSIGN() ~> (KWRECORD() ~> (LBRACE() ~> idsTypeParser))) ~ (rep(
        COMMA() ~> idsTypeParser
      ) <~ RBRACE()) <~ SEMICOLON() ^^ { case id ~ v1 ~ vs =>
        val elements: List[(Identifier, InlineType)] = v1 ++ vs.flatten
        TypeDecl(id, Some(RecordType(elements)))
      }
    }

  lazy val enumDeclParser: PackratParser[TypeDecl] =
    positioned {
      KWTYPE() ~> idParser ~ (ASSIGN() ~> (KWENUM() ~> (LBRACE() ~> idParser))) ~ (rep(
        COMMA() ~> idParser
      ) <~ RBRACE()) <~ SEMICOLON() ^^ { case id ~ v1 ~ vs =>
        TypeDecl(id, Some(EnumType(List(v1) ++ vs)))
      }
    }

  lazy val typeDeclParser: PackratParser[TypeDecl] =
    positioned {

      enumDeclParser |
        recordDeclParser |
        KWTYPE() ~> idParser ~ (ASSIGN() ~> inlineTypeParser) <~ SEMICOLON() ^^ {
          case id ~ t =>
            TypeDecl(id, Some(t))
        } |
        KWTYPE() ~> idParser <~ SEMICOLON() ^^ { case id =>
          TypeDecl(id, None)
        }
    }

  lazy val varDeclParser: PackratParser[StateVarDecl] =
    positioned {

      KWVAR() ~> idListParser ~ COLON() ~ inlineTypeParser <~ SEMICOLON() ^^ {
        case ids ~ COLON() ~ typ =>
          StateVarDecl(ids, typ)
      }
    }

  lazy val constDeclParser: PackratParser[StateConstDecl] =
    positioned {

      KWCONST() ~> idListParser ~ COLON() ~ inlineTypeParser <~ SEMICOLON() ^^ {
        case ids ~ COLON() ~ typ =>
          StateConstDecl(ids, typ)
      }
    }

  lazy val inputsDeclParser: PackratParser[InputVarsDecl] =
    positioned {

      KWINPUT() ~> idListParser ~ COLON() ~ inlineTypeParser <~ SEMICOLON() ^^ {
        case ids ~ COLON() ~ typ =>
          InputVarsDecl(ids, typ)
      }
    }

  lazy val outputsDeclParser: PackratParser[OutputVarsDecl] =
    positioned {

      KWOUTPUT() ~> idListParser ~ COLON() ~ inlineTypeParser <~ SEMICOLON() ^^ {
        case ids ~ COLON() ~ typ =>
          OutputVarsDecl(ids, typ)
      }
    }

  lazy val sharedVarDeclParser: PackratParser[SharedVarsDecl] =
    positioned {

      KWSHAREDVAR() ~> idListParser ~ COLON() ~ inlineTypeParser <~ SEMICOLON() ^^ {
        case ids ~ COLON() ~ typ =>
          SharedVarsDecl(ids, typ)
      }
    }

  lazy val outerAxiomParser: PackratParser[OuterAxiom] =
    positioned {

      KWAXIOM() ~> exprParser <~ SEMICOLON() ^^ { case e =>
        OuterAxiom(e)
      }
    }

  lazy val defineDeclParser: PackratParser[DefineDecl] =
    positioned {

      KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (ASSIGN() ~> OPSUB()) ~ literalParser <~ SEMICOLON() ^^ {
        case id ~ typ ~ OPSUB() ~ lit =>
          DefineDecl(id, List.empty, typ, lit.negate())
      } |
        KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (ASSIGN() ~> OPSUB()) ~ idParser <~ SEMICOLON() ^^ {
          case id ~ typ ~ OPSUB() ~ lit =>
            DefineDecl(
              id,
              List.empty,
              typ,
              OperatorApplication(NegationOp(), List(lit))
            )
        } |
        KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (ASSIGN() ~> literalParser) <~ SEMICOLON() ^^ {
          case id ~ typ ~ lit => DefineDecl(id, List.empty, typ, lit)
        } |
        KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) ~ (ASSIGN() ~> idParser) <~ SEMICOLON() ^^ {
          case id ~ typ ~ lit => DefineDecl(id, List.empty, typ, lit)
        } |
        KWDEF() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) ~ (ASSIGN() ~> exprParser) <~ SEMICOLON() ^^ {
          case id ~ args ~ typ ~ expr =>
            DefineDecl(id, args, typ, expr)
        }
    }

  lazy val functionDeclParser: PackratParser[FunctionDecl] =
    positioned {

      KWCONST() ~> idParser ~ (COLON() ~> inlineTypeParser) <~ SEMICOLON() ^^ {
        case id ~ typ =>
          FunctionDecl(id, List.empty, typ)
      } |
        KWFUNC() ~> idParser ~ typeListParser ~ (COLON() ~> inlineTypeParser) <~ SEMICOLON() ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args, typ)
        } |
        KWFUNC() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) <~ SEMICOLON() ^^ {
          case id ~ args ~ typ =>
            FunctionDecl(id, args.map(p => p._2), typ)
        }
    }

  lazy val synthesisDeclParser: PackratParser[SynthesisDecl] =
    positioned {

      KWSYNTHESIS() ~> KWFUNC() ~> idParser ~ idTypeListParser ~ (COLON() ~> inlineTypeParser) <~ SEMICOLON() ^^ {
        case id ~ args ~ typ =>
          SynthesisDecl(id, args, typ)
      }
    }

  lazy val initDeclParser: PackratParser[InitDecl] =
    positioned {
      KWINIT() ~> blkStmtParser ^^ { case b => InitDecl(b) }
    }

  lazy val nextDeclParser: PackratParser[NextDecl] =
    positioned {
      KWNEXT() ~> blkStmtParser ^^ { case b => NextDecl(b) }
    }

  lazy val specDeclParser: PackratParser[SpecDecl] =
    positioned {

      (KWINVARIANT()) ~> idParser ~ (COLON() ~> exprParser) <~ SEMICOLON() ^^ {
        case id ~ expr => SpecDecl(id, expr)
      }
    }

  lazy val innerAxiomParser: PackratParser[InnerAxiom] =
    positioned {

      KWINVARIANT() ~> KWAXIOM() ~> idParser ~ (COLON() ~> exprParser) <~ SEMICOLON() ^^ {
        case id ~ expr => InnerAxiom(id, expr, true)
      } |
        KWAXIOM() ~> idParser ~ (COLON() ~> exprParser) <~ SEMICOLON() ^^ {
          case id ~ expr => InnerAxiom(id, expr, false)
        }
    }

  lazy val innerDeclParser: PackratParser[InnerDecl] =
    positioned {

      initDeclParser |
        nextDeclParser |
        varDeclParser |
        constDeclParser |
        inputsDeclParser |
        outputsDeclParser |
        sharedVarDeclParser |
        specDeclParser |
        innerAxiomParser
    }

  // control commands.
  lazy val cmdParser: PackratParser[Command] =
    positioned {
      KWOPTION() ~> (LPARENTHESIS() ~> strliteral) ~ (COMMA() ~> strliteral) <~ RPARENTHESIS() <~ SEMICOLON() ^^ {
        case name ~ set =>
          SolverOption(name.str, set.str)
      } |
        idParser ~ (LPARENTHESIS() ~> integerParser <~ RPARENTHESIS()).? <~ SEMICOLON() ^^ {
          case id ~ k =>
            ProofCommand(id, k)
        } |
        KWGETVALUE() ~> exprListParser.? <~ SEMICOLON() ^^ { case ts =>
          GetValue(ts.getOrElse(List.empty))
        } |
        KWCHECK() <~ SEMICOLON() ^^ { case _ =>
          Check()
        } |
        KWTRACE() ~> LPARENTHESIS() ~> integerParser ~ (COMMA() ~> boolParser).? ~ (COMMA() ~> blkStmtParser).? <~ RPARENTHESIS() <~ SEMICOLON() ^^ {
          case k ~ b ~ e =>
            Trace(
              k,
              b.getOrElse(BoolLit(true)),
              e.getOrElse(BlockStmt(List.empty))
            )
        }
    }

  lazy val cmdBlockParser: PackratParser[List[Command]] =
    KWCONTROL() ~ LBRACE() ~> rep(cmdParser) <~ RBRACE()

  lazy val moduleParser: PackratParser[ModuleDecl] =
    positioned {
      KWMODULE() ~> idParser ~ (LBRACE() ~> rep(
        innerDeclParser
      ) ~ (cmdBlockParser.?) <~ RBRACE()) ^^ {
        case id ~ (decls ~ Some(cs)) =>
          ModuleDecl(id, decls, cs)
        case id ~ (decls ~ None) =>
          ModuleDecl(id, decls, List.empty)
      }
    }

  lazy val outerDeclParser: PackratParser[OuterDecl] =
    positioned {
      moduleParser |
        typeDeclParser |
        defineDeclParser | // define has to come before function because of const
        functionDeclParser |
        synthesisDeclParser |
        outerAxiomParser
    }

  lazy val modelParser: PackratParser[Model] =
    rep(
      outerDeclParser
    ) ^^ { case declList => Model(declList) }
}
