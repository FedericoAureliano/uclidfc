package front

import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.input.CharArrayReader.EofCh
import scala.collection.mutable
import scala.util.parsing.input.Positional

trait UclidTokens extends Tokens {
  abstract class UclidToken extends Token with Positional

  /** Keywords. */
  case class Keyword(chars: String) extends UclidToken {
    override def toString = "`" + chars + "'"
  }

  /** The class of integer literal tokens. */
  case class IntegerLit(chars: String, base: Int) extends UclidToken {
    override def toString = chars.toString + "_" + base.toString
  }

  /** The class of identifier tokens. */
  case class Identifier(chars: String) extends UclidToken {
    override def toString = "identifier " + chars
  }
}

/** Uclid's lexical tokens.
  *  Most of this code is based on the Scala library's StdLexical.
  *  We can't subclass StdLexical because it uses StdToken while we have more interesting tokens (UclidTokens).
  */
class UclidLexical extends Lexical with UclidTokens with Positional {

  override def token: Parser[Token] =
    (positioned {
      (letter | '_') ~ rep(letter | '_' | digit) ^^ {
        case first ~ rest =>
          processIdent((first :: rest).mkString(""))
      }
    }
      | positioned {
        digit ~ rep(digit) ^^ {
          case first ~ rest =>
            IntegerLit((first :: rest).mkString(""), 10)
        }
      }
      | EofCh ^^^ EOF
      | '\"' ~> failure("unclosed string literal")
      | delim
      | failure("illegal character"))

  // see `whitespace in `Scanners`
  def whitespace: Parser[Any] = rep(
    whitespaceChar
      | '/' ~ '*' ~ comment
      | '/' ~ '/' ~ rep(chrExcept(EofCh, '\n'))
      | '/' ~ '*' ~ failure("unclosed comment")
  )

  protected def comment: Parser[Any] = (
    rep(chrExcept(EofCh, '*') | '*' ~ not('/')) ~ '*' ~ '/'
      ^^^ ' '
  )

  /** The set of reserved identifiers: these will be returned as `Keyword`s. */
  val reserved = new mutable.HashSet[String]

  /** The set of delimiters (ordering does not matter). */
  val delimiters = new mutable.HashSet[String]

  protected def processIdent(name: String) =
    if (reserved contains name) Keyword(name) else Identifier(name)

  private lazy val _delim: Parser[Token] = {
    // construct parser for delimiters by |'ing together the parsers for the individual delimiters,
    // starting with the longest one -- otherwise a delimiter D will never be matched if there is
    // another delimiter that is a prefix of D
    def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { _ =>
      Keyword(s)
    }

    val d = new Array[String](delimiters.size)
    delimiters.copyToArray(d, 0)
    scala.util.Sorting.quickSort(d)
    d.toList
      .map(parseDelim)
      .foldRight(
        failure("no matching delimiter"): Parser[Token]
      )((x, y) => y | x)
  }
  protected def delim: Parser[Token] = _delim

}
