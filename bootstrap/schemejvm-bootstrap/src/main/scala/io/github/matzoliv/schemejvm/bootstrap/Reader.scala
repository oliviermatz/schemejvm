package io.github.matzoliv.schemejvm.bootstrap

import java.io.{ByteArrayInputStream, InputStreamReader}

import io.github.matzoliv.schemejvm.runtime.{Eof, Void}

import scala.annotation.tailrec
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}
import scala.util.parsing.input.StreamReader

class ReadFailedException(msg: String) extends Exception(msg)

object Reader extends RegexParsers with JavaTokenParsers {

  override protected val whiteSpace = """(\s|$;.*|(?=[^\\]);.*)+""".r

  def read(string: String): Object = read(StreamReader(new InputStreamReader(new ByteArrayInputStream(string.getBytes("UTF8")))))

  def read(reader: StreamReader): Object = parse(expression, reader) match {
    case Success(result, _) =>
      result
    case NoSuccess(msg, _) =>
      throw new ReadFailedException(msg)
  }

  def readMultiple(string: String): List[Object] = readMultiple(StreamReader(new InputStreamReader(new ByteArrayInputStream(string.getBytes("UTF8")))))

  def readMultiple(reader: StreamReader): List[Object] = parse(expressions, reader) match {
    case Success(result, _) =>
      result
    case NoSuccess(msg, _) =>
      throw new ReadFailedException(msg)
  }

  private def trueParser: Parser[Object] = "#t" ^^ { _ => S.Bool(true) }

  private def falseParser: Parser[Object] = "#f" ^^ { _ => S.Bool(false) }

  private def voidParser: Parser[Object] = "#!void" ^^ { _ => Void.value }

  private def eofParser: Parser[Object] = "#!eof" ^^ { _ => Eof.value }

  private def minusSymbol: Parser[Object] = "-[@!+*=_?<>A-Za-z]*".r ^^ (S.Symbol(_))

  private def symbolParser: Parser[Object] = "[@!+*=_?<>A-Za-z][@!+\\-*=_?<>.#:A-Za-z0-9\\[\\]]*".r ^^ (S.Symbol(_))

  private def doubleParser: Parser[java.lang.Double] =  "-?[0-9]+\\.[0-9]*".r ^^ (java.lang.Double.parseDouble(_))

  private def integerParser: Parser[java.lang.Integer] = "-?[0-9]+".r ^^ (java.lang.Integer.parseInt(_))

  private def quoteParser: Parser[Object] = "'" ~> expression ^^ (S.List(S.Symbol("quote"), _))

  private def unquoteSplicingParser: Parser[Object] = ",@" ~> expression ^^ (S.List(S.Symbol("unquote-splicing"), _))

  private def unquoteParser: Parser[Object] = "," ~> expression ^^ (S.List(S.Symbol("unquote"), _))

  private def quasiquoteParser: Parser[Object] = "`" ~> expression ^^ (S.List(S.Symbol("quasiquote"), _))

  private def stringParser: Parser[Object] = stringLiteral ^^ (_.stripPrefix("\"").stripSuffix(("\"")).replace("\\\"", "\""))

  private def characterParser: Parser[Object] = "#\\\\(space|newline|[a-zA-z_0-9;!@#$%^&\\*()\\-+\\[\\]\\\\',.\"<>`~=\\?:])".r ^^ {
    case "#\\space" => new Character(' ')
    case "#\\newline" => new Character('\n')
    case s => new Character(s.charAt(2))
  }

  private def makePair(xs: List[Object], tail: Object = null) = {
    @tailrec
    def loop(xs: List[AnyRef], acc: Object): Object = xs match {
      case Nil => acc
      case x::xs => loop(xs, S.Cons(x, acc))
    }
    loop(xs.reverse, tail)
  }

  private def listParser: Parser[Object] = "(" ~> rep(expression) <~ ")" ^^ (S.List(_:_*))

  private def listWithDotParser: Parser[Object] = "(" ~ rep(expression) ~ "." ~ expression ~ ")" ^^ {
    case (_ ~ expressions ~ _ ~ tail ~ _) => makePair(expressions, tail)
  }

  def expression: Parser[Object] = trueParser | falseParser | voidParser | eofParser | characterParser |
    symbolParser | doubleParser | integerParser | stringParser | minusSymbol  |
    quoteParser | unquoteSplicingParser | unquoteParser | quasiquoteParser | listParser | listWithDotParser

  def expressions: Parser[List[Object]] = rep(expression)
}
