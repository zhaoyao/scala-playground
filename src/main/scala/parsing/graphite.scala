package parsing

import scala.util.parsing.combinator.{PackratParsers, ImplicitConversions}
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader._
import scala.Some
import scala.util.parsing.combinator.token.{StdTokens, Tokens}

trait GraphiteTokens extends StdTokens {

  case class Operator(chars: String) extends Token {
    override def toString = "operator " + chars
  }

  case class MetricKey(chars: String) extends Token {
    override def toString = "metric " + chars
  }

}

class Lexer extends StdLexical with GraphiteTokens with ImplicitConversions {

  override def token: Parser[Token] =
  //( '\"' ~ rep(charSeq | letter) ~ '\"' ^^ lift(StringLit)
    (string ^^ StringLit
      | number ~ letter ^^ { case n ~ l => ErrorToken("Invalid number format : " + n + l)}
      | '-' ~> whitespace ~ number ~ letter ^^ { case ws ~ num ~ l => ErrorToken("Invalid number format : -" + num + l)}
      | '-' ~> whitespace ~ number ^^ { case ws ~ num => NumericLit("-" + num)}
      | number ^^ NumericLit
      | operator ^^ Operator
      | rep1(identChar) ^^ { case xs => Identifier(xs.mkString("")) }
//      | repsep(rep1(validMetricChar), dot) ^^ { case xs => MetricKey(xs.map(_.mkString("")).mkString("."))}
      | EofCh ^^^ EOF
      | delim
      | '\"' ~> failure("Unterminated string")
      | failure("Illegal character")
      )

  def checkKeyword(xs: List[Any]) = {
    val strRep = xs mkString ""
    if (reserved contains strRep) Keyword(strRep) else ErrorToken("Not a keyword: " + strRep)
  }

  def dot = elem('.')

  def operators = Set[Char]() ++ "()[]{},.=".toCharArray

  def operator = elem("operator", operators.contains) ^^ { case x => x.toString}

  /** A string is a collection of zero or more Unicode characters, wrapped in
    * double quotes, using backslash escapes (cf. http://www.json.org/).
    */
  def string = '\"' ~> rep(charSeq) <~ '\"' ^^ {
    _ mkString ""
  }

  override def whitespace = rep(whitespaceChar)

  def number = intPart ~ opt(fracPart) ^^ { case i ~ f => i + optString(".", f)}

  def intPart = zero | intList

  def intList = nonzero ~ rep(digit) ^^ { case x ~ y => (x :: y) mkString ""}

  def fracPart = '.' ~> rep(digit) ^^ {
    _ mkString ""
  }

  private def optString[A](pre: String, a: Option[A]) = a match {
    case Some(x) => pre + x.toString
    case None => ""
  }

  def zero: Parser[String] = '0' ^^^ "0"

  def nonzero = elem("nonzero digit", d => d.isDigit && d != '0')

  def exponent = elem("exponent character", d => d == 'e' || d == 'E')

  def sign = elem("sign character", d => d == '-' || d == '+')

  def printables = Set[Char]() ++ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~".toArray
  def charSeq = elem("char", printables.contains)

  def identChars = Set[Char]() ++ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_*".toArray
  override def identChar = elem("identChar", identChars.contains)

  val validMetricChars = Set[Char]() ++ "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#$%&*+-/:;<>?@[]^_`|~]".toArray

  def validMetricChar = elem("validMetricChars", validMetricChars.contains)

  val hexDigits = Set[Char]() ++ "0123456789abcdefABCDEF".toArray

  def hexDigit = elem("hex digit", hexDigits.contains)
}

class Parser extends StdTokenParsers with GraphiteTokens with ImplicitConversions with PackratParsers {

  type Tokens = GraphiteTokens
  val lexical = new Lexer

  lexical.delimiters ++= Seq("(", ")", ",", "=")

  /** Type signature for functions that can parse numeric literals */
  type NumericParser = String => Any

  // Global default number parsing function
  protected var defaultNumberParser: NumericParser = {
    _.toDouble
  }

  // Per-thread default number parsing function
  protected val numberParser = new ThreadLocal[NumericParser]() {
    override def initialValue() = defaultNumberParser
  }

  //  def arg = bool | num | stringLiteral

  //  def kwarg = keyword ~ Equal ~ arg

  //  def args = repsep(kwarg | arg, Comma)

  //  def kwargs = repsep(kwarg, Comma)

  //  def call = keyword ~ LeftParen ~ (args ~ (Comma ~ kwargs ?) ?) ~ RightParen

  def call: Parser[Any] = log {
    ident ~ op("(") ~ opt(args ~ opt(op(",") ~> kwargs)) ~ op(")") ^^ {
      case func ~ _ ~ Some(args ~ Some(kwargs)) ~ _ => (func, args, Map(kwargs: _*))
          case func ~ _ ~ Some(args ~ None) ~ _ => (func, args, Map())
      case func ~ _ ~ None ~ _ => (func, List(), Map())
    }
  }("call")

//  def op(op: String) = accept("operator", { case x if x == op => x})

  def metricPathElement = log {ident <~ not(op("="))}("metricPathElement")
  def metricPathEnum = log { "{" ~> rep1sep(metricPathElement, op(",")) <~ "}" }("metricPathEnum")

  override def not[T](p: => Parser[T]): Parser[Unit] = log {super.not(p)}("not")

  def metricPath = log { rep1sep(metricPathElement|metricPathEnum, op(".")) ^^ { case xs => xs.mkString(".") } }("metricPath")

  def args = log {rep1sep(expression|metricPath, op(","))}("args")

  //todo 如何解析kwargs? 无论进步进行metricPath的抽象
  //如果不抽象metric path, 那么就必须将metric path解析为ident, 这样expression中就必须包含ident规则，这样x=1的x就会被优先解析为 ident
  //如果抽象出metric path, 可以将ident从expression中摘掉，同样地问题是x=1的x会被解析成MetricPath("x")
  //所以问题出现在ident的定义？ 或者说metric path的定义？
  //根本问题出在哪里呢？
  //问题出在 metric path element的定义，去除歧义就能正确解析了
  def kwargs = log {rep1sep(kwarg, op(","))}("kwargs")
  def kwarg =
    log {ident ~ op("=") ~ expression ^^ { case k ~ _ ~ value => (k, value)}}("kwarg")

  def stringVal = log {
    accept("string", { case lexical.StringLit(n) => n})
  }("string")

  def number = log {
    accept("number", { case lexical.NumericLit(n) => numberParser.get.apply(n)})
  }("number")

  def op(op: String) = log {
    accept("operator", { case lexical.Operator(x) if x == op => x})
  }("op " + op)

  def expression = log {
    "true" ^^^ true | "false" ^^^ false | number | stringVal | call
  }("expression")


  override def ident = log { super.ident }("ident")
}

object GraphiteTarget extends Parser {

  def parseFull(input: String) =
    phrase(expression)(tokenize(input)) match {
      case Success(result, _) => Some(result)
      case x => x
    }

  def tokenize(input: String) = new lexical.Scanner(input)

  /**
   * 将 a.{b,c}.e.{f,g} 展开成 [a,b,e,f] [a,b,e,g] [a,c,e,f] [a,c,e,g]
   * @return
   */
  def flattenMetricEnum = {
    Nil
  }

}

object Lexer extends App{
//  private var scanner: Lexer#Scanner = GraphiteTarget.tokenize("aliasByNode(gmon.apps.athena.components.*.total_hit , -2, x=1)")
//
//  print("(")
//  while (!scanner.atEnd) {
//    print("[")
//    print(scanner.first)
//    print("]")
//    scanner = scanner.rest
//    print(", ")
//  }
//  println(")")
  println(GraphiteTarget.parseFull("aliasByNode(gmon.apps.athena.components.*.{total_hit,total_miss}, -2, x=1, y = 2)"))
//  println('*'.isLetter)
}