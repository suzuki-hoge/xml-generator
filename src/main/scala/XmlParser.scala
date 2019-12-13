import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  def cond: Parser[Cond] = "<cond>" ~> key ~ method <~ "</cond>" ^^ (x => Cond(x._1, x._2))

  def key: Parser[Key] = "<key>" ~> identifier <~ "</key>" ^^ Key

  def method: Parser[Method] = "<val>" ~> ($equals | $contains) <~ "</val>"

  private def $equals: Parser[Equals] = "%equal(" ~> value <~ ")" ^^ Equals

  private def $contains: Parser[Contains] = "%contains(" ~> value <~ ")" ^^ Contains

  private def value: Parser[Value] = identifier ^^ Value

  private def identifier: Parser[String] = "[a-z_]+".r

  def apply[T](parser: Parser[T], s: String): T = parse(parser, s).get
}
