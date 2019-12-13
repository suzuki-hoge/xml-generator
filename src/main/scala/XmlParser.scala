import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  def key: Parser[String] = "<key>" ~> identifier <~ "</key>" ^^ (v => v)

  private def identifier: Parser[String] = "[a-z_]+".r

  def apply[T](parser: Parser[T], s: String): T = parse(parser, s).get
}
