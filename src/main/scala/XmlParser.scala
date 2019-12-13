import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  def key: Parser[Key] = "<key>" ~> identifier <~ "</key>" ^^ Key

  private def identifier: Parser[String] = "[a-z_]+".r

  def apply[T](parser: Parser[T], s: String): T = parse(parser, s).get
}
