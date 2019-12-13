import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  def key: Parser[String] = "<key>" ~ "[a-z]+".r ~ "</key>" ^^ { case _ ~ v ~ _ => v }

  def apply[T](parser: Parser[T], s: String): T = parse(parser, s).get
}
