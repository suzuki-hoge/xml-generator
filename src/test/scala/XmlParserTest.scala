import XmlParser.key
import org.scalatest.FunSuite

import scala.util.parsing.combinator.JavaTokenParsers

class XmlParserTest extends FunSuite with JavaTokenParsers {
  test("key") {
    assert(
      XmlParser(key, "<key>status</key>") == Key("status")
    )
  }
}
