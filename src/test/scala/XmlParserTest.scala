import ImplicitConverters._
import XmlParser.cond
import org.scalatest.FunSuite

import scala.util.parsing.combinator.JavaTokenParsers

class XmlParserTest extends FunSuite with JavaTokenParsers {
  test("cond") {
    val s =
      """<cond>
        |    <key>status</key>
        |    <val>%equal(ng)</val>
        |</cond>""".stripMargin

    val exp = "status" $equals "ng"

    assert(XmlParser(cond, s) == exp)
  }
}
