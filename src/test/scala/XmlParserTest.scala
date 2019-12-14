import ImplicitConverters._
import XmlParser.{assignment, testable}
import org.scalatest.FunSuite

import scala.util.parsing.combinator.JavaTokenParsers

class XmlParserTest extends FunSuite with JavaTokenParsers {
  test("testable") {
    val s =
      """<test type="OR">
        |    <test type="AND">
        |        <cond>
        |            <key>status</key>
        |            <val>%equal(ng)</val>
        |        </cond>
        |        <cond>
        |            <key>reason</key>
        |            <val>%contains(system)</val>
        |        </cond>
        |    </test>
        |    <cond>
        |        <key>status</key>
        |        <val>%equal(ok)</val>
        |    </cond>
        |</test>""".stripMargin

    val exp = ("status" $equals "ng") && ("reason" $contains "system") || ("status" $equals "ok")

    assert(XmlParser(testable, s) == exp)
  }

  test("assignment") {
    val s =
      """<action>
        |    <key>check</key>
        |    <val>pass</val>
        |</action>""".stripMargin

    val exp = "check" is "pass"

    assert(XmlParser(assignment, s) == exp)
  }
}
