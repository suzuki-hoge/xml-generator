import ImplicitConverters._
import org.scalatest.FunSuite

class PartsTest extends FunSuite {
  test("statement") {
    val statement =
      If(("status" $equals "ng") && ("reason" $contains "system") || ("status" $equals "ok"))
        ._then("check" is "pass", "limit" is "today")
        ._else("check" is "fail")

    val exp =
      """<cond_rule>
        |  <conds>
        |    <test type="OR">
        |      <test type="AND">
        |        <cond>
        |          <key>status</key>
        |          <val>%equal(ng)</val>
        |        </cond>
        |        <cond>
        |          <key>reason</key>
        |          <val>%contains(system)</val>
        |        </cond>
        |      </test>
        |      <cond>
        |        <key>status</key>
        |        <val>%equal(ok)</val>
        |      </cond>
        |    </test>
        |    <true action="true_action" />
        |    <false action="false_action" />
        |  </conds>
        |</cond_rule>
        |
        |<action_rule>
        |  <actions action="true_action">
        |    <action>
        |      <key>check</key>
        |      <val>pass</val>
        |    </action>
        |    <action>
        |      <key>limit</key>
        |      <val>today</val>
        |    </action>
        |  </actions>
        |  <actions action="false_action">
        |    <action>
        |      <key>check</key>
        |      <val>fail</val>
        |    </action>
        |  </actions>
        |</action_rule>""".stripMargin

    assert(statement.toXml == exp)
  }
}
