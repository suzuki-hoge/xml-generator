import ImplicitConverters._
import Statement.If
import org.scalatest.FunSuite

class PartsTest extends FunSuite {
  test("statement") {
    val statement1 = If(("status" $equals "ng") && ("reason" $contains "system"))
      ._then("check" is "pass", "limit" is "today")
      ._else("check" is "fail")

    assert(statement1.toString == "AND(status equals ng, reason contains system) then (check = pass, limit = today) else (check = fail)")
  }
}
