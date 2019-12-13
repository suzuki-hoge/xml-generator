import ImplicitConverters._
import org.scalatest.FunSuite

class PartsTest extends FunSuite {
  test("cond") {
    val cond1 = "status" $equals "ng"
    val cond2 = "reason" $contains "system"

    val test1 = cond1 && cond2

    assert(test1.toString == "AND(status equals ng, reason contains system)")

    val cond3 = "status" $equals "ok"

    val test2 = test1 || cond3

    assert(test2.toString == "OR(AND(status equals ng, reason contains system), status equals ok)")

    val test3 = cond3 || test1

    assert(test3.toString == "OR(status equals ok, AND(status equals ng, reason contains system))")

    val test4 = test1 || test1

    assert(test4.toString == "OR(AND(status equals ng, reason contains system), AND(status equals ng, reason contains system))")
  }

  test("assignment") {
    val action1 = "check" is "pass"

    assert(action1.toString == "check = pass")
  }
}
