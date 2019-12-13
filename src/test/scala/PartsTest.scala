import org.scalatest.FunSuite

class PartsTest extends FunSuite {
  test("cond") {
    val cond1 = Cond(Key("status"), Equals(Value("ng")))
    val cond2 = Cond(Key("reason"), Contains(Value("system")))

    val test1 = cond1.and(cond2)

    assert(test1.toString == "AND(status equals ng, reason contains system)")

    val cond3 = Cond(Key("status"), Equals(Value("ok")))

//    val test2 = test1.or(cond3)
  }
}
