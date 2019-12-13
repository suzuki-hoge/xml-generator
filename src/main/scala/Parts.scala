case class Cond(key: Key, method: Method)

case class Test(cond1: Cond, cond2: Cond, operator: Operator)

trait Operator

object And extends Operator

object Or extends Operator

case class Key(v: String)

trait Method

case class Equals(value: Value) extends Method

case class Contains(value: Value) extends Method

case class Value(v: String)
