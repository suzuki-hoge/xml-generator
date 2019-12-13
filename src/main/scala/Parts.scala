case class Cond(key: Key, method: Method) {
  def and(other: Cond): Test = Test(this, other, And)

  def or(other: Cond): Test = Test(this, other, Or)

  override def toString: String = s"${key.v} $method"
}

case class Test(cond1: Cond, cond2: Cond, operator: Operator) {
  override def toString: String = s"$operator($cond1, $cond2)"
}

trait Operator

object And extends Operator {
  override def toString: String = "AND"
}

object Or extends Operator {
  override def toString: String = "OR"
}

case class Key(v: String)

trait Method

case class Equals(value: Value) extends Method {
  override def toString: String = s"equals ${value.v}"
}

case class Contains(value: Value) extends Method {
  override def toString: String = s"contains ${value.v}"
}

case class Value(v: String)
