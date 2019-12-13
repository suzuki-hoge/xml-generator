trait Testable

case class Cond(key: Key, method: Method) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toString: String = s"${key.v} $method"
}

case class Test(testable1: Testable, testable2: Testable, operator: Operator) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toString: String = s"$operator($testable1, $testable2)"
}

trait Operator

object And extends Operator {
  override def toString: String = "AND"
}

object Or extends Operator {
  override def toString: String = "OR"
}

case class Key(v: String) {
  def $equals(value: Value): Cond = Cond(this, Equals(value))

  def $contains(value: Value): Cond = Cond(this, Contains(value))
}

trait Method

case class Equals(value: Value) extends Method {
  override def toString: String = s"equals ${value.v}"
}

case class Contains(value: Value) extends Method {
  override def toString: String = s"contains ${value.v}"
}

case class Value(v: String)
