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

trait Action

case class Assignment(name: VarName, value: VarValue) extends Action {
  override def toString: String = s"${name.v} = ${value.v}"
}

case class VarName(v: String) {
  def is(value: VarValue): Assignment = Assignment(this, value)
}

case class VarValue(v: String)

case class If(testable: Testable) {
  def _then(actions: Action*): Then = Then(testable, actions)
}

case class Then(testable: Testable, thenActions: Seq[Action]) {
  def _else(actions: Action*): Statement = Statement(testable, thenActions, actions)
}

case class Statement(testable: Testable, thenActions: Seq[Action], elseActions: Seq[Action]) {
  override def toString: String = s"$testable then (${thenActions.mkString(", " )}) else (${elseActions.mkString(", ")})"
}
