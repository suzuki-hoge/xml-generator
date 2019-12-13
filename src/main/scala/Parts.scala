trait Testable {
  def toXml(): String
}

case class Cond(key: Key, method: Method) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toXml(): String =
    s"""<cond>
       |${key.toXml()}
       |${method.toXml()}
       |</cond>""".stripMargin
}

case class Test(testable1: Testable, testable2: Testable, operator: Operator) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toXml(): String =
    s"""<test type="$operator">
       |${testable1.toXml()}
       |${testable2.toXml()}
       |</test>""".stripMargin
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

  def toXml(): String = s"<key>$v</key>"
}

trait Method {
  def toXml(): String
}

case class Equals(value: Value) extends Method {
  override def toXml(): String = s"<val>%equal(${value.v})</val>"
}

case class Contains(value: Value) extends Method {
  override def toXml(): String = s"<val>%contains(${value.v})</val>"
}

case class Value(v: String)

trait Action {
  def toXml():String
}

case class Assignment(name: VarName, value: VarValue) extends Action {
  override def toXml(): String =
    s"""<action>
       |${name.toXml()}
       |${value.toXml()}
       |</action>""".stripMargin
}

case class VarName(v: String) {
  def is(value: VarValue): Assignment = Assignment(this, value)

  def toXml(): String = s"<key>$v</key>"
}

case class VarValue(v: String) {
  def toXml(): String = s"<val>$v</val>"
}

case class If(testable: Testable) {
  def _then(actions: Action*): Then = Then(testable, actions)
}

case class Then(testable: Testable, thenActions: Seq[Action]) {
  def _else(actions: Action*): Statement = Statement(testable, thenActions, actions)
}

case class Statement(testable: Testable, thenActions: Seq[Action], elseActions: Seq[Action]) {
  def toXml: String =
    s"""<cond_rule>
       |<conds>
       |${testable.toXml()}
       |<true action="true_action" />
       |<false action="false_action" />
       |</conds>
       |</cond_rule>
       |
       |<action_rule>
       |<actions action="true_action">
       |${thenActions.map(_.toXml()).mkString("\n")}
       |</actions>
       |<actions action="false_action">
       |${elseActions.map(_.toXml()).mkString("\n")}
       |</actions>
       |</action_rule>""".stripMargin
}
