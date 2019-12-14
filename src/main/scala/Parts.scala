trait Testable {
  def toXml(indent: Indent): String

  def toScala: String
}

case class Cond(key: Key, method: Method) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toXml(indent: Indent): String =
    s"""$indent<cond>
       |${key.toXml(indent + 1)}
       |${method.toXml(indent + 1)}
       |$indent</cond>""".stripMargin

  override def toScala: String = s"(${key.toScala} ${method.toScala})"
}

case class Test(testable1: Testable, testable2: Testable, operator: Operator) extends Testable {
  def &&(other: Testable): Test = Test(this, other, And)

  def ||(other: Testable): Test = Test(this, other, Or)

  override def toXml(indent: Indent): String =
    s"""$indent<test type="${operator.toXml}">
       |${testable1.toXml(indent + 1)}
       |${testable2.toXml(indent + 1)}
       |$indent</test>""".stripMargin

  override def toScala: String = s"${testable1.toScala} ${operator.toScala} ${testable2.toScala}"
}

trait Operator {
  def toXml: String

  def toScala: String
}

object And extends Operator {
  override def toXml: String = "AND"

  override def toScala: String = "&&"
}

object Or extends Operator {
  override def toXml: String = "OR"

  override def toScala: String = "||"
}

case class Key(v: String) {
  def $equals(value: Value): Cond = Cond(this, Equals(value))

  def $contains(value: Value): Cond = Cond(this, Contains(value))

  def toXml(indent: Indent): String = s"$indent<key>$v</key>"

  def toScala: String = s""""$v""""
}

trait Method {
  def toXml(indent: Indent): String

  def toScala: String
}

case class Equals(value: Value) extends Method {
  override def toXml(indent: Indent): String = s"$indent<val>%equal(${value.v})</val>"

  override def toScala: String = s"""$$equals "${value.v}""""
}

case class Contains(value: Value) extends Method {
  override def toXml(indent: Indent): String = s"$indent<val>%contains(${value.v})</val>"

  override def toScala: String = s"""$$contains "${value.v}""""
}

case class Value(v: String)

trait Action {
  def toXml(indent: Indent): String

  def toScala: String
}

case class Assignment(name: VarName, value: VarValue) extends Action {
  override def toXml(indent: Indent): String =
    s"""$indent<action>
       |${name.toXml(indent + 1)}
       |${value.toXml(indent + 1)}
       |$indent</action>""".stripMargin

  override def toScala: String = s""""${name.v}" is "${value.v}""""
}

case class VarName(v: String) {
  def is(value: VarValue): Assignment = Assignment(this, value)

  def toXml(indent: Indent): String = s"$indent<key>$v</key>"
}

case class VarValue(v: String) {
  def toXml(indent: Indent): String = s"$indent<val>$v</val>"
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
       |  <conds>
       |${testable.toXml(Indent(2))}
       |    <true action="true_action" />
       |    <false action="false_action" />
       |  </conds>
       |</cond_rule>
       |
       |<action_rule>
       |  <actions action="true_action">
       |${thenActions.map(_.toXml(Indent(2))).mkString("\n")}
       |  </actions>
       |  <actions action="false_action">
       |${elseActions.map(_.toXml(Indent(2))).mkString("\n")}
       |  </actions>
       |</action_rule>""".stripMargin

  def toScala: String =
    s"""import ImplicitConverters._
       |
       |If(${testable.toScala})
       |  ._then(${thenActions.map(_.toScala).mkString(", ")})
       |  ._else(${elseActions.map(_.toScala).mkString(", ")})""".stripMargin
}

case class Indent(v: Int) {
  def +(n: Int): Indent = Indent(v + n)

  override def toString: String = "  " * v
}