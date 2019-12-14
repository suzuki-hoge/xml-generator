import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  def statement: Parser[Statement] = condRule ~ actionRule ^^ (x => Statement(x._1, x._2._1, x._2._2))

  def condRule: Parser[Testable] = "<cond_rule>" ~ "<conds>" ~> testable <~ "<true action=\"true_action\" />" ~ "<false action=\"false_action\" />" ~ "</conds>" ~ "</cond_rule>"

  def actionRule: Parser[(Seq[Action], Seq[Action])] = "<action_rule>" ~> thenAction ~ elseAction <~ "</action_rule>" ^^ (x => (x._1, x._2))

  def thenAction: Parser[Seq[Action]] = "<actions action=\"true_action\">" ~> rep1(action) <~ "</actions>"

  def elseAction: Parser[Seq[Action]] = "<actions action=\"false_action\">" ~> rep1(action) <~ "</actions>"

  def testable: Parser[Testable] = test | cond

  def test: Parser[Test] = "<test type=\"" ~> operator ~ "\">" ~ testable ~ testable <~ "</test>" ^^ { case o ~ _ ~ t1 ~ t2 => Test(t1, t2, o) }

  private def operator: Parser[Operator] = "AND" ^^ (_ => And) | "OR" ^^ (_ => Or)

  def cond: Parser[Cond] = "<cond>" ~> key ~ method <~ "</cond>" ^^ (x => Cond(x._1, x._2))

  def key: Parser[Key] = "<key>" ~> identifier <~ "</key>" ^^ Key

  def method: Parser[Method] = "<val>" ~> ($equals | $contains) <~ "</val>"

  private def $equals: Parser[Equals] = "%equal(" ~> value <~ ")" ^^ Equals

  private def $contains: Parser[Contains] = "%contains(" ~> value <~ ")" ^^ Contains

  private def value: Parser[Value] = identifier ^^ Value

  def action: Parser[Action] = assignment

  def assignment: Parser[Assignment] = "<action>" ~> varName ~ varValue <~ "</action>" ^^ (x => Assignment(x._1, x._2))

  def varName: Parser[VarName] = "<key>" ~> identifier <~ "</key>" ^^ VarName

  def varValue: Parser[VarValue] = "<val>" ~> identifier <~ "</val>" ^^ VarValue

  private def identifier: Parser[String] = "[a-z_]+".r

  def apply[T](parser: Parser[T], s: String): T = parse(parser, s).get
}
