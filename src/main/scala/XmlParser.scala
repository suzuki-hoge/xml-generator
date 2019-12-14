import scala.util.parsing.combinator.JavaTokenParsers

object XmlParser extends JavaTokenParsers {
  private def statement: Parser[Statement] = {
    def condRule: Parser[Testable] = "<cond_rule>" ~ "<conds>" ~> testable <~ "<true action=\"true_action\" />" ~ "<false action=\"false_action\" />" ~ "</conds>" ~ "</cond_rule>"

    def thenAction: Parser[Seq[Action]] = "<actions action=\"true_action\">" ~> rep1(action) <~ "</actions>"

    def elseAction: Parser[Seq[Action]] = "<actions action=\"false_action\">" ~> rep1(action) <~ "</actions>"

    condRule ~ "<action_rule>" ~ thenAction ~ elseAction ~ "</action_rule>" ^^ { case t ~ _ ~ tA ~ eA ~ _ => Statement(t, tA, eA) }
  }

  private def testable: Parser[Testable] = {
    def test: Parser[Test] = {
      def operator: Parser[Operator] = "AND" ^^ (_ => And) | "OR" ^^ (_ => Or)

      "<test type=\"" ~> operator ~ "\">" ~ testable ~ testable <~ "</test>" ^^ { case o ~ _ ~ t1 ~ t2 => Test(t1, t2, o) }
    }

    def cond: Parser[Cond] = {
      def key: Parser[Key] = "<key>" ~> identifier <~ "</key>" ^^ Key

      def method: Parser[Method] = {
        def $equals: Parser[Equals] = "%equal(" ~> value <~ ")" ^^ Equals

        def $contains: Parser[Contains] = "%contains(" ~> value <~ ")" ^^ Contains

        def value: Parser[Value] = identifier ^^ Value

        "<val>" ~> ($equals | $contains) <~ "</val>"
      }

      "<cond>" ~> key ~ method <~ "</cond>" ^^ { case k ~ v => Cond(k, v) }
    }

    test | cond
  }

  private def action: Parser[Action] = {
    def assignment: Parser[Assignment] = {
      def varName: Parser[VarName] = "<key>" ~> identifier <~ "</key>" ^^ VarName

      def varValue: Parser[VarValue] = "<val>" ~> identifier <~ "</val>" ^^ VarValue

      "<action>" ~> varName ~ varValue <~ "</action>" ^^ { case n ~ v => Assignment(n, v) }
    }

    assignment
  }

  private def identifier: Parser[String] = "[a-z_]+".r

  def apply(s: String): Statement = parse(statement, s).get
}
